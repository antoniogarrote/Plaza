-module(plaza_web) .

-author("Antonio Garrrote Hernandez") .

-include_lib("states.hrl").
-include_lib("definitions.hrl").
-include_lib("http_records.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([lifting/6, operation/6, lowering/6]) .
-export([process_path_pattern/1, resolve_uri/6, parse_accept_header/1, build_default_sparql_query/6, query_sparql_repository/6]) .
-export([state_update/3, state_get/2, state_new/0, triple_spaces_for_request/6]) .


%% public API


%% @doc
%% Executes a list of Functions over the request/response parameters and returns the
%% result of the last successful execution or error.
combine(Method, Request, Response, State, Application, Resource, Functions) ->
    do_combine([Method, Request, Response, State, Application, Resource], Functions) .


state_update(Key, Value, State) ->
    dict:store(Key, Value, State)  .


state_get(Key, State) ->
    dict:fetch(Key, State) .


state_new() ->
    dict:new() .


%% @doc
%% Default mechanism for transforming a HTTP request into aerror
%% set of triples.
lifting('GET', Request, Response, State, Application, Resource) ->
    combine('GET', Request, Response, State, Application, Resource,
            [ fun resolve_uri/6,
              fun triple_spaces_for_request/6
              fun build_default_sparql_query/6 ]) ;

lifting('POST', Request, Response, State, Application, Resource) ->
    case Resource:is_metaresource() of
        true ->
            combine('POST', Request, Response, State, Application, Resource,
                    [ fun resolve_uri/6,
                      fun new_rdf_instance_from_request/6 ]) ;
        false ->
            {error, ["Impossible to process POST request for non metaresource", Resource:uri(), Request#request.parameters]}
    end .


%% @doc
%% Default empty GET operation. It just passes the triples to the lowering
%% operation
operation('GET', Request, Response, State, Application, Resource) ->
    combine('GET', Request, Response, State, Application, Resource,
            [ fun query_sparql_repository/6,
              fun(M,Req,Rsp,St,App,Res) -> {ok, [M, Req, Rsp#response{ code = 200 }, St, App, Res]} end ]) ;

operation('POST', Request, Response, State, Application, Resource) ->
    combine('POST', Request, Response, State, Application, Resource,
            [ fun triple_spaces_for_request/6,
              fun write_triple_space/6 ]) .


%% @doc
%% Default empty GET operation. It just passes the triples to the lowering
%% operation
lowering('GET', Request, Response, State, Application, Resource) ->
    default_format_response('GET', Request, Response, State, Application, Resource) ;

lowering('POST', Request, Response, State, Application, Resource) ->
    default_format_response('POST', Request, Response, State, Application, Resource) .


%% Functions


%% @doc
%% Resolves the variables in a URI pattern into a
%% URI using another URI as the provider of data.
%% @provides
%%  - route
resolve_uri(Method, Request, Response, State, Application, Resource) ->
    Params = Request#request.parameters,
    Uri    = Resource:uri(),
    {Scheme, RestUri} = plaza_utils:strip_protocol_domain(Uri),
    UriParts = process_path_pattern(RestUri),
    case resolve_uri(Params, UriParts, []) of
        error -> error ;
        ResolvedRestUri -> UriP = Scheme ++ lists:foldl(fun(P,Ac) -> Ac ++ "/" ++ P end, "", ResolvedRestUri),
                           {ok, [Method, Request, Response, state_update(route, UriP, State), Application, Resource]}
    end .

%% Retrieves a SPARQL query from the State and queries the Resource triple space
%% using that query. Stores the resulting set of triple spaces in the state.
%% @requires
%%   -sparql_query
%% @provides
%%   -rdf_triples
query_sparql_repository(Method, Request, Response, State, Application, Resource) ->
    Query = state_get(sparql_query, State),
    Triples = plaza_repository:sparql_query(Application, Query),
    Set = plaza_triples:set(lists:map(fun([{s,S},{p,P},{o,O}]) -> plaza_triples:t(S,P,O,Resource:triple_space()) end,
                                      Triples)),
    {ok, [Method, Request, Response, state_update(rdf_triples, Set, State), Application, Resource]} .


%% Builds a default SPARQL query for a resource
%% that retrieves all the triples for that resource
%% or limits the query based on query parameters
%% and a given vocabulary.
%% @requires
%%  - triple_spaces
%%  - route
%% @provides
%%  - sparql_query
build_default_sparql_query(Method, Request, Response, State, Application, Resource) ->
    Uri = state_get(route, State),
    Dataset = Resource:triple_space(),
    Parameters = Request#request.parameters,
    Vocabulary = Application#plaza_app.vocabulary,
    Contexts = state_get(triple_spaces, State),

    DatasetClause = lists:foldl(fun(C,A) -> A ++ C end, "", [" FROM <" ++ Ctx ++">" || Ctx <- Contexts]),
    Query = default_sparql_query(Uri, Dataset, Parameters, Vocabulary, "SELECT ?s ?p ?o" ++ DatasetClause ++ " WHERE { ", true, 0),
    {ok, [Method, Request, Response, state_update(sparql_query, Query,State), Application, Resource]} .


%% @doc
%% Transforms the params of the HTTP request and a provided generated URI into
%% a set of RDF triples describing the new resource.
%% @requires
%%  - route
%% @provides
%%  - rdf_triples
new_rdf_instance_from_request(Method, Request, Response, State, Application, Resource) ->
    ResolvedUri = state_get(route,State),
    ResourceUri = Resource:generate_instance_uri(Request, Response, State, Application#plaza_app.vocabulary),
    Graph = rdf_graph_from_request_params(ResourceUri, ResolvedUri, Request, Application#plaza_app.vocabulary),
    {ok, [Method, Request, Response, state_update(rdf_triples, Graph, State), Application, Resource]} .


%% @doc
%% Fixes the triple spaces for a resource and resquest.
%% @requires
%%   - route
%% @provides
%%   - triple_spaces
triple_spaces_for_request(Method, Request, Response, State, Application, Resource) ->
    Contexts = case Resource:is_metaresource() of
                   true ->  case Method of
                                'POST' -> [state_get(route,State), Resource:uri()]
                            end ;
                   false -> case Method of
                                'GET'  -> MetaResource = Resource:metaresource(),
                                          triple_spaces_for_resource(state_get(route,State), MetaResource)
                            end
               end,
    {ok, [Method, Request, Response, state_update(triple_spaces, Contexts, State), Application, Resource]} .


%% @doc
%% Inserts a set of processed RDF triples read from the State into
%% the resources Triple Space.
%% @requires
%%  -rdf_triples
%%  -route
%%  -triple_spaces
%% @updates
%%  -Response#response.code
write_triple_space(Method, Request, Response, State, Application, Resource) ->
    Triples = state_get(rdf_triples, State),
    ParsedTriples = plaza_formaters:format(xml, Application#plaza_app.vocabulary, Application#plaza_app.namespaces, Triples),
    Contexts = state_get(triple_spaces, State),
    error_logger:info_msg("ENCODED TRIPLES:~n~p~n",[ParsedTriples]),
    error_logger:info_msg("CONTEXTS:~n~p~n",[Contexts]),
    case plaza_repository:add_encoded_triples(Application, Resource:uri(), ParsedTriples, Contexts, ?RDF_XML) of
        ok     -> {ok, [Method, Request, Response#response{ code = 201 }, State, Application, Resource]} ;
        _Other -> {error, ["Impossible to create resource in repository", Resource:uri(), Request#request.parameters]}
    end .


%% @doc
%% Retrieves the processed RDF triples for the resource from the State and lowers its representation
%% with the right formatter for the value of the Content-Type HTTP header.
%% @requires
%%   - rdf_triples
%% @updates
%%   - headers
%%   - body
default_format_response(Method, Request, Response, State, Application, Resource) ->
    Triples = state_get(rdf_triples, State),
    ResponseP = Response#response{body = Triples},
    Formats = parse_accept_header(Request),
    {{Type, Subtype},Body} = plaza_formaters:format(Formats, ResponseP, Application#plaza_app.vocabulary, Application#plaza_app.namespaces, Resource),
    Headers = Response#response.headers,
    %% @todo check if they are already present
    UpdatedHeaders = [{"Content-Type", Type++"/"++Subtype},{"Charset", "utf-8"} | Headers],
    {ok, [Method, Request, Response#response{ headers = UpdatedHeaders, body = Body }, State, Application, Resource]} .


%% private functions



default_sparql_query(Uri, Dataset, [], _Vocabulary, Query, _First, Num) ->
    case Num =:= 0 of
        true  -> "SELECT ?s ?p ?o FROM <" ++ Dataset ++ "> WHERE { ?s ?p ?o FILTER( ?s = <" ++ Uri ++ "> )}" ;
        false -> Query ++ "FILTER( ?s = <" ++ Uri ++ ">) }"
    end ;

default_sparql_query(Uri, Dataset, [{Key, Value} | Ps], Vocabulary, Query, First, Num) ->
    case plaza_vocabulary:resolve(Vocabulary, Key) of
        {ok, Prop } ->     QueryP = case First of
                                        true    ->  Query ++ " ?s " ++ Prop ++ " \"" ++ Value ++ "\""  ;
                                        false   ->  Query ++ " . ?s " ++ Prop ++ " \"" ++ Value ++ "\""
                                    end,
                           default_sparql_query(Uri, Dataset, Ps, Vocabulary, QueryP,false, Num+1) ;
        error       ->     default_sparql_query(Uri, Dataset, Ps, Vocabulary, Query, First, Num)
    end .


resolve_uri(_Params, [], Acum) ->
    lists:reverse(Acum) ;
resolve_uri(Params, [P | Ps], Acum) when is_atom(P) ->
    case proplists:get_value(P, Params) of
        undefined    -> error ;
        Value        -> resolve_uri(Params, Ps, [Value | Acum])
    end ;
resolve_uri(Params, [P | Ps], Acum) ->
    resolve_uri(Params, Ps, [P | Acum]) .


do_combine(Params, []) -> {ok, Params} ;
do_combine(Params, [F | Fs]) ->
    case apply(F,Params) of
        {ok, ParamsP}   -> do_combine(ParamsP, Fs) ;
        {error, Reason} -> {error, Reason}
    end .

%% do_combine(Params, [F | Fs]) ->
%%     try apply(F,Params) of
%%         {ok, ParamsP}   -> do_combine(ParamsP, Fs) ;
%%         {error, Reason} -> {error, Reason}
%%     catch
%%         Class:Reason    -> {error, [Class, Reason]}
%%     end .

%% @doc
%% Process a string with a request pattern, transforming it into a
%% list of pattern tokens.
process_path_pattern(Pattern) ->
    Elements = plaza_utils:split(Pattern, "/"),
    HasDot = lists:member($.,lists:nth(1,lists:reverse(Elements))),
    ElementsP = if
                    HasDot =:= true ->
                        [ToSplit | Rest] = lists:reverse(Elements),
                        Splitted = plaza_utils:split(ToSplit, "\\."),
                        lists:reverse(Rest) ++ Splitted ;
                    true            ->
                        Elements
                end,
    lists:map( fun(Elem) ->
                       HasColon = (lists:nth(1,Elem) =:= $:),
                       if
                           HasColon =:= true ->
                               {_Colon, Val} = lists:split(1,Elem),
                               list_to_atom(Val) ;
                           true ->
                               Elem
                       end
               end,
               ElementsP) .


parse_accept_header(Request) ->
    %% @todo this should not be mochiweb dependent
    case plaza_mochiweb_adapter:header(Request, 'Accept') of
        undefined  ->  [{"application", "xml"}] ; %% default case
        String     ->  parse_formats(String)
    end .


%% @doc
%% Transforms a Accept HTTP header string into
%% a list of tuples {"type","subtype"}
parse_formats(AcceptHeader) ->
    Hs = plaza_utils:split(AcceptHeader, ","),
    Ls = lists:map(fun(H) ->
                           Tmp = plaza_utils:split(H,";"),
                           case Tmp of
                               [L, _Q] -> plaza_utils:split(L,"/") ;
                               [L]     -> plaza_utils:split(L,"/")
                           end
                   end,
                   Hs),
    lists:map(fun([F,S]) -> {F,S} end, Ls) .


rdf_graph_from_request_params(Subject, Dataset, Request,Vocabulary) ->
    Params = Request#request.parameters,
    do_rdf_graph_params(Subject, Dataset, Params, Vocabulary, plaza_triples:set()) .

do_rdf_graph_params(_Subject, _Dataset, [], _Vocabulary, Set) ->
    Set ;
do_rdf_graph_params(Subject, Dataset, [{P,V}| Params], Vocabulary, Set) ->
    case plaza_vocabulary:resolve(Vocabulary, P) of
        error     -> do_rdf_graph_params(Subject, Dataset, Params, Vocabulary, Set) ;
        Predicate -> do_rdf_graph_params(Subject, Dataset, Params, Vocabulary, plaza_triples:set_add(Subject, Predicate, plaza_triples:l(V,undefined,undefined), Dataset, Set))
    end .


%% @doc
%% Retrieves a set of URIs for the the resources' tree of
%% a certain resource.
%% The Uri of the resource and the associated meta resource
%% must be provided as arguments.
triple_spaces_for_resource(Uri, MetaResource) ->
    [Uri [ MetaResource:uri() | [ R:uri() || R <- MetaResource:resources_tree()]] .


%% @doc
%% Computes the triple spaces for a meta resource based in its
%% associated resource's tree.
triple_spaces_for_metaresource(MetaResource) ->
    [ MetaResource:uri() | [ R:uri() || R <- MetaResource:resources_tree()] .


%% Tests


parse_formats_test() ->
    Str = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    Ls = parse_formats(Str),
    ?assertEqual([{"text","html"}, {"application","xhtml+xml"}, {"application", "xml"}, {"*","*"}], Ls) .


process_path_pattern_test() ->
    PatternA = process_path_pattern("/this/is/a/test"),
    ?assertEqual(["this","is","a","test"], PatternA),
    PatternB = process_path_pattern("/this/is/a/*"),
    ?assertEqual(["this","is","a","*"], PatternB),
    PatternC = process_path_pattern("/this/:verb/:article/*"),
    ?assertEqual(["this",verb,article,"*"], PatternC),
    PatternD = process_path_pattern("/this/:verb/:article/*/or.something"),
    ?assertEqual(["this",verb,article,"*","or","something"], PatternD) .


resolve_uri_test() ->
    Res = case resolve_uri([{value, "45"}, {other, "test"}], process_path_pattern("/this/value/:value/txt"), []) of
              error           -> error ;
              ResolvedRestUri -> "http://test.com" ++ lists:foldl(fun(P,Ac) -> Ac ++ "/" ++ P end, "", ResolvedRestUri)
          end,
    ?assertEqual("http://test.com/this/value/45/txt", Res) .
