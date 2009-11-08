-module(plaza_web) .

-author("Antonio Garrrote Hernandez") .

-include_lib("states.hrl").
-include_lib("definitions.hrl").
-include_lib("http_records.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([lifting/6, operation/6, lowering/6]) .
-export([process_path_pattern/1, resolve_uri/6, parse_accept_header/1, build_default_sparql_query/6, query_sparql_repository/6]) .
-export([state_update/3, state_get/2, state_new/0, triple_spaces_for_request/6]) .
-export([pattern_to_delete_in_update/6, triples_to_insert_in_update/6, update_triple_space/6]) .


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
    error_logger:info_msg("Lifiting resource GET ~p",[Resource]),
    combine('GET', Request, Response, State, Application, Resource,
            [ fun resolve_uri/6,
              fun triple_spaces_for_request/6,
              fun build_default_sparql_query/6 ]) ;

lifting('POST', Request, Response, State, Application, Resource) ->
    case Resource:is_metaresource() of
        true ->
            combine('POST', Request, Response, State, Application, Resource,
                    [ fun resolve_uri/6,
                      fun new_rdf_instance_from_request/6 ]) ;
        false ->
            {error, ["Impossible to process POST request for non metaresource", Resource:uri(), Request#request.parameters]}
    end ;
lifting('PUT', Request, Response, State, Application, Resource) ->
    case Resource:is_metaresource() of
        true ->
            {error, ["Impossible to process PUT request for a metaresource", Resource:uri(), Request#request.parameters]} ;
        false ->
            combine('PUT', Request, Response, State, Application, Resource,
                    [ fun resolve_uri/6,
                      fun triple_spaces_for_request/6,
                      fun pattern_to_delete_in_update/6,
                      fun triples_to_insert_in_update/6 ])
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
              fun write_triple_space/6 ]) ;

operation('PUT', Request, Response, State, Application, Resource) ->
    combine('PUT', Request, Response, State, Application, Resource,
            [ fun update_triple_space/6 ]) .


%% @doc
%% Default empty GET operation. It just passes the triples to the lowering
%% operation
lowering('GET', Request, Response, State, Application, Resource) ->
    default_format_response('GET', Request, Response, State, Application, Resource) ;

lowering('POST', Request, Response, State, Application, Resource) ->
    default_format_response('POST', Request, Response, State, Application, Resource) ;

lowering('PUT', Request, Response, State, Application, Resource) ->
    default_format_response('PUT', Request, Response, State, Application, Resource) .

%% Functions


%% @doc
%% Resolves the variables in a URI pattern into a
%% URI using another URI as the provider of data.
%% @provides
%%  - route
resolve_uri(Method, Request, Response, State, Application, Resource) ->
    Params = Request#request.parameters,
    %Uri    = Resource:uri(),
    Path    = case Resource:is_metaresource() of
                  true  -> plaza_ts_trees:metaresource_path_uri(Resource:url_token(),
                                                                plaza_ts_trees:make((Application#plaza_app.application_module):write_tree())) ;
                  false -> plaza_ts_trees:resource_path_uri(Resource:url_token(),
                                                            plaza_ts_trees:make((Application#plaza_app.application_module):write_tree()))
              end,
    Uri = "http://" ++ (Application#plaza_app.application_module):domain() ++ Path,
    error_logger:info_msg("Resolving URI: ~p with pattern ~p",[Uri, Params]),
    {Scheme, RestUri} = plaza_utils:strip_protocol_domain(Uri),
    UriParts = process_path_pattern(RestUri),
    case resolve_uri(Params, UriParts, []) of
        error -> {error, ["Impossible to resolve URI and params", Params, UriParts]} ;
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
    Parameters = Request#request.parameters,
    Vocabulary = Application#plaza_app.vocabulary,
    Namespaces = Application#plaza_app.namespaces,
    Contexts = state_get(triple_spaces, State),
    error_logger:info_msg("Inserting datasets clause in sparql query with value ~p",[Contexts]),
    DatasetClause = lists:foldl(fun(C,A) -> A ++ C end, "", [" FROM <" ++ Ctx ++">" || Ctx <- Contexts]),
    error_logger:info_msg("Inserting datasets clause in sparql query with actual value ~p",[DatasetClause]),
    Query = case Resource:is_metaresource() of
                true  -> default_metaresource_sparql_query(Uri, DatasetClause, Parameters, Vocabulary, Namespaces, "SELECT ?s ?p ?o" ++ DatasetClause ++ " WHERE { ", true, 0) ;
                false -> default_sparql_query(Uri, DatasetClause, Parameters, Vocabulary, Namespaces, "SELECT ?s ?p ?o" ++ DatasetClause ++ " WHERE { ", true, 0)
            end,
    {ok, [Method, Request, Response, state_update(sparql_query, Query,State), Application, Resource]} .


%% @doc
%% Transforms the params of the HTTP request and a provided generated URI into
%% a set of RDF triples describing the new resource.
%% @requires
%%  - route
%% @provides
%%  - rdf_triples
%%  - new_resource_uri
new_rdf_instance_from_request(Method, Request, Response, State, Application, Resource) ->
    ResolvedUri = state_get(route,State),
    ResourceUri = Resource:generate_instance_uri(Request, Response, State, Application#plaza_app.vocabulary),
    GraphPre = rdf_graph_from_request_params(ResourceUri, ResolvedUri, Request, Application#plaza_app.vocabulary, Application#plaza_app.namespaces),
    Graph = case Resource:is_metaresource() of
                true  -> plaza_triples:set_add(ResourceUri, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">>, plaza_utils:to_binary(Resource:uri()), GraphPre) ;
                false -> GraphPre
            end,
    StateP = state_update(new_resource_uri, ResourceUri, State),
    {ok, [Method, Request, Response, state_update(rdf_triples, Graph, StateP), Application, Resource]} .


%% @doc
%% Transforms the params of the HTTP request and a provided generated URI into
%% a set of RDF triples that should be deleted as part of the update operation.
%% @requires
%%  - route
%% @provides
%%  - pattern_to_delete_in_update
pattern_to_delete_in_update(Method, Request, Response, State, Application, Resource) ->
    ResolvedUri = state_get(route,State),
    Parameters = Request#request.parameters,
    {Construct, Where} = build_query_update_delete_query(ResolvedUri, Parameters, Application, {"",""}, true, 0),
    Query = "CONSTRUCT {"++ Construct ++ "} FROM <" ++ ResolvedUri ++ "> WHERE{ " ++ Where ++"}",

    StateP = state_update(pattern_to_delete_in_update, Query, State),
    {ok, [Method, Request, Response, StateP, Application, Resource]} .


%% @doc
%% Sends an update request to the RDF repository.
%% @requires
%%  - route
%%  - pattern_to_delete_in_update
%%  - rdf_triples
%%  - triples_spaces
update_triple_space(Method, Request, Response, State, Application, Resource) ->
    BaseUrl = state_get(route, State),
    QueryDelete = state_get(pattern_to_delete_in_update, State),
    TriplesUpdate = state_get(rdf_triples, State),
    ContextsUpdate = state_get(triple_spaces, State),
    ContextsDelete = [BaseUrl],
    ParsedTriples = plaza_formaters:format(xml, Application#plaza_app.vocabulary, Application#plaza_app.namespaces, TriplesUpdate),
    error_logger:info_msg("DELETE QUERY:~n~p~n",[QueryDelete]),
    error_logger:info_msg("CONTEXTS DELETE:~n~p~n",[ContextsDelete]),
    error_logger:info_msg("ENCODED TRIPLES:~n~p~n",[ParsedTriples]),
    error_logger:info_msg("CONTEXTS UPDATE:~n~p~n",[ContextsUpdate]),

    case plaza_repository:update_graph(Application, BaseUrl, QueryDelete, ContextsDelete, ParsedTriples, ContextsUpdate, ?RDF_XML) of
        ok     -> {ok, [Method, Request, Response#response{ code = 200 }, State, Application, Resource]} ;
        Other -> {error, ["Impossible to update resource in repository", Other, BaseUrl, Request#request.parameters]}
    end .


%% @doc
%% Transforms the params of the HTTP request and a provided generated URI into
%% a set of RDF triples that should be deleted as part of the update operation.
%% @requires
%%  - route
%% @provides
%%  - rdf_triples
triples_to_insert_in_update(Method, Request, Response, State, Application, Resource) ->
    Uri = state_get(route, State),
    Parameters = lists:filter(fun({_K,V}) -> V =/=  "_delete" end, Request#request.parameters),
    Graph = do_rdf_graph_params(Uri, [Uri], Parameters, Application#plaza_app.vocabulary, Application#plaza_app.namespaces, plaza_triples:set()),
    {ok, [Method, Request, Response, state_update(rdf_triples, Graph, State), Application, Resource]} .


%% @doc
%% Fixes the triple spaces for a resource and resquest.
%% @requires
%%   - route
%%   - new_resource_uri (if POST)
%% @provides
%%   - triple_spaces
triple_spaces_for_request(Method, Request, Response, State, Application, Resource) ->
    Contexts = case Resource:is_metaresource() of
                   true ->  case Method of
                                'POST' -> [binary_to_list(state_get(new_resource_uri,State)) |
                                           lists:map(fun(R) -> "http://" ++ (Application#plaza_app.application_module):domain() ++ R end,
                                                     write_tree_for_resource(state_get(route,State), Resource, Application)) ] ;
                                'GET'  -> [ state_get(route, State) |
                                            lists:map(fun(R) -> "http://" ++ (Application#plaza_app.application_module):domain() ++ R end,
                                                      read_tree_for_resource(state_get(route,State), Resource)) ]
                            end ;
                   false -> case Method of
                                'GET'  -> MetaResource = Resource:metaresource(),
                                          [ state_get(route, State) |
                                            lists:map(fun(R) -> "http://" ++ (Application#plaza_app.application_module):domain() ++ R end,
                                                      read_tree_for_resource(state_get(route,State), MetaResource)) ] ;
                                'PUT'  -> MetaResource = Resource:metaresource(),
                                          lists:map(fun(R) -> "http://" ++ (Application#plaza_app.application_module):domain() ++ R end,
                                                    write_tree_for_resource(state_get(route,State), MetaResource, Application))
                            end
               end,
    error_logger:info_msg("Contexts for resource ~p :~n ~p ~n",[state_get(route,State), Contexts]),
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




build_query_update_delete_query(_Subject, [], _Application, Query, _First, _Num) ->
    Query ;
build_query_update_delete_query(Subject, [{Key, Value} | Ps], Application, {Construct, Where}, First, Num) ->
    error_logger:info_msg("QUERY DELETE ~p WITH  ~p args",[Key, length([{Key, Value} | Ps])]),
    Vocabulary = Application#plaza_app.vocabulary,
    Namespaces = Application#plaza_app.namespaces,

    case resolve_term_or_ns(Key, Vocabulary, Namespaces) of
        error ->     build_query_update_delete_query(Subject, Ps, Application, {Construct, Where}, First, Num) ;
        Prop  ->     [Next] = io_lib:format("~p",[Num]),
                     ValueP = "?o" ++ Next,
                     NumP = Num + 1,
                     PropL = binary_to_list(Prop),
                     ConstructP = case First of
                                      true    ->  Construct ++ " <" ++ Subject ++ "> <" ++ PropL ++ "> " ++ ValueP  ;
                                      false   ->  Construct ++ " . <" ++ Subject ++ "> <" ++ PropL ++ "> " ++ ValueP
                                  end,
                     WhereP = case First of
                                  true  -> Where ++ " <" ++ Subject ++ "> <" ++ PropL ++ "> " ++ ValueP ;
                                  false -> Where ++ " . <" ++ Subject ++ "> <" ++ PropL ++ "> " ++ ValueP
                              end,
                     error_logger:info_msg("RECURING WITH: ~p ~p ~p ~p + args",[Subject, {ConstructP, WhereP}, false, NumP]),
                     build_query_update_delete_query(Subject, Ps, Application, {ConstructP, WhereP}, false, NumP)
    end .


resolve_term_or_ns(Term,Vocabulary,Namespaces) ->
    case is_atom(Term) of
        true  -> plaza_vocabulary:resolve(Vocabulary, Term) ;
        false -> case lists:member($:,Term) of
                     true  -> [Ns, Local] = plaza_utils:split(Term, ":"),
                              list_to_binary([plaza_namespaces:resolve_prefix(Namespaces, plaza_utils:to_atom(Ns)),Local]) ;
                     false -> plaza_vocabulary:resolve(Vocabulary, Term)
                 end
    end .


extract_triple_spaces_from_uri(Path, WriteTree) ->
    RequestPathTokens = plaza_web:process_path_pattern(Path),
    error_logger:info_msg("Path tokens ~p", [RequestPathTokens]),

    Tree = plaza_ts_trees:make(WriteTree),
    error_logger:info_msg("Nodes tokens ~p", [plaza_ts_trees:nodes(Tree)]),
    ResourceTokens = skip_prefix_from_request(RequestPathTokens, plaza_ts_trees:nodes(Tree)),
    error_logger:info_msg("Resource tokens ~p", [ResourceTokens]),
    lists:nthtail(1,lists:reverse(lists:foldl(fun(E,[P | Ps]) -> [ P ++ "/" ++ E , P | Ps] end, [[]], ResourceTokens))) .



skip_prefix_from_request([], _Nodes) -> [] ;
skip_prefix_from_request([T | Ts], Nodes) ->
    case lists:any(fun(E) -> E =:= plaza_utils:to_atom(T) end, Nodes) of
        true  ->  [T | Ts] ;
        false -> skip_prefix_from_request(Ts, Nodes)
    end .


default_sparql_query(Uri, Dataset, [], _Vocabulary, _Namespaces, Query, _First, Num) ->
    case Num =:= 0 of
        true  -> "SELECT ?s ?p ?o " ++ Dataset ++ " WHERE { ?s ?p ?o FILTER( ?s = <" ++ Uri ++ "> )}" ;
        false -> Query ++ "FILTER( ?s = <" ++ Uri ++ ">) }"
    end ;
default_sparql_query(Uri, Dataset, [{Key, Value} | Ps], Vocabulary, Namespaces, Query, First, Num) ->
    case resolve_term_or_ns(Key, Vocabulary, Namespaces) of
        error  ->     default_sparql_query(Uri, Dataset, Ps, Vocabulary, Namespaces, Query, First, Num) ;
        Prop   ->     QueryP = case First of
                                   true    ->  Query ++ " ?s " ++ binary_to_list(Prop) ++ " \"" ++ Value ++ "\""  ;
                                   false   ->  Query ++ " . ?s " ++ binary_to_list(Prop) ++ " \"" ++ Value ++ "\""
                               end,
                      default_sparql_query(Uri, Dataset, Ps, Vocabulary, Namespaces, QueryP,false, Num+1)
    end .


default_metaresource_sparql_query(Uri, Dataset, [], _Vocabulary, _Namespaces, Query, _First, Num) ->
    case Num =:= 0 of
        true  -> "SELECT ?s ?p ?o " ++ Dataset ++ " WHERE { ?s ?p ?o. ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <" ++ Uri ++ "> }" ;
        false -> Query ++  " ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <" ++ Uri ++ "> }"
    end ;
default_metaresource_sparql_query(Uri, Dataset, [{Key, Value} | Ps], Vocabulary, Namespaces, Query, First, Num) ->
    case resolve_term_or_ns(Key, Vocabulary, Namespaces) of
        error ->     default_metaresource_sparql_query(Uri, Dataset, Ps, Vocabulary, Namespaces, Query, First, Num) ;
        Prop  ->     QueryP = case First of
                                  true    ->  Query ++ " ?s " ++ binary_to_list(Prop) ++ " \"" ++ Value ++ "\""  ;
                                  false   ->  Query ++ " . ?s " ++ binary_to_list(Prop) ++ " \"" ++ Value ++ "\""
                              end,
                     default_metaresource_sparql_query(Uri, Dataset, Ps, Vocabulary, Namespaces, QueryP,false, Num+1)
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
        {error, Reason} -> {error, Reason} ;
        Other           -> error_logger:error_msg("Unknown response combining function: ~p, ~p, ~p",[F, Params, Other]),
                           {error, ["Unkown response", Other]}

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
    HasDot = case Elements of
                 []     -> false ;
                 _Other -> lists:member($.,lists:nth(1,lists:reverse(Elements)))
             end,
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


rdf_graph_from_request_params(Subject, Dataset, Request, Vocabulary, Namespaces) ->
    Params = Request#request.parameters,
    do_rdf_graph_params(Subject, Dataset, Params, Vocabulary, Namespaces, plaza_triples:set()) .

do_rdf_graph_params(_Subject, _Dataset, [], _Vocabulary, _Namespaces, Set) ->
    Set ;
do_rdf_graph_params(Subject, Dataset, [{P,V}| Params], Vocabulary, Namespaces, Set) ->
    case resolve_term_or_ns(P, Vocabulary, Namespaces) of
        error     -> do_rdf_graph_params(Subject, Dataset, Params, Vocabulary, Namespaces, Set) ;
        Predicate -> do_rdf_graph_params(Subject, Dataset, Params, Vocabulary, Namespaces,
                                         plaza_triples:set_add(Subject, Predicate, plaza_triples:l(V,undefined,undefined), Dataset, Set))
    end .


%% @doc
%% Retrieves a set of URIs for the the resources' tree of
%% a certain resource.
%% The Uri of the resource and the associated meta resource
%% must be provided as arguments.
read_tree_for_resource(_Uri, MetaResource) ->
    Tree = plaza_ts_trees:make(MetaResource:read_tree()),
    Nodes = plaza_ts_trees:nodes(Tree),
    [ R:uri() || R <- Nodes] .


%% @doc
%% Computes the triple spaces for a meta resource based in its
%% associated resource's tree.
read_tree_for_metaresource(MetaResource) ->
    Tree = plaza_ts_trees:make(MetaResource:read_tree()),
    Nodes = plaza_ts_trees:nodes(Tree),
    [ R:uri() || R <- Nodes].


write_tree_for_resource(Uri, _MetaResource, Application) ->
    extract_triple_spaces_from_uri(Uri, (Application#plaza_app.application_module):write_tree()) .
%%     Tree = plaza_ts_trees:make((Application#plaza_app.application_module):write_tree()),
%%     Path = plaza_ts_trees:path(MetaResource, Tree),
%%     [Uri | Path] .

write_tree_for_metaresource(Uri, _MetaResource, Application) ->
    extract_triple_spaces_from_uri(Uri, (Application#plaza_app.application_module):write_tree()) .
%%     Tree = plaza_ts_trees:make((Application#plaza_app.application_module):write_tree()),
%%     plaza_ts_trees:path(MetaResource, Tree) .


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


resolve_term_or_ns_test() ->
    Vocabulary = plaza_vocabulary:make([{test, <<"http://test.com/test">>}]),
    Ns = plaza_namespaces:make([{dc, <<"http://purl.dc.org#">>},
                                {test, <<"http://test.com#">>}]),
    ?assertEqual(<<"http://test.com#something">>,
                 resolve_term_or_ns("test:something", Vocabulary, Ns)),
    ?assertEqual(<<"http://test.com/test">>,
                 resolve_term_or_ns(test,Vocabulary,Ns)).
