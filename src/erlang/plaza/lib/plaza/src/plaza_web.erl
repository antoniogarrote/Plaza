-module(plaza_web) .

-author("Antonio Garrrote Hernandez") .

-include_lib("states.hrl").
-include_lib("definitions.hrl").
-include_lib("http_records.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([lifting/6, operation/6, lowering/6]) .
-export([process_path_pattern/1, resolve_uri/2, parse_accept_header/1]) .


%% public API

%% compose(Request,Response,State,[]) ->
%%     {ok, Response} ;

%% compose(Request,Response,State,[F | Fs]) ->
%%     try
%%         case F(Request, Response, State) of
%%             {ok, ResponseP} ->
%%                 compose(Request, ResponseP,


%% @doc
%% Default mechanism for transforming a HTTP request into a
%% set of triples.
lifting('GET', Request, Response, State, Application, Resource) ->
    % First, we resolve the URI of the resource
    case resolve_uri(Request#request.parameters, Resource:uri()) of
        error       -> {error, ["Impossible to resolve URI", Resource:uri(), Request#request.parameters]} ;
        ResolvedUri -> case default_sparql_query(ResolvedUri, Resource:triple_space(), Request#request.parameters, Application) of
                           error             -> {error, ["Error building default SPARQL query", Resource:uri(), ResolvedUri, Request#request.parameters]} ;
                           {ok, SparqlQuery} -> Triples = plaza_repository:sparql_query(Application, SparqlQuery),
                                                Set = plaza_repository:set(lists:map(fun([{s,S},{p,P},{o,O}]) -> plaza_triples:t(S,P,O,Resource:triple_space()) end,
                                                                                     Triples)),
                                                {ok, Response#response{body = Set}, State}
                       end
    end ;

lifting('POST', Request, Response, State, Application, Resource) ->
    case Resource:is_metaresource() of
        true ->  % First, we resolve the URI of the resource
            case resolve_uri(Request#request.parameters, Resource:uri()) of
                error       -> {error, ["Impossible to resolve URI", Resource:uri(), Request#request.parameters]} ;
                ResolvedUri -> ResourceUri = Resource:generate_instance_uri(Request, Response, State, Application#plaza_app.vocabulary),
                               Graph = rdf_graph_from_request_params(ResourceUri, ResolvedUri, Request, Application#plaza_app.vocabulary),
                               {ok, Response#response{body = Graph}, State}
            end ;
        false -> {error, ["Impossible to process POST request for non metaresource", Resource:uri(), Request#request.parameters]}
    end .


%% @doc
%% Default empty GET operation. It just passes the triples to the lowering
%% operation
operation('GET', _Request, Response, State, _Application, _Resource) ->
    {ok, Response#response{ code = 200 }, State} ;

operation('POST', Request, Response, State, Application, Resource) ->
    Triples = Response#response.body,
    ParsedTriples = plaza_formaters:rdf_formatter(xml,Triples),
    case plaza_repository:add_encoded_triples(Application, Resource:uri(), ParsedTriples,?RDF_XML) of
        ok     -> {ok, Response#response{ code = 201 }, State} ;
        _Other -> {error, ["Impossible to create resource in repository", Resource:uri(), Request#request.parameters]}
    end .



%% @doc
%% Default empty GET operation. It just passes the triples to the lowering
%% operation
lowering('GET', Request, Response, State, Application, Resource) ->
    Formats = parse_accept_header(Request),
    {{Type, Subtype},Body} = plaza_formaters:format(Formats, Response, Application, Resource),
    Headers = Response#response.headers,
    %% @todo check if they are already present
    UpdatedHeaders = [{"Content-Type",Type++"/"++Subtype++"; charset=utf-8"}| Headers],
    {ok, Response#response{ headers = UpdatedHeaders, body = Body }, State} ;

lowering('POST', Request, Response, State, Application, Resource) ->
    Formats = parse_accept_header(Request),
    {{Type, Subtype},Body} = plaza_formaters:format(Formats, Response, Application, Resource),
    Headers = Response#response.headers,
    %% @todo check if they are already present
    UpdatedHeaders = [{"Content-Type",Type++"/"++Subtype++"; charset=utf-8"}| Headers],
    {ok, Response#response{ headers = UpdatedHeaders, body = Body }, State} .


%% @doc
%% Resolves the variables in a URI pattern into a
%% URI using another URI as the provider of data.
resolve_uri(Params, Uri) ->
    {Scheme, RestUri} = plaza_utils:strip_protocol_domain(Uri),
    UriParts = process_path_pattern(RestUri),
    case resolve_uri(Params, UriParts, []) of
        error -> error ;
        ResolvedRestUri -> Scheme ++ lists:foldl(fun(P,Ac) -> Ac ++ "/" ++ P end, "", ResolvedRestUri)
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


%% Builds a default SPARQL query for a resource
%% that retrieves all the triples for that resource
%% or limits the query based on query parameters
%% and a given vocabulary.
default_sparql_query(Uri, Dataset, Parameters, Application) ->
    default_sparql_query(Uri, Dataset, Parameters, Application#plaza_app.vocabulary, "SELECT ?s ?p ?o FROM <" ++ Dataset ++ "> WHERE { ", true, 0) .

default_sparql_query(Uri, Dataset, [], _Vocabulary, Query, _First, Num) ->
    case Num =:= 0 of
        true  -> {ok, "SELECT ?s ?p ?o FROM <" ++ Dataset ++ "> WHERE { ?s ?p ?o FILTER ?s = <" ++ Uri ++ "> }"} ;
        false -> {ok, Query ++ "FILTER ?s = <" ++ Uri ++ "> }"}
    end ;

default_sparql_query(Uri, Dataset, [{Key, Value} | Ps], Vocabulary, Query, First, Num) ->
    case plaza_vocabulary:resolve(Vocabulary, Key) of
        {ok, Prop } -> QueryP = case First of
                                    true    ->  Query ++ " ?s " ++ Prop ++ " \"" ++ Value ++ "\""  ;
                                    false   ->  Query ++ " . ?s " ++ Prop ++ " \"" ++ Value ++ "\""
                                end,
                       default_sparql_query(Uri, Dataset, Ps, Vocabulary, QueryP, Num+1, false) ;
        error        -> default_sparql_query(Uri, Dataset, Ps, Vocabulary, Query, Num, First)
    end .


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
                    true ->
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
    case vocabulary:resolve(Vocabulary, P) of
        error     -> do_rdf_graph_params(Subject, Dataset, Params, Vocabulary, Set) ;
        Predicate -> do_rdf_graph_params(Subject, Dataset, Params, Vocabulary, plaza_triples:set_add(Subject, Predicate, plaza_triples:l(V,undefined,undefined), Dataset, Set))
    end .


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
    ?assertEqual("http://test.com/this/value/45/txt", resolve_uri([{value, "45"}, {other, "test"}], "http://test.com/this/value/:value/txt")) .
