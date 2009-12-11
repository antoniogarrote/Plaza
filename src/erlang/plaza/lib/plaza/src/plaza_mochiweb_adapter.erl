%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(plaza_mochiweb_adapter) .

-author("Antonio Garrote Hernandez") .

-behaviour(gen_server) .

-include_lib("eunit/include/eunit.hrl").
-include_lib("http_records.hrl").
-include_lib("definitions.hrl").

-export([start_link/2, update_routes/3, loop/3, handle_request/3, header/2]) .
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% Public API


start_link(Name, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name,Options}, []) .


update_routes(Name, Routes, Application) ->
    gen_server:call(Name, {update_routes, Routes, Application}) .


%% Callbacks


init({Name,Options}) ->
    {ok, CurrentDir} = file:get_cwd(),
    error_logger:info_msg("Starting mochiweb adapter with DOCROOT ~p and options ~p", [CurrentDir ++ "/www", Options]),
    Loop = fun (Req) ->
		   ?MODULE:loop(Name,Req, CurrentDir ++ "/www")
	   end,
    Params = [{name, ?MODULE}, {loop, Loop} | Options],
    mochiweb_http:start(Params),
    {ok, []}.


handle_call({update_routes, NewRoutes, Application}, _From, Routes) ->
    UpdatedRoutes = lists:map(fun({Pattern, Action}) ->
                                      { plaza_web:process_path_pattern(Pattern),
                                        Action,
                                        Application }
                              end,
                              NewRoutes),
    {reply, ok, Routes ++ UpdatedRoutes} .


handle_cast({handle_request, Request, DocRoot}, Routes) ->
    spawn(?MODULE,
          handle_request,
          [Request, Routes, DocRoot]),
    {noreply, Routes} .


%% dummy callbacks so no warning are shown at compile time
handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State} .


terminate(shutdown, _State) ->
    ok.



%% Private functions


%% @doc
%% Handles a mochiweb incoming request
handle_request(Request, Routes, DocRoot) ->
    error_logger:info_msg("received web request: ~p", [Request]),
    PlazaRequest = make_request(Request),
    error_logger:info_msg("routing: ~p", [Request]),
    Response = route(PlazaRequest, Routes, DocRoot),
    case Response of
        {push, ResponseP, RemainingStages} -> do_push_response(ResponseP, Request, RemainingStages) ;
        _RegularResponse                   -> do_response(Response,Request)
    end.


%% @doc
%% Check if a path and a patter match, updating the Request
%% object accordingly
match_path([], [], Request) ->
    {ok, Request} ;
match_path(_Path, ["*"], Request) ->
    {ok, Request} ;
match_path(_Pattern, [], _Request) ->
    error ;
match_path([], _Pattern, _Request) ->
    error ;
match_path(Pattern, F, Request) when is_function(F) ->
    case F(Pattern,Request) of
        {ok, Result} -> {ok, Result} ;
        _Other       -> error
    end ;
match_path([Pattern|Patterns], [Path|Paths], #request{ parameters = Params} = Request) when is_atom(Pattern)  ->
    match_path(Patterns, Paths, Request#request{ parameters = [{Pattern,Path} | Params]}) ;
match_path([Pattern|Patterns], [Path|Paths], Request) ->
    case Pattern =:= Path of
        true  -> match_path(Patterns, Paths, Request) ;
        false -> error
    end .


%% @doc
%% Provided a set of routes and a plaza request,
%% return {ok, action()} if any route matches or
%% error in any other case.
route(Request, Routes, DocRoot) ->
    RequestPathTokens = plaza_web:process_path_pattern(Request#request.path),
    error_logger:info_msg("routing: ~p in ~p", [RequestPathTokens, lists:map(fun({R,P,_A}) -> {R,P} end, Routes)]),
    State = plaza_web:state_new(),
    case do_route(RequestPathTokens, Routes, Request) of
        {ok, {resource,R}, RequestP, ApplicationP} -> error_logger:info_msg("routing success, routing to resource: ~p", [R]),
                                                      process_resource(R, RequestP, #response{}, State, ApplicationP) ;
        {ok, {M,F}, RequestP, ApplicationP}        -> error_logger:info_msg("routing success, routing to: ~p", [{M,F}]),
                                                      M:F(RequestP, #response{}, State, ApplicationP) ;
        {ok, F, RequestP, ApplicationP}            -> error_logger:info_msg("routing success, routing to: ~p", [F]),
                                                      F(RequestP, #response{}, State, ApplicationP) ;
        {error, _RequestP}                         -> error_logger:info_msg("routing error", []),
                                                      check_static_file(DocRoot, Request#request.path)
    end.

check_static_file(DocRoot, Path) ->
    File = case Path == [] of
               true  -> DocRoot ++ Path ++ "/index.html" ;
               false -> case lists:nth(1,lists:reverse(Path)) == $/ of
                            true  -> DocRoot ++ Path ++ "index.html" ;
                            false -> DocRoot ++ Path
                        end
           end ,
    error_logger:info_msg("Trying to serve static file ~p",[File]),
    case file:read_file_info(File) of
        {ok, _Info}  -> {static, DocRoot, Path} ;
        _Error       -> #response{ code = 404,
                                   headers = [{"Content-Type", "text/plain"},
                                              {"Charset", "utf-8"}],
                                   body = "\"Unknown resource\"" }
    end .



%% @doc
%% Check if special parameters are passed in the request.
%% Supported parameters are:
%% - xblocking  -> blocking request
%% - xsubscribe -> subscribe to a query
parse_special_parameters(Request, State) ->
    do_parse_special_parameters(?SPECIAL_PARAMETERS, Request, State) .
do_parse_special_parameters([], Request, State) ->
    {Request, State} ;
do_parse_special_parameters([{P,SP}|Ps], Request, State) ->
    error_logger:info_msg("Parsing special parameter ~p in ~p",[P, Request#request.parameters]),
    case proplists:get_value(P, Request#request.parameters) of
        undefined  ->  error_logger:info_msg("not found",[]),
                       do_parse_special_parameters(Ps, Request, plaza_web:state_update(SP, false, State)) ;
        Value     ->  error_logger:info_msg("ok",[]),
                      ParamsP = proplists:delete(P, Request#request.parameters),
                      error_logger:info_msg("new params ~p",[ParamsP]),
                      do_parse_special_parameters(Ps, Request#request{ parameters = ParamsP }, plaza_web:state_update(SP, Value, State))
    end .


%% @doc
%% Process the routing of a request to a resource through the
%% the cycle of lifting, operation and lowering.
process_resource(Resource, Request, Response, State, Application) ->
    {RequestP,StateP} = parse_special_parameters(Request, State),
    do_processing_resource([lifting, operation, lowering], Resource, RequestP, Response, StateP, Application) .


%% @doc
%% Converts the http Method in a capitalized symbol
%% if necessary.
method_to_atom(Method) when is_list(Method) ->
    list_to_atom(string:to_upper(Method)) ;
method_to_atom(Method) -> Method .


do_processing_resource([], _Resource, _Request, Response, _State, _Application) ->
    Response ;
do_processing_resource([S | Stages], Resource, Request, Response, State, Application) ->
    Method = method_to_atom(Request#request.method),
    error_logger:info_msg("Processing resource: ~p, ~p, ~p",[Resource, S, Method]),
    case apply(Resource, S, [Method, Request, Response, State, Application]) of
        {ok, [_M, _R, ResponseP, StateP, _A, _Res]} -> do_processing_resource(Stages, Resource, Request, ResponseP, StateP, Application) ;
        {error, Reason}                             -> error_logger:info_msg("Error processing resource", [Resource, Reason]),
                                                       default_handle_error(Reason) ;
        %% TODO here handling blocking operations
        {push,{ok, [_M, _R, ResponseP, StateP, _A, _Res]}} -> {push, do_processing_resource(Stages, Resource, Request, ResponseP, StateP, Application), Stages} ;
        {push, {error, Reason}}                            -> error_logger:info_msg("Error processing resource", [Resource, Reason]),
                                                              default_handle_error(Reason)
    end .


default_handle_error(Reason) ->
    #response{ code = 500,
               headers = [{"Content-Type", "text/plain"},
                          {"Charset", "UTF-8"}],
               body = lists:flatten(io_lib:format("~p",[Reason])) } .


do_route(Path, [], _Request) ->
    error_logger:info_msg("Named route not found: ~p",[Path]),
    {error, route_not_found} ;
do_route(Path, [{Pattern, Action, Application} | Routes], Request) ->
    error_logger:info_msg("Routing try:~p <-> ~p",[Pattern, Path]),
    case match_path(Pattern, Path, Request) of
        {ok, RequestP}  -> error_logger:info_msg("Routing ok:~p <-> ~p",[Pattern, Path]),
                           {ok, Action, RequestP, Application} ;
        error           -> error_logger:info_msg("Routing error:~p <-> ~p",[Pattern, Path]),
                           do_route(Path, Routes, Request) ;
        Other           -> error_logger:info_msg("Routing WTF!!???:~p <-> ~p = ~p",[Pattern, Path, Other]),
                           {ok, Action, Request, Application}
    end .


%% route_tokens(Path, Tokens, Request) ->
%%     error_logger:info_msg("Routing with tokens ~p", [Tokens]),


loop(Name, Req, DocRoot) ->
    gen_server:cast(Name, {handle_request, Req, DocRoot}) .


%% @doc
%% Creates a new request from the mochiweb equivalent
make_request(Req) ->
    error_logger:info_msg("making request method: ~p", [Req:get(method)]),
    error_logger:info_msg("making request path: ~p", [Req:get(path)]),
    error_logger:info_msg("making request headers: ~p", [Req:get(headers)]),
    error_logger:info_msg("making request parameters: ~p", [Req:parse_qs() ++ Req:parse_post()]),
    #request{ method = Req:get(method),
              path = Req:get(path),
              headers = Req:get(headers),
              raw = Req,
              parameters = Req:parse_qs() ++ Req:parse_post() } .


%% @doc
%% returns the header from the mochiweb request.
%% @type key() = atom() | binary() | string().
header(Req,Key) ->
    (Req#request.raw):get_header_value(Key).


%% @doc
%% Composes a mochiweb response from plaza response record.
do_response(Response, Request) ->
    error_logger:info_msg("Sending back response: ~p", [Response]),
    case Response of
        {static, DocRoot, Path}  ->  PathP = case Path == [] of
                                                 true  -> "/index.html" ;
                                                 false -> case lists:nth(1,lists:reverse(Path)) == $/ of
                                                              true  -> Path ++ "index.html" ;
                                                              false -> Path
                                                          end
                                             end ,
                                     Request:serve_file(lists:nthtail(1,PathP), DocRoot) ;
        Resp                     ->  Request:respond({Resp#response.code,
                                                      Resp#response.headers,
                                                      Resp#response.body})
    end .


%% @doc
%% Composes a mochiweb response from plaza response record.
do_push_response(PushResponse, Request, RemainingStages) ->
    error_logger:info_msg("Doing push response ~p",[PushResponse]),
    Resp = extract_push_response(PushResponse),
    ChunkedResponse = Request:respond({Resp#response.code,
                                       Resp#response.headers,
                                       chunked}),
    error_logger:info_msg("Writing through ~p",[ChunkedResponse]),
    error_logger:info_msg("Writing chunk ~p",[Resp#response.body]),
    ChunkedResponse:write_chunk(Resp#response.body),
    push_response_cycle(Request, ChunkedResponse, RemainingStages) .

push_response_cycle(Request, Response, RemainingStages) ->
    receive
        Received ->  Resp = extract_push_response(Received),
                     case Resp of
                         {ok, [_M, R, ResponseP, StateP, A, Res]} ->
                             error_logger:info_msg("Push response cycle received push request",[]),
                             error_logger:info_msg("Push response received triples: ~p",[plaza_web:state_get(rdf_triples, StateP)]),
                             ResponsePP = do_processing_resource(RemainingStages, Res, R, ResponseP, StateP, A),
                             error_logger:info_msg("Writing chunk ~p",[ResponsePP#response.body]),
                             Response:write_chunk(ResponsePP#response.body),
                             error_logger:info_msg("Chunked response written",[]),
                             push_response_cycle(Request, Response, RemainingStages) ;
                         {error, Reason} ->
                             error_logger:info_msg("Error processing resource ~p", [Reason]),
                             Response:write_chunk("") ;
                         Other ->
                             error_logger:info_msg("Push response cycle received unknown request ~p, EXITING...",[Other]),
                             Response:write_chunk("")
                     end
        after ?PROXY_TIMEOUT ->
                Response:write_chunk(?IDLE_MESSAGE),
                push_response_cycle(Request, Response, RemainingStages)
    end .

%% @doc
%% Extracts the response from the push or nested push tuples.
extract_push_response({push, R}) ->
    extract_push_response(R) ;
extract_push_response(R) -> R .


%% Tests


match_path_a_test() ->
    Req = #request{},
    ?assertEqual(match_path([],[],Req),{ok, Req}),
    ?assertEqual(match_path(["whatever"],["*"],Req),{ok,Req}),
    ?assertEqual(match_path(["whatever"],[],Req),error),
    {ok,#request{ parameters = Params }} = match_path([test],["whatever"],Req),
    ?assertEqual(Params,[{test,"whatever"}]),
    {ok,#request{ parameters = ParamsB }} = match_path(["whatever"],["whatever"],Req),
    ?assertEqual(ParamsB,[]),
    ?assertEqual(match_path(["a"],["b"],Req), error),
    ?assertEqual(match_path(["a"], fun(["a"],Request) ->
                                           {ok, Request}
                                   end,
                            Req),
                 {ok,Req}),
    ?assertEqual(match_path(["a"], fun(["a"],_Req) ->
                                           error
                                   end,
                            Req),
                 error),
    {ok,#request{ parameters=ParamsC }} =  match_path(["Blogs",id],["Blogs","17a10803-f064-4718-ae46-4a6d3c88415c"],Req),
    ?assertEqual(ParamsC,[{id,"17a10803-f064-4718-ae46-4a6d3c88415c"}]) .
