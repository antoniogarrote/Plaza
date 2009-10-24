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

-export([start_link/2, update_routes/3, loop/3, handle_request/2, header/2]) .
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% Public API


start_link(Name, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name,Options}, []) .


update_routes(Name, Routes, Application) ->
    gen_server:call(Name, {update_routes, Routes, Application}) .


%% Callbacks


init({Name,Options}) ->
    Loop = fun (Req) ->
		   ?MODULE:loop(Name,Req, none)
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
    {reply, ok, Routes ++ UpdatedRoutes } .


handle_cast({handle_request, Request}, Routes) ->
    spawn(?MODULE,
          handle_request,
          [Request, Routes]),
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
handle_request(Request, Routes) ->
    error_logger:info_msg("received web request: ~p", [Request]),
    PlazaRequest = make_request(Request),
    Response = route(PlazaRequest, Routes),
    do_response(Response,Request) .


%% @doc
%% Check if a path and a patter match, updating the Request
%% object accordingly
match_path([], [], Request) ->
    {ok, Request} ;
match_path(_Path, ["*"], Request) ->
    {ok, Request} ;
match_path(_Pattern, [], _Request) ->
    error ;
match_path(Pattern, F, Request) when is_function(F) ->
    case F(Pattern,Request) of
        {ok, Result} -> {ok, Result} ;
        _Other       -> error
    end ;
match_path([Pattern|Patterns], [Path|Paths], #request{ parameters = Params} = Request) when is_atom(Path)  ->
    match_path(Patterns, Paths, Request#request{ parameters = [{Path, Pattern} | Params]}) ;
match_path([Pattern|Patterns], [Path|Paths], Request) ->
    case Pattern =:= Path of
        true  -> match_path(Patterns, Paths, Request) ;
        false -> error
    end .


%% @doc
%% Provided a set of routes and a plaza request,
%% return {ok, action()} if any route matches or
%% error in any other case.
route(Request, Routes) ->
    RequestPathTokens = plaza_web:process_path_pattern(Request#request.path),
    error_logger:info_msg("routing: ~p", [RequestPathTokens]),
    case do_route(RequestPathTokens, Routes,Request) of
        {ok, {M,F}, Request, Application} -> error_logger:info_msg("routing success, routing to: ~p", [{M,F}]),
                                             M:F(Request, #response{}, [], Application) ;
        {ok, F, Request, Application}     -> error_logger:info_msg("routing success, routing to: ~p", [F]),
                                             F(Request, #response{}, [], Application) ;
        {error, Request}     -> error_logger:info_msg("routing error", []),
                                #response{ code = 404,
                                           headers = [{"Content-Type", "application/json"},
                                                      {"Charset", "UTF8"}],
                                           body = "\"Unknown resource\"" }
    end.



do_route(_Path, [], Request) ->
    {error, Request} ;
do_route(Path, [{Pattern, Action, Application} | Routes], Request) ->
    case match_path(Pattern, Path, Request) of
        {ok, Request}  -> {ok, Action, Request, Application} ;
        error          -> do_route(Path, Routes, Request)
    end .


loop(Name, Req, _DocRoot) ->
    gen_server:cast(Name, {handle_request, Req}) .


%% @doc
%% Creates a new request from the mochiweb equivalent
make_request(Req) ->
    #request{ method = Req:get(method),
              path = Req:get(path),
              headers = Req:get(headers),
              raw = Req,
              parameters = Req:parse_qs() ++ Req:parse_post()} .


%% @doc
%% returns the header from the mochiweb request.
%% @type key() = atom() | binary() | string().
header(Req,Key) ->
    (Req#request.raw):get_header_value(Key).


%% @doc
%% Composes a mochiweb response from plaza response record.
do_response(Response, Request) ->
    Request:respond({Response#response.code,
                     Response#response.headers,
                     Response#response.body}) .


%% Tests


match_path_a_test() ->
    Req = #request{},
    ?assertEqual(match_path([],[],Req),{ok, Req}),
    ?assertEqual(match_path(["whatever"],["*"],Req),{ok,Req}),
    ?assertEqual(match_path(["whatever"],[],Req),error),
    {ok,#request{ parameters = Params }} = match_path(["whatever"],[test],Req),
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
                 error) .
