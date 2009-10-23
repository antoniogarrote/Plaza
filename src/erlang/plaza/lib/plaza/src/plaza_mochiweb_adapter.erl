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

-export([start_link/2, update_routes/2, loop/3, handle_request/2, header/2]) .
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% Public API

start_link(Name,Options) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name,Options}, []) .


update_routes(Name,Routes) ->
    gen_server:call(Name, {update_routes, Routes}) .


%% Callbacks


init({Name,Options}) ->
    Loop = fun (Req) ->
		   ?MODULE:loop(Name,Req, none)
	   end,
    Params = [{name, ?MODULE}, {loop, Loop} | Options],
    mochiweb_http:start(Params),
    {ok, []}.


handle_call({update_routes, NewRoutes}, _From, Routes) ->
    UpdatedRoutes = lists:map(fun({Pattern, Action}) ->
                                      { process_path_pattern(Pattern),
                                        Action }
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
    Request = make_request(Request),
    Response = route(Request, Routes),
    do_response(Response,Request) .

%%     Method = Request#request.method,
%%     Path = Request#request.path,
%%     Headers = Request#request.headers,
%%     Parameters = Request#request.parameters,
%%     Req:respond({200,
%%                  [{"Content-type", "plain/text"}],
%%                  lists:flatten(io_lib:format("Method:~p~nPath:~p~nHeaders:~p~nParameters:~p~n",[Method,Path,Headers,Parameters]))}) .


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

%% @doc
%% Provided a set of routes and a plaza request,
%% return {ok, action()} if any route matches or
%% error in any other case.
route(Request, Routes) ->
    RequestPathTokens = plaza_utils:split(Request#request.path),
    case do_route(RequestPathTokens, Routes,Request) of
        {ok, [M,F], Request} -> M:F(Request, #response{}, []) ;
        {ok, F, Request}     -> F(Request, #response{}, []) ;
        {error, Request}     -> #response{ code = 404,
                                           headers = [{"Content-Type", "application/json"},
                                                      {"Charset", "UTF8"}],
                                           body = "\"Unknown resource\"" }
    end.



do_route(_Path, [], Request) ->
    {error, Request} ;
do_route(Path, [{Pattern, Action} | Routes], Request) ->
    case match_path(Pattern, Path, Request) of
        {ok, Request}  -> {ok, Action, Request} ;
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

process_path_pattern_test() ->
    PatternA = process_path_pattern("/this/is/a/test"),
    ?assertEqual(["this","is","a","test"], PatternA),
    PatternB = process_path_pattern("/this/is/a/*"),
    ?assertEqual(["this","is","a","*"], PatternB),
    PatternC = process_path_pattern("/this/:verb/:article/*"),
    ?assertEqual(["this",verb,article,"*"], PatternC),
    PatternD = process_path_pattern("/this/:verb/:article/*/or.something"),
    ?assertEqual(["this",verb,article,"*","or","something"], PatternD) .
