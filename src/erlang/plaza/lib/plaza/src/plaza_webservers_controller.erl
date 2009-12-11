%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(plaza_webservers_controller) .

-author("Antonio Garrote Hernandez") .

-behaviour(gen_server) .

-include_lib("states.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/0, start_server/1]) .
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% Public API


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) .


start_server(Options) ->
    gen_server:call(?MODULE, {start_web_server, Options}) .


%% Callbacks


init(_State) ->
    {ok, [] }.


handle_call({start_web_server, Options}, _From, Servers) ->
    {port,Port} = proplists:lookup(port,Options),
    Identifier = list_to_atom(lists:flatten(io_lib:format("plaza_server_~p",[Port]))),
    {Result, NewState} = case lists:member(Identifier, Servers) of
                              false  -> plaza_mochiweb_adapter:start_link(Identifier,Options),
                                        {Identifier, [Identifier | Servers]} ;
                              true   -> {Identifier, Servers}
                          end,
    {reply, Result, NewState} .


%% dummy callbacks so no warning are shown at compile time
handle_cast(_Msg, State) ->
    {noreply, State} .


handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State} .


terminate(shutdown, _State) ->
    ok.


%% Private methods
