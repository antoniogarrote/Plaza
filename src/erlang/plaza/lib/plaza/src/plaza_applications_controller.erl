%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(plaza_applications_controller) .

-author("Antonio Garrote Hernandez") .

-behaviour(gen_server) .

-include_lib("states.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/0, start_plaza_application/1]) .
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% Public API


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) .


start_plaza_application(Options) ->
    gen_server:call(?MODULE, {start_plaza_application, Options}) .


%% Callbacks


init(_State) ->
    %% seed the random number generator
    {ok, #app_controller{} }.


handle_call({start_plaza_application, Options}, _From, #app_controller{ apps=Apps } = State) ->
    {NewState, Result} = case proplists:lookup(name,Options) of
                             none         -> {State, {error, "no name for application to start"}} ;
                             {name, Name} -> Pid = plaza_application_proxy:start_link(Options),
                                             {State#app_controller{ apps=[{list_to_atom(Name), Pid} | Apps] }, ok}
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
