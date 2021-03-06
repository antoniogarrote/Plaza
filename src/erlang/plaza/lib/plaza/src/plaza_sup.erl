%%%----------------------------------------------------------------
%%% @author  Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------
-module(plaza_sup).

-author("Antonio Garrote Hernandez") .

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    PlazaRabbitBackend = {plaza_rabbit_backend, {plaza_rabbit_backend, start_link, []},
                          Restart, Shutdown, Type, [plaza_rabbit_backend]},

    PlazaApplicationsController  = {plaza_applications_controller, {plaza_applications_controller, start_link, []},
                                    Restart, Shutdown, Type, [plaza_applications_controller]},

    PlazaWebserversController  = {plaza_webservers_controller, {plaza_webservers_controller, start_link, []},
                                  Restart, Shutdown, Type, [plaza_webservers_controller]},

    {ok, {SupFlags, [PlazaRabbitBackend, PlazaApplicationsController, PlazaWebserversController]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


