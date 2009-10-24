%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(plaza_application_proxy) .

-author("Antonio Garrote Hernandez") .

-behaviour(gen_server) .

-include_lib("eunit/include/eunit.hrl").
-include_lib("states.hrl").

-export([start_link/1, get_configuration/1]) .
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% Public API


start_link(Options) ->
    {name, Name} = proplists:lookup(name,Options),
    gen_server:start_link({local, list_to_atom(Name)}, ?MODULE, Options, []) .


get_configuration(ApplicationName) ->
    gen_server:call(ApplicationName, get_configuration) .


%% Callbacks


init(Options) ->
    ProxyState = make_initial_state(Options),
    ServerOptions = ProxyState#plaza_app.server_options,
    ServerName = plaza_webservers_controller:start_server(ServerOptions),
    Result = case ServerName of
                 error  -> {stop, "Unable to start web server"} ;
                 _Id    -> Routes = ProxyState#plaza_app.routes,
                           plaza_mochiweb_adapter:update_routes(ServerName, Routes, ProxyState),
                           {ok, ProxyState#plaza_app{ webserver = ServerName }}
             end,
    Result  .


handle_call(get_configuration, _From, State) ->
    {reply, State, State} .


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


make_initial_state(Options) ->
    AppModule   = list_to_atom(plaza_utils:proplist_find(app,Options)),
    RepoModule  = apply(AppModule, repository_module, []),
    VocabularyModule = apply(AppModule, vocabulary_module, []),
    Environment = case plaza_utils:proplist_find(env,Options) of
                      none -> development ;
                      Env  -> list_to_atom(Env)
                  end,
    ServerOptions = apply(AppModule, server_configuration, []),
    Routes = apply(AppModule, routes, []),
    #plaza_app{name =  list_to_atom(plaza_utils:proplist_find(name,Options)),
               application_module = AppModule,
               repository_module = RepoModule,
               server_options = ServerOptions,
               routes = Routes,
               environment = Environment,
               vocabulary = compile_vocabulary([plaza_core_ontology:vocabulary() |
                                                lists:map(fun(L) -> plaza_vocabulary:make(L) end,
                                                          apply(VocabularyModule, vocabulary, []))]) } .


compile_vocabulary(Vocabularies) ->
    plaza_vocabulary:merge(Vocabularies) .
