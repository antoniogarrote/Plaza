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
    create_queues(ProxyState),
    plaza_repository:connect(ProxyState),
    ServerOptions = ProxyState#plaza_app.server_options,
    ServerName = plaza_webservers_controller:start_server(ServerOptions),
    Result = case ServerName of
                 error  -> {stop, "Unable to start web server"} ;
                 _Id    -> Routes = ProxyState#plaza_app.routes,
                           plaza_mochiweb_adapter:update_routes(ServerName, Routes, ProxyState),
                           {ok, ProxyState}
    end,
    Result .


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


create_queues(State) ->
    AppModule = State#plaza_app.application_module,
    Resources = lists:filter(fun(R) ->
                                  R:is_metaresource()
                             end,
                             apply(AppModule, resources, [])),
    BinarizedResources = lists:map(fun(R) -> plaza_utils:to_binary(R) end, Resources),
    error_logger:info_msg("Creating queues for resources ~p",[BinarizedResources]),
    lists:foreach(fun(R) ->
                          plaza_rabbit_backend:create_queue(R, R)
                  end,
                  BinarizedResources),
    BinarizedResources.

make_initial_state(Options) ->
    AppModule = list_to_atom(plaza_utils:proplist_find(app,Options)),
    RepoModule = apply(AppModule, repository_module, []),
    VocabularyModule = apply(AppModule, vocabulary_module, []),
    Environment = case plaza_utils:proplist_find(env,Options) of
                      none -> development ;
                      Env  -> list_to_atom(Env)
                  end,
    ServerOptions = apply(AppModule, server_configuration, []),
    Tokens = collect_resources(apply(AppModule, resources, [])),
    Routes = apply(AppModule, routes, []) ++ plaza_ts_trees:generate_trees(AppModule:write_tree(), Tokens),
    #plaza_app{name =  list_to_atom(plaza_utils:proplist_find(name,Options)),
               application_module = AppModule,
               repository_module = RepoModule,
               server_options = ServerOptions,
               write_tree = plaza_ts_trees:make(AppModule:write_tree()),
               routes = Routes,
               environment = Environment,
               url_tokens = Tokens,
               namespaces = plaza_namespaces:merge([plaza_core_ontology:namespaces(),
                                                    plaza_namespaces:make(apply(VocabularyModule, namespaces,[]))]),
               vocabulary = compile_vocabulary([plaza_core_ontology:vocabulary() |
                                                lists:map(fun(L) -> plaza_vocabulary:make(L) end,
                                                          apply(VocabularyModule, vocabulary, []))]) } .


compile_vocabulary(Vocabularies) ->
    plaza_vocabulary:merge(Vocabularies) .

collect_resources(Resources) ->
    collect_resources(Resources,dict:new()) .

collect_resources([], Acum) ->
    Acum ;
collect_resources([R | Rs], Acum) ->
    Token = R:url_token(),
    AcumP = case R:is_metaresource() of
                true  -> dict:append(Token, {metaresource, R}, Acum) ;
                false -> dict:append(Token, {resource, R}, Acum)
            end,
    collect_resources(Rs,AcumP) .
