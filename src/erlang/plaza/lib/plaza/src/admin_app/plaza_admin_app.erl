-module(plaza_admin_app) .

-author("Antonio Garrrote Hernandez") .

-include_lib("states.hrl").
-include_lib("definitions.hrl").
-include_lib("http_records.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([repository_connection/0]) .
-export([create_tbox/2, update_tbox/2, clear_tbox/1]) .
-export([repository_module/0, vocabulary_module/0, server_configuration/0, routes/0, domain/0]) .
-export([write_tree/0, resources/0]) .
-export([vocabulary/0, namespaces/0]) .



%% Plaza Application Configuration


repository_module() -> plaza_admin_app .


vocabulary_module() -> plaza_admin_app .


server_configuration() ->
    [{port, 7777}] .


domain() ->
    "localhost:7777" .


routes() -> [] .


write_tree() -> [plaza_apps] .


resources() -> [plaza_apps] .


repository_connection() ->
    {ok, AdminOpts} = application:get_env(plaza,administration_app),
    plaza_utils:proplist_find(repository_connections, AdminOpts) .


% vocabularies

namespaces() ->
    [{pzadm, <<"http://semantic_rest.org/plaza/admin#">>}] .


vocabulary() ->
    [ ] .



%% Public API


create_tbox(Application, Options) ->
    spawn(fun() ->
                  URL = ?PLAZA_URL ++ "/admin/tbox/" ++ atom_to_list(Application),
                  CommonURL = ?PLAZA_URL ++ "/admin/tbox",
                  Format = plaza_utils:proplist_find(format, Options),
                  Path = plaza_utils:proplist_find(file, Options),
                  {ok, Triples} = file:read_file(Path),

                  {ok, AdminOpts} = application:get_env(plaza,administration_app),
                  Configuration = #plaza_app{ repository_module = plaza_admin_app,
                                              environment = plaza_utils:proplist_find(environment, AdminOpts) },
                  plaza_repository:add_encoded_triples(Configuration, URL, Triples, [URL, CommonURL], Format)
          end) .


update_tbox(Application, Options) ->
    spawn(fun() ->
                  URL = ?PLAZA_URL ++ "/admin/tbox/" ++ atom_to_list(Application),
                  CommonURL = ?PLAZA_URL ++ "/admin/tbox",
                  Format = plaza_utils:proplist_find(format, Options),
                  Path = plaza_utils:proplist_find(file, Options),
                  {ok, Triples} = file:read_file(Path),

                  {ok, AdminOpts} = application:get_env(plaza,administration_app),
                  Configuration = #plaza_app{ repository_module = plaza_admin_app,
                                              environment = plaza_utils:proplist_find(environment, AdminOpts) },

                  plaza_repository:delete_graph(Configuration, URL, []),
                  plaza_repository:add_encoded_triples(Configuration, URL, Triples, [URL, CommonURL], Format)
          end) .


clear_tbox(Application) ->
    spawn(fun() ->
                  URL = ?PLAZA_URL ++ "/admin/tbox/" ++ atom_to_list(Application),
                  CommonURL = ?PLAZA_URL ++ "/admin/tbox",
                  {ok, AdminOpts} = application:get_env(plaza,administration_app),
                  Configuration = #plaza_app{ repository_module = plaza_admin_app,
                                              environment = plaza_utils:proplist_find(environment, AdminOpts) },

                  plaza_repository:delete_graph(Configuration, URL, [CommonURL])
          end) .
