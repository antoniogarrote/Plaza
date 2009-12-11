-module(plaza_apps) .

-author("Antonio Garrote Hernandez") .

-include_lib("eunit/include/eunit.hrl").
-include_lib("states.hrl").
-include_lib("definitions.hrl").
-include_lib("http_records.hrl").

-import(plaza_admin_app, [domain/0]) .

-export([namespaces/0, vocabulary/0, generate_instance_uri/4, triple_space/0, uri/0, is_metaresource/0, operations/0, lifting/5, operation/5, lowering/5]) .
-export([read_tree/0, url_token/0]) .


triple_space() -> "http://" ++ domain() ++ "/plaza_apps" .

uri() -> "http://" ++ domain() ++ "/plaza_apps" .

is_metaresource() -> true .

read_tree() -> [] .

operations() -> ['GET'] .

url_token() -> plaza_apps .

generate_instance_uri(_Request, _Response, _Context, _Application) ->
    list_to_binary(uri() ++ "/" ++uuid:string()) .


%% Ontology


namespaces() ->
    [] .

vocabulary() ->
    [] .


%% request : lifting -> operation -> lowering

lifting('GET', Request, Response, Context, Application) ->
    plaza_web:lifting('GET', Request, Response, Context, Application, ?MODULE) .


operation('GET', Request, Response, Context, Application) ->
    Applications = plaza_applications_controller:get_application_configurations(),
    Triples = lists:map(fun(A) -> plaza_app_to_triples(A) end, Applications),
    RdfTriples = plaza_triples:set(lists:foldl(fun(X, Ac) -> Ac ++ X end, [], Triples)),
    % Vocabulary = plaza_vocabulary:make(plaza_admin_app:namespaces()),
    % RdfTriples = plaza_triples:norm(Vocabulary, TriplesP),
    ResponseP = Response#response{ code=200 },
    ContextP = plaza_web:state_update(rdf_triples, RdfTriples, Context),

    {ok, ['GET', Request, ResponseP, ContextP, Application, ?MODULE]} .



lowering('GET', Request, Response, Context, Application) ->
    plaza_web:lowering('GET', Request, Response, Context, Application, ?MODULE) .

%% private functions

plaza_app_to_triples(PlazaApp) ->
    Uri = list_to_binary(uri() ++ "/" ++ atom_to_list(PlazaApp#plaza_app.name)),

    NameT = plaza_triples:t(plaza_triples:uri(Uri),
                            plaza_triples:uri(pzadm, "name"),
                            plaza_triples:l(plaza_utils:to_binary(PlazaApp#plaza_app.name))),
    error_logger:info_msg("Name triple : ~p",[NameT]),
    AppMT = plaza_triples:t(plaza_triples:uri(Uri),
                            plaza_triples:uri(pzadm, "application_module"),
                            plaza_triples:l(plaza_utils:to_binary(PlazaApp#plaza_app.application_module))),

    RepMT = plaza_triples:t(plaza_triples:uri(Uri),
                            plaza_triples:uri(pzadm, "repository_module"),
                            plaza_triples:l(plaza_utils:to_binary(PlazaApp#plaza_app.repository_module))),

    EnvT = plaza_triples:t(plaza_triples:uri(Uri),
                           plaza_triples:uri(pzadm, "environment"),
                           plaza_triples:l(plaza_utils:to_binary(PlazaApp#plaza_app.environment))),

    [NameT, AppMT, RepMT, EnvT] .

%%    Triples = plaza_triples:set([NameT, AppMT, RepMT, EnvT]),

%%     Vocabulary = plaza_vocabulary:make(plaza_admin_app:namespaces()),

%%     plaza_triples:norm(Vocabulary, Triples) .
