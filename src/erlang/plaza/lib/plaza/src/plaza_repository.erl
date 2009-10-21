%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(plaza_repository) .

-author("Antonio Garrote Hernandez") .

-include_lib("eunit/include/eunit.hrl") .
-include_lib("states.hrl") .

-export([connect/1, add_encoded_triples/4, sparql_query/2]) .


%% Some definitions of the messages exchanged with the
%% repository Java node
-define(REPOSITORY_JAVA_MBOX, triples_repository) .
-define(CONNECT_MESSAGE, connect) .
-define(ADD_ENCODED_TRIPLES_MESSAGE, add_triples) .
-define(QUERY_MESSAGE, 'query') .

%% Public API

connect(Config) ->
    error_logger:info_msg("Connecting to repository with config ~p", [Config]),
    Opts = retrieve_repository_options(Config),
    error_logger:info_msg("Repository configuration ~p", [Opts]),
    Node = plaza_utils:proplist_find(node, Opts),
    error_logger:info_msg("Repository node ~p", [Node]),
    {?REPOSITORY_JAVA_MBOX, Node} ! {self(), ?CONNECT_MESSAGE, Opts},
    receive
        ok     -> ok ;
        _Error -> erlang:error("Impossible to stablish connection to the repository", Opts)
    end .


add_encoded_triples(Config, BaseUrl, Triples, Format) ->
    Opts = retrieve_repository_options(Config),
    Node = plaza_utils:proplist_find(node, Opts),
    {?REPOSITORY_JAVA_MBOX, Node} ! {self(), ?ADD_ENCODED_TRIPLES_MESSAGE, [{base, BaseUrl},
                                                                            {triples, Triples},
                                                                            {format, Format}]},
    receive
        ok     -> ok ;
        _Error -> erlang:error("Impossible to add triples to the repository", [Config, BaseUrl, Triples, Format])
    end .


sparql_query(Config, Query) ->
    Opts = retrieve_repository_options(Config),
    Node = plaza_utils:proplist_find(node, Opts),
    {?REPOSITORY_JAVA_MBOX, Node} ! {self(), ?QUERY_MESSAGE, [{'query', Query}]},
    receive
        {ok, Result} -> Result ;
        Error        -> erlang:error("Error querying repository:" ++ Error, [Config, Query])
    end .


%% Private methods


retrieve_repository_options(ApplicationConfiguration) ->
    RepositoryConnectionOpts = apply(ApplicationConfiguration#plaza_app.repository_module, repository_connection, []),
    plaza_utils:select_environment(ApplicationConfiguration#plaza_app.environment, RepositoryConnectionOpts) .
