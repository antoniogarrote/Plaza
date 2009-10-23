-module(demo_application) .

-author("Antonio Garrote Hernandez") .

-export([repository_module/0, vocabulary_module/0, server_configuration/0]) .

repository_module() -> repository .

vocabulary_module() -> vocabulary .

server_configuration() ->
    [{port, 7777}] .
