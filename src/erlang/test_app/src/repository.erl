-module(repository) .

-author("Antonio Garrote Hernandez") .

-export([repository_connection/0]) .


repository_connection() ->
    [{development, [{node, 'sesame@nb-agarrote'},
                    {type, "memory"},
                    {persistent, "false"}]}] .
