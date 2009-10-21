-module(vocabulary) .

-author("Antonio Garrote Hernandez") .

-export([vocabulary/0]) .


repository_connection() ->
    [{test_app, <<"http://plaza-framework.org/test_app#">>}] .
