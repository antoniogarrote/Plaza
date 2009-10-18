%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(plaza_utils) .

-author("Antonio Garrote Hernandez") .

-include_lib("eunit/include/eunit.hrl").

-export([proplist_find/2, select_environment/2]) .


%% Public API


proplist_find(Key, Props) ->
    case proplists:lookup(Key, Props) of
        none -> none ;
        {Key, Value} -> Value
    end .

select_environment(Env, Props) ->
    proplist_find(Env, Props) .


%% Tests


proplist_find_test() ->
    Data = [{a, "a"}, {b, "b"}],
    ?assertEqual("a", proplist_find(a, Data)),
    ?assertEqual(none, proplist_find(c, Data)) .

select_environment_test() ->
    Data = [{development, [{a, "a"}]}, 
            {production,  [{b, "b"}]}],
    ?assertEqual("a", proplist_find(a, select_environment(development, Data))) .
