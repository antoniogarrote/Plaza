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

-export([proplist_find/2, select_environment/2, to_binary/1, split/2]) .


%% Public API


proplist_find(Key, Props) ->
    case proplists:lookup(Key, Props) of
        none -> none ;
        {Key, Value} -> Value
    end .


select_environment(Env, Props) ->
    proplist_find(Env, Props) .


to_binary(Data) when is_list(Data)   ->
    list_to_binary(Data) ;
to_binary(Data) when is_binary(Data) ->
    Data ;
to_binary(Data) when is_atom(Data)   ->
    list_to_binary(atom_to_list(Data)) .

split(Str, Regex) ->
    lists:filter(fun(L) -> L =/= [] end,re:split(Str,Regex,[{return,list}])).



%% Tests


proplist_find_test() ->
    Data = [{a, "a"}, {b, "b"}],
    ?assertEqual("a", proplist_find(a, Data)),
    ?assertEqual(none, proplist_find(c, Data)) .

select_environment_test() ->
    Data = [{development, [{a, "a"}]}, 
            {production,  [{b, "b"}]}],
    ?assertEqual("a", proplist_find(a, select_environment(development, Data))) .

to_binary_test() ->
    ?assertEqual(<<"test">>, to_binary("test")),
    ?assertEqual(<<"test">>, to_binary(<<"test">>)),
    ?assertEqual(<<"test">>, to_binary(test)) .
