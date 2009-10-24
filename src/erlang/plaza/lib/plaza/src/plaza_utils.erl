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

-export([proplist_find/2, select_environment/2, to_binary/1, split/2, strip_protocol/1, strip_protocol_domain/1]) .


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

strip_protocol(Uri) ->
    [Protocol, Rest] = split(Uri,"://"),
    {Protocol,Rest} .

strip_protocol_domain(Uri) ->
    {Protocol,Tmp} = strip_protocol(Uri),
    [Domain | Rest] = split(Tmp, "/"),
    {Protocol ++ "://" ++ Domain, lists:foldl(fun(Part,Acum) -> Acum ++ "/" ++ Part end, "", Rest)} .


%% Tests


strip_protocol_test() ->
    ?assertEqual({"http","test.com/test"},strip_protocol("http://test.com/test")),
    ?assertEqual({"ftp","test.com/test"},strip_protocol("ftp://test.com/test")) .


strip_protocol_domain_test() ->
    ?assertEqual({"http://test.com","/test/:a/b"},strip_protocol_domain("http://test.com/test/:a/b")),
    ?assertEqual({"ftp://test.com:3000","/test/c/d.e"},strip_protocol_domain("ftp://test.com:3000/test/c/d.e")) .


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
