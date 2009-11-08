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
-export([to_atom/1, dict_find/2, subbitstring/2, merge_dicts/2]) .


%% Public API

subbitstring(_BS1,BS2) when size(BS2) =:= 0 ->
    false ;
subbitstring(BS1, BS2) when size(BS1) =:= 0->
    {ok, BS2} ;
subbitstring(BS1, BS2) ->
    <<H1:1/binary,R1/binary>> = BS1,
    <<H2:1/binary,R2/binary>> = BS2,
    case H1 == H2 of
        true  -> subbitstring(R1,R2) ;
        false -> false
    end .


proplist_find(Key, Props) ->
    case proplists:lookup(Key, Props) of
        none -> none ;
        {Key, Value} -> Value
    end .

dict_find(Key, Dict) ->
    case dict:find(Key, Dict) of
        error -> none ;
        {ok, Value} -> Value
    end .


select_environment(Env, Props) ->
    proplist_find(Env, Props) .


to_binary(Data) when is_list(Data)   ->
    list_to_binary(Data) ;
to_binary(Data) when is_binary(Data) ->
    Data ;
to_binary(Data) when is_atom(Data)   ->
    list_to_binary(atom_to_list(Data)) .

to_atom(Data) when is_list(Data) ->
    list_to_atom(Data) ;
to_atom(Data) when is_binary(Data) ->
    list_to_atom(binary_to_list(Data)) ;
to_atom(Data) when is_atom(Data) ->
    Data .

split(Str, Regex) ->
    lists:filter(fun(L) -> L =/= [] end,re:split(Str,Regex,[{return,list}])).

strip_protocol(Uri) ->
    [Protocol, Rest] = split(Uri,"://"),
    {Protocol,Rest} .

strip_protocol_domain(Uri) ->
    {Protocol,Tmp} = strip_protocol(Uri),
    [Domain | Rest] = split(Tmp, "/"),
    {Protocol ++ "://" ++ Domain, lists:foldl(fun(Part,Acum) -> Acum ++ "/" ++ Part end, "", Rest)} .


merge_dicts(DictPrevalent, Dict) ->
    Keys = dict:fetch_keys(DictPrevalent),
    do_merge_dicts(Keys, DictPrevalent, Dict) .
do_merge_dicts([], _Dict, Merged) ->
    Merged ;
do_merge_dicts([K | Ks], Dict, Merged) ->
    {ok, Value} = dict:find(K, Dict),
    do_merge_dicts(Ks, Dict, dict:store(K, Value, Merged)) .

%% Tests

subbitstring_test() ->
    A = <<"test">>,
    B = <<"test&something else">>,
    Res = subbitstring(A,B),
    ?assertEqual(Res,{ok, <<"&something else">>}),
    C = <<"no way">>,
    ResB = subbitstring(A,C),
    ?assertEqual(ResB,false) .


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

dict_find_test() ->
    Data = [{a, "a"}, {b, "b"}],
    Dict = dict:from_list(Data),
    ?assertEqual("a", dict_find(a, Dict)),
    ?assertEqual(none, dict_find(c, Dict)) .

select_environment_test() ->
    Data = [{development, [{a, "a"}]},
            {production,  [{b, "b"}]}],
    ?assertEqual("a", proplist_find(a, select_environment(development, Data))) .

to_binary_test() ->
    ?assertEqual(<<"test">>, to_binary("test")),
    ?assertEqual(<<"test">>, to_binary(<<"test">>)),
    ?assertEqual(<<"test">>, to_binary(test)) .

to_atom_test() ->
    ?assertEqual(test, to_atom("test")),
    ?assertEqual(test, to_atom(<<"test">>)),
    ?assertEqual(test, to_atom(test)) .
