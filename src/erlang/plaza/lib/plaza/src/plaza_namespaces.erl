%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(plaza_namespaces) .

-author("Antonio Garrote Hernandez") .


-include_lib("eunit/include/eunit.hrl").


-export([make/1, resolve_prefix/2, with_namespace/2, merge/1, update_uris/2, update_triples/2]) .


%% This implementation of a vocabulary is defined in terms
%% of the Erlang dict data structure.


%% public API


%% @doc
%% Creates a new namespaces dictionary from a proplist.
make(Proplist) ->
    Direct = dict:from_list(Proplist),
    Inverse = lists:map(fun({A,B}) -> {plaza_utils:to_binary(B),A} end, Proplist),
    %Inverse = dict:from_list(InverseList),
    {Direct, Inverse}.



%% @doc
%% Given a namespaces' dictionary and a namespace's symbol,
%% returns the prefix for that namespace.
resolve_prefix({Direct,_Inverse}, Term) when is_atom(Term) ->
   plaza_utils:dict_find(Term, Direct) ;

resolve_prefix({Direct, _Inverse},Term) ->
    plaza_utils:dict_find(plaza_utils:to_atom(Term), Direct).


%% @doc
%% Given a dicitionary of namespaces and a URI, try to find a namespace for
%% that uri and returns the namespace and the namespaced uri or false
with_namespace({Direct,Inverse}, Term) when is_list(Term) ->
    with_namespace({Direct, Inverse}, plaza_utils:to_binary(Term)) ;

with_namespace({_Direct, Inverse},Term) ->
    do_with_namespace(Inverse, Term) .

do_with_namespace([], Term) ->
    {none, Term} ;
do_with_namespace([{UriPrefix, Name} | Nss], Term) ->
    case plaza_utils:subbitstring(UriPrefix, Term) of
        {ok, Rest} -> {Name, list_to_binary([plaza_utils:to_binary(Name), <<":">>, Rest])} ;
        false      -> do_with_namespace(Nss, Term)
    end .


%% @doc
%% Merges a list of vocabularies into a single vocabulary.
%% If two terms in the vocabulary collide, the latter overwrites the prior.
merge(Namespaces) ->
    lists:foldl(fun({D,I}, {AD,AI}) ->
                        ND = dict:merge(fun(_Key, Value1, _Value2) -> Value1 end,
                                        D, AD),
                        NI = lists:foldl(fun({K,V}, Acum) ->
                                                 case proplists:lookup(K,Acum) of
                                                     none   -> [{K,V} | Acum] ;
                                                     _Other -> Acum
                                                 end
                                         end,[], I ++ AI),
                        {ND,NI}
                end,
                {dict:new(),[]},
                Namespaces) .


update_uris(Uris, Ns) ->
    do_update_uris(Uris,Ns, [],[]) .

do_update_uris([], _Ns, Uris, Nsp) ->
    {Uris,Nsp} ;
do_update_uris([U | Us], Ns, Uris, Nsp) ->
    case with_namespace(Ns, U) of
        {FoundNs, Updated} ->
            case lists:any(fun(ANs) ->
                                   FoundNs =:= ANs
                           end,
                           Nsp) of
                true  ->
                    do_update_uris(Us, Ns, [Updated | Uris], Nsp) ;
                false ->
                    do_update_uris(Us, Ns, [Updated | Uris], [FoundNs | Nsp])
            end
    end .


update_triples(Triples, Ns) ->
    do_update_triples(Triples,Ns, [],[]) .

do_update_triples([], _Ns, Triples, Nsp) ->
    {Triples,Nsp} ;
do_update_triples([{S,U,O,C} | Us], Ns, Triples, Nsp) ->
    case with_namespace(Ns, U) of
        {FoundNs, Updated} ->
            case lists:any(fun(ANs) ->
                                   error_logger:info_msg("COMPARING: ~p vs ~p ? ~p",[FoundNs, ANs, (FoundNs =:= ANs)]),
                                   FoundNs =:= ANs
                           end,
                           Nsp) of
                true  ->
                    do_update_triples(Us, Ns, [{S,Updated,O,C} | Triples], Nsp) ;
                false ->
                    do_update_triples(Us, Ns, [{S,Updated,O,C} | Triples], [FoundNs | Nsp])
            end
    end ;
do_update_triples([{S,U,O} | Us], Ns, Triples, Nsp) ->
    case with_namespace(Ns, U) of
        {FoundNs, Updated} ->
            case lists:any(fun(ANs) ->
                                   FoundNs =:= ANs
                           end,
                           Nsp) of
                true  ->
                    do_update_triples(Us, Ns, [{S,Updated,O} | Triples], Nsp) ;
                false ->
                    do_update_triples(Us, Ns, [{S,Updated,O} | Triples], [FoundNs | Nsp])
            end
    end .

%% Tests

update_uris_test() ->
    Ns = make([{dc, <<"http://purl.dc.org#">>},
               {test, <<"http://test.com#">>},

               {never, <<"http://never.com#">>}]),
    {Uris, Nsp} = update_uris([<<"http://purl.dc.org#Test">>,
                               <<"http://test.com#Other">>],
                              Ns),
    ?assertEqual([<<"test:Other">>,<<"dc:Test">>], Uris),
    ?assertEqual([test,dc], Nsp) .

with_namespace_test() ->
    Ns = make([{dc, <<"http://purl.dc.org#">>},
               {test, <<"http://test.com#">>}]),
    Res = with_namespace(Ns, <<"http://purl.dc.org#Title">>),
    ?assertEqual(Res,{dc, <<"dc:Title">>}) .


merge_test() ->
    NSA = make([{testa, <<"http://testa.com#">>}]),
    NSB = make([{testb, <<"http://testb.com#">>}]),
    NSC = make([{testa, <<"http://testa.com#">>}]),
    {_D,I} = merge([NSA, NSB, NSC]),
    ?assertEqual(I,[{<<"http://testb.com#">>, testb},
                    {<<"http://testa.com#">>, testa}]) .
