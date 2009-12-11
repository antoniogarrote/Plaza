%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(plaza_vocabulary) .

-author("Antonio Garrote Hernandez") .


-include_lib("eunit/include/eunit.hrl").


-export([make/1, resolve/2, merge/1]) .


%% This implementation of a vocabulary is defined in terms
%% of the Erlang dict data structure.


%% public API


%% @doc
%% Creates a new dictionary from a proplist.
make(Proplist) ->
    dict:from_list(Proplist) .


%% @doc
%% Searches for a term in the vocabulary.
resolve(Vocabulary, Term) when is_list(Term) ->
    resolve(Vocabulary, list_to_atom(Term)) ;

resolve(Vocabulary,Term) ->
    error_logger:info_msg("Resolving ~p with dict ~p",[Vocabulary, Term]),
    case dict:find(Term, Vocabulary) of
        {ok, Value}   -> Value ;
        error         -> error

    end .

%% @doc
%% Merges a list of vocabularies into a single vocabulary.
%% If two terms in the vocabulary collide, the latter overwrites the prior.
merge(Vocabularies) ->
    lists:foldl(fun(Dict, Acum) ->
                        dict:merge(fun(_Key, Value1, _Value2) -> Value1 end,
                                   Dict, Acum)
                end,
                dict:new(),
                Vocabularies) .


%% Tests


make_test() ->
    Props = [{a,"a"},{b,"b"},{c,"c"}],
    Vocabulary = make(Props),
    ?assertEqual({ok, "a"}, dict:find(a,Vocabulary)),
    ?assertEqual({ok, "b"}, dict:find(b,Vocabulary)),
    ?assertEqual({ok, "c"}, dict:find(c,Vocabulary)) .


resolve_test() ->
    Props = [{a,"a"},{b,"b"},{c,"c"}],
    Vocabulary = make(Props),
    ?assertEqual("a", resolve(Vocabulary,a)),
    ?assertEqual("b", resolve(Vocabulary,b)),
    ?assertEqual("c", resolve(Vocabulary,c)),
    ?assertEqual(error, try resolve(Vocabulary,d) of
                           V -> V
                        catch
                           error:_ -> error
                        end) .

merge_test() ->
    Props = [{a,"a"},{b,"b"}],
    Vocabulary = make(Props),
    Props2 = [{a,"a2"},{c,"c2"}],
    Vocabulary2 = make(Props2),
    Merged = merge([Vocabulary, Vocabulary2]),
    ?assertEqual("a2", resolve(Merged,a)),
    ?assertEqual("b", resolve(Merged,b)),
    ?assertEqual("c2", resolve(Merged,c)) .
