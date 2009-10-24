%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(plaza_triples) .

-author("Antonio Garrote Hernandez") .


-include_lib("eunit/include/eunit.hrl").
-include_lib("triples.hrl").

-export([uri/2, uri/1, triple/3, triple/4, t/3, t/4, norm/2, set/0, set/1, set_add/4, set_add/5, literal/1, literal/3]).
-export([l/1, l/3]) .


%% Public API


%% @doc
%% Builds a new literal
literal(Value, Type, Lang) ->
    {literal, Value, Type, Lang} .

literal(Value) ->
    {literal, Value, undefined, undefined} .


%% @doc
%% shortcut for building a literal
l(Value, Type, Lang) ->
    literal(Value, Type, Lang) .
l(Value) ->
    literal(Value) .


%% @doc
%% Builds a new URI
uri(Ns, Id) ->
    {Ns, Id} .

uri(Uri) ->
    {Uri} .


%% @doc
%% Builds a new triple
triple(S,P,O) ->
    #triple{subject = S,
            predicate = P,
            object = O} .

triple(S,P,O,C) ->
    #triple{subject = S,
            predicate = P,
            object = O,
            context = C}  .


%% @doc
%% Alias for building a triple
t(S,P,O) ->
    triple(S,P,O) .

t(S,P,O,C) ->
    triple(S,P,O,C) .

%% @doc
%% A set of triples
set() ->
    gb_sets:new() .
set(L) ->
    gb_sets:from_list(L) .


%% @doc
%% Adds a triple to a set.
set_add(S,P,O,C,Set) ->
    gb_sets:add_element(t(S,P,O,C),Set) .

set_add(S,P,O,Set) ->
    gb_sets:add_element(t(S,P,O),Set) .


%% @doc
%% Transform triples into a normalized representation
norm(Vocabulary, #triple{} = T) ->
    {norm(Vocabulary, T#triple.subject),
     norm(Vocabulary, T#triple.predicate),
     norm(Vocabulary, T#triple.object),
     norm(Vocabulary, T#triple.context)} ;
norm(Vocabulary, {literal, V, undefined, undefined}) ->
    {literal, norm(Vocabulary,V), undefined, undefined} ;
norm(Vocabulary, {literal, V, T, undefined}) ->
    {literal, norm(Vocabulary,V), norm(Vocabulary,T), undefined} ;
norm(Vocabulary, {literal, V, T, L}) ->
    {literal, norm(Vocabulary,V), norm(Vocabulary,T), L} ;
norm(Vocabulary, {Ns, Id}) ->
    case gb_sets:is_set({Ns, Id}) of
        true -> norm_set(Vocabulary, {Ns, Id}) ;
        false -> plaza_utils:to_binary([norm(Vocabulary,Ns), norm(Vocabulary,Id)])
    end ;
norm(Vocabulary, {Uri}) ->
    plaza_utils:to_binary(norm(Vocabulary,Uri)) ;
norm(Vocabulary, T) when is_atom(T) ->
    plaza_vocabulary:resolve(Vocabulary, T) ;
norm(Vocabulary, T) ->
    case gb_sets:is_set(T) of
        true  -> norm_set(Vocabulary, T) ;
        false -> plaza_utils:to_binary(T)
    end .


norm_set(Vocabulary, S) ->
    lists:map(fun(T) ->
                      error_logger:info_msg("normalizing:~p~n",[T]),
                      norm(Vocabulary,T)
              end,
              gb_sets:to_list(S)) .

%% Tests


uri_test() ->
    ?assertEqual({"http://test.com#anything"}, uri("http://test.com#anything")),
    ?assertEqual({rdf,description}, uri(rdf,description)) .


norm_any_test() ->
    Vocabulary = plaza_vocabulary:make([{rdf, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>},
                                        {dc, <<"http://purl.org/dc/elements/1.1/">>}]),
    ?assertEqual(<<"http://test.com#anything">>,
                 norm(Vocabulary,"http://test.com#anything")) .


norm_atom_test() ->
    Vocabulary = plaza_vocabulary:make([{rdf, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>},
                                        {dc, <<"http://purl.org/dc/elements/1.1/">>}]),
    ?assertEqual(<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>,
                 norm(Vocabulary, rdf)) .


norm_uri_a_test() ->
    Vocabulary = plaza_vocabulary:make([{rdf, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>},
                                        {dc, <<"http://purl.org/dc/elements/1.1/">>}]),
    ?assertEqual(<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#Description">>,
                 norm(Vocabulary, uri(rdf,"Description"))) .


norm_uri_b_test() ->
    Vocabulary = plaza_vocabulary:make([{rdf, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>},
                                        {dc, <<"http://purl.org/dc/elements/1.1/">>},
                                        {description, <<"Description">>}]),
    ?assertEqual(<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#Description">>,
                 norm(Vocabulary, uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#Description"))) .


norm_triple_test() ->
    Vocabulary = plaza_vocabulary:make([{rdf, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>},
                                        {dc, <<"http://purl.org/dc/elements/1.1/">>},
                                        {description, <<"Description">>}]),
    Triple = norm(Vocabulary, t(uri(rdf,description),
                                uri(dc, "Title"),
                                "Esto es un test",
                                uri("http://test.com/my_test_graph"))),
    ?assertEqual({<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#Description">>,
                 <<"http://purl.org/dc/elements/1.1/Title">>,
                 <<"Esto es un test">>,
                 <<"http://test.com/my_test_graph">>},
                 Triple) .


norm_literal_a_test() ->
    Vocabulary = plaza_vocabulary:make([{rdf, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>},
                                        {dc, <<"http://purl.org/dc/elements/1.1/">>},
                                        {description, <<"Description">>}]),

    L = l("this is a test"),
    ?assertEqual({literal, <<"this is a test">>, undefined, undefined}, norm(Vocabulary,L)) .


norm_literal_b_test() ->
    Vocabulary = plaza_vocabulary:make([{rdf, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>},
                                        {dc, <<"http://purl.org/dc/elements/1.1/">>},
                                        {description, <<"Description">>}]),

    L = l("this is a test", "http://www.w3.org/2001/XMLSchema#int", undefined),
    ?assertEqual({literal, <<"this is a test">>, <<"http://www.w3.org/2001/XMLSchema#int">>, undefined}, norm(Vocabulary,L)) .


norm_set_test() ->
    Vocabulary = plaza_vocabulary:make([{rdf, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>},
                                        {dc, <<"http://purl.org/dc/elements/1.1/">>},
                                        {description, <<"Description">>}]),

    TripleA = t(uri(rdf,description),
                uri(dc, "Title"),
                "Esto es un test",
                uri("http://test.com/my_test_graph")),

    TripleB = t(uri(rdf,"Hola2"),
                uri(dc, "Title2"),
                "Esto es un test2",
                uri("http://test.com/my_test_graph2")),

    Set = set([TripleA, TripleB]),

    L = norm(Vocabulary,Set),

    ?assertEqual([{<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#Description">>,
                   <<"http://purl.org/dc/elements/1.1/Title">>,
                   <<"Esto es un test">>,
                   <<"http://test.com/my_test_graph">>},
                  {<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#Hola2">>,
                   <<"http://purl.org/dc/elements/1.1/Title2">>,
                   <<"Esto es un test2">>,
                   <<"http://test.com/my_test_graph2">>}],
                 L) .
