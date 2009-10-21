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

-export([uri/2, uri/1, triple/3, triple/4, t/3, t/4, norm/2]).


%% Public API


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
%% Transform triples into a normalized representation
norm(Vocabulary, #triple{} = T) ->
    {norm(Vocabulary, T#triple.subject),
     norm(Vocabulary, T#triple.predicate),
     norm(Vocabulary, T#triple.object),
     norm(Vocabulary, T#triple.context)} ;
norm(Vocabulary, {Ns, Id}) ->
    plaza_utils:to_binary([norm(Vocabulary,Ns), norm(Vocabulary,Id)]) ;
norm(Vocabulary, {Uri}) ->
    plaza_utils:to_binary(norm(Vocabulary,Uri)) ;
norm(Vocabulary, T) when is_atom(T) ->
    plaza_vocabulary:resolve(Vocabulary, T) ;
norm(_Vocabulary, T) ->
    plaza_utils:to_binary(T) .


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
