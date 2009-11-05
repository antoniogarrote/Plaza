%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%% This module contains parser and query engine for a
%%% subset of the SPARQL query language.
%%% The subset consists mainly of the SELECT construct without
%%% filtering the resulting triples.
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,

-module(plaza_sparql) .

-author("Antonio Garrote Hernandez") .

-include_lib("eunit/include/eunit.hrl").

-export([do_query/2, parse_sparql_query/1]) .


%% @doc
%% Sorts a set of patterns using the number of variables as the
%% criterium -> 1,2,3,0
sort_patterns(Patterns) ->
    lists:sort(fun do_sort/2,Patterns) .


%% @doc
%% Counts the number of variables in a pattern.
count_vars([], 0) -> 4 ;
count_vars([],Count) -> Count ;
count_vars([E | Es], Count) ->
    case E of
        <<"?",_R/binary>>  -> count_vars(Es, Count + 1) ;
        <<"$",_R/binary>>  -> count_vars(Es, Count + 1) ;
        _Uri               -> count_vars(Es, Count)
    end .


%% @doc
%% Sorts two patterns using the number of variables criterium.
do_sort(A, B) ->
    VarsA = count_vars(tuple_to_list(A),0),
    VarsB = count_vars(tuple_to_list(B),0),
    case A =:= 0 of
       true -> false ;
       false -> case B =:= 0 of
                   true -> true ;
                   false -> VarsA < VarsB
                end
    end .


%% @doc
%% Does a SPARQL query for a set of graph patterns and a set of triples.
%% It returns list of dictionaries with the bindings for each var in the
%% query.
do_query(QueryPatterns, Triples) ->
    PatternsP = sort_patterns(QueryPatterns),
    do_query(PatternsP, Triples, []) .
do_query(Patterns, RemainingTriples, Matches) ->
    case s_match(Patterns, RemainingTriples) of
        error               -> Matches ;
        {Bindings, Triples} -> do_query(Patterns, Triples, [Bindings | Matches])
    end .


%% @doc
%% Matches some patterns over a set of triples.
%% It returns a dict with the bindings for the vars in the patterns
%% and the list of not matched triples remaining.
s_match(Patterns, Triples) ->
    do_s_match(Patterns, Triples, dict:new()) .
do_s_match([], Triples, Bindings) ->
    {Bindings, Triples} ;
do_s_match([Pattern | Patterns], Triples, Bindings) ->
    Result = do_bind(Pattern, Triples, [], Bindings),
    case Result of
        error                 ->  error ;
        {BindingsP, TriplesP} ->  do_s_match(lists:map(fun(P) -> resolve(P,BindingsP) end,Patterns), TriplesP, BindingsP)
    end .


%% @doc
%% Binds a pattern over a set of triples.
do_bind(_Pattern, [], _NotConsumed, _Bindings) ->
    error ;
do_bind(Pattern, [Triple | Triples], NotConsumed, Bindings) ->
    case bind(Pattern, Triple, Bindings) of
        error     -> do_bind(Pattern, Triples, [Triple | NotConsumed], Bindings) ;
        BindingsP -> {BindingsP, Triples ++ NotConsumed}
    end .


%% @doc
%% Binding rules.
bind({<<"?",S/binary>>,<<"?",P/binary>>,<<"?",O/binary>>},{X,Y,Z,_G}, Bindings) ->
    BP = Bindings:append(S,X),
    BPP = BP:append(P,Y),
    BPP:append(O,Z) ;
bind({X,<<"?",P/binary>>,<<"?",O/binary>>},{X,Y,Z,_G}, Bindings) ->
    BP = Bindings:append(P,Y),
    BP:append(O,Z) ;
bind({<<"?",S/binary>>, Y, <<"?",O/binary>>},{X,Y,Z,_G}, Bindings) ->
    BP = Bindings:append(S,X),
    BP:append(O,Z) ;
bind({<<"?",S/binary>>,<<"?",P/binary>>,Z},{X,Y,Z,_G}, Bindings) ->
    BP = Bindings:append(S,X),
    BP:append(P,Y) ;
bind({X, Y,<<"?",O/binary>>},{X,Y,Z,_G}, Bindings) ->
    Bindings:append(O,Z) ;
bind({<<"?",S/binary>>, Y, Z},{X,Y,Z,_G}, Bindings) ->
    Bindings:append(S,X) ;
bind({X,<<"?",P/binary>>,Z},{X,Y,Z,_G}, Bindings) ->
    Bindings:append(P,Y) ;
bind(_P,{_X,_Y,_Z,_G}, _Bindings) ->
    error .


%% @doc
%% Resolves the variables in a triple pattern using a set of bindings.
resolve({<<"?",S/binary>> = CS,<<"?",P/binary>> = CP,<<"?",O/binary>> = CO},Bindings) ->
    case dict:find(S,Bindings) of
        {ok, [ValS]} ->
            case dict:find(P,Bindings) of
                {ok, [ValP]} ->
                    case dict:find(O,Bindings) of
                        {ok, [ValO]} -> {ValS, ValP, ValO} ;
                        error      -> {ValS, ValP, CO}
                    end ;
                error ->
                    case dict:find(O,Bindings) of
                        {ok, [ValO]} -> {ValS, CP, ValO} ;
                        error           -> {ValS, CP, CO}
                    end
            end ;
        error ->
            case dict:find(P,Bindings) of
                {ok, [ValP]} ->
                    case dict:find(O,Bindings) of
                        {ok, [ValO]} -> {CS, ValP, ValO} ;
                        error           -> {CS, ValP, CO}
                    end ;
                error ->
                    case dict:find(O,Bindings) of
                        {ok, [ValO]} -> {CS, CP, ValO} ;
                        error           -> {CS, CP, CO}
                    end
            end
    end ;

resolve({ValS,<<"?",P/binary>> = CP,<<"?",O/binary>> = CO},Bindings) ->
    case dict:find(P,Bindings) of
        {ok, [ValP]} ->
            case dict:find(O,Bindings) of
                {ok, [ValO]} -> {ValS, ValP, ValO} ;
                error           -> {ValS, ValP, CO}
            end ;
        error ->
            case dict:find(O,Bindings) of
                {ok, [ValO]} -> {ValS, CP, ValO} ;
                error           -> {ValS, CP, CO}
            end
    end ;
resolve({<<"?",S/binary>> = CS, ValP, <<"?",O/binary>> = CO},Bindings) ->
    case dict:find(S,Bindings) of
        {ok, [ValS]} ->
            case dict:find(O,Bindings) of
                {ok, [ValO]} -> {ValS, ValP, ValO} ;
                error           -> {ValS, ValP, CO}
            end ;
        error ->
            case dict:find(O,Bindings) of
                {ok, [ValO]} -> {CS, ValP, ValO} ;
                error           -> {CS, ValP, CO}
            end
    end ;
resolve({<<"?",S/binary>> = CS, <<"?",P/binary>> = CP, ValO}, Bindings) ->
    case dict:find(S,Bindings) of
        {ok, [ValS]} ->
            case dict:find(P,Bindings) of
                {ok, [ValP]} -> {ValS, ValP, ValO} ;
                error           -> {ValS, CP}
            end ;
        error ->
            case dict:find(P,Bindings) of
                {ok, [ValP]} -> {CS, ValP, ValO} ;
                error           -> {CS, CP, ValO}
            end
    end ;
resolve({ValS, ValP, <<"?",O/binary>> = CO}, Bindings) ->
    case dict:find(O,Bindings) of
        {ok, [ValO]} -> {ValS, ValP, ValO} ;
        error           -> {ValS, ValP, CO}
    end ;
resolve({<<"?",S/binary>> = CS, ValP, ValO}, Bindings) ->
    case dict:find(S,Bindings) of
        {ok, [ValS]} -> {ValS, ValP, ValO} ;
        error           -> {CS, ValP, ValO}
    end ;
resolve({ValS, <<"?",P/binary>> = CP, ValO}, Bindings) ->
    case dict:find(P,Bindings) of
        {ok, [ValP]} -> {ValS, ValP, ValO} ;
        error           -> {ValS, CP, ValO}
    end .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                        SPARQL parser                                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc
%% Main public function of the parser. It accepts a SPARQL query in a string
%% and returns a tuple with a list of variables and a list of triple patterns {S,P,O,G} as binaries or
%% error, if some error was found during the parsing process
-spec parse_sparql_query(list()) -> {[bitstring()],[{bitstring(), bitstring(), bitstring(), bitstring()}]} | error .
parse_sparql_query(Input) ->
    parser:parse(sparql_parser(), Input) .


%% @doc
%% This function returns the SPARQL parser composing top level parsers
%% [5] 	SelectQuery  ::=  'SELECT' ( 'DISTINCT' | 'REDUCED' )? ( Var+ | '*' ) DatasetClause* WhereClause SolutionModifier
sparql_parser() ->
    fun( I ) ->
            case (parser:pAnd( [
                                  parser:pString("SELECT"),
                                  fun parser:pSpaces/1,
                                  parser:pOr([parser:pString("*"),
                                              parser:pMany( fun var_parser/1 )]),
                                  fun parser:pSpaces/1,
                                  parser:pMaybe(parser:pString("WHERE")),
                                  fun parser:pSpaces/1,
                                  fun group_graph_pattern_parser/1
                                ])) ( I ) of
                {["SELECT",_,Vars,_,_W,_,Triples],[]} -> {Vars,lists:map(fun({triple,S,P,O}) -> {S,P,O,undefined} end, Triples)} ;
                Other                                 -> Other
            end
    end .

%% @doc
%% [32] TriplesSameSubject ::=  VarOrTerm PropertyListNotEmpty | TriplesNode PropertyList
triples_same_subject_parser( S ) ->
    case (parser:pOr([parser:pAnd([fun var_or_term_parser/1,
                                   fun property_list_not_empty_parser/1]),
                     parser:pAnd([fun triples_node_parser/1,
                                  parser:pMaybe(fun property_list_not_empty_parser/1)])]))( S ) of

        {[Sub,Ps],SP} -> case Sub of
                             {blank, BId, BTs} -> {lists:flatten(
                                                     lists:map(fun({predicate, P, O, Ts}) ->
                                                                       [{triple, BId, P, O} | Ts]
                                                               end,lists:flatten(Ps)) ++ BTs),
                                                   SP} ;
                             _Other            -> {lists:flatten(
                                                     lists:map(fun({predicate, P, O, Ts}) ->
                                                                       [{triple, Sub, P, O} | Ts]
                                                               end, lists:flatten(Ps))),
                                                   SP}
                         end ;
        error         -> error ;
        Other         -> Other
    end .

%% @doc
%% [38]    	TriplesNode  	  ::=    	Collection | BlankNodePropertyList
triples_node_parser( S ) ->
    (parser:pOr([fun blank_node_property_list_parser/1]))( S ) .


%% @doc
%% [39]    	BlankNodePropertyList  	  ::=    	'[' PropertyListNotEmpty ']'
blank_node_property_list_parser( S ) ->
    case (parser:pAnd([fun parser:pSpaces/1,
                       parser:pString("["),
                       fun parser:pSpaces/1,
                       fun property_list_not_empty_parser/1,
                       fun parser:pSpaces/1,
                       parser:pString("]"),
                       fun parser:pSpaces/1]))( S ) of
        error              -> error ;
        {[_,"[",_,Ps,_,"]",_], SP} -> BId = gen_blank_id(),
                                      {{blank, BId, lists:flatten(lists:map(fun({predicate, P, O, Ts}) -> [{triple, BId, P, O} | Ts] end, Ps))}, SP}
    end .


%% @doc
%% Generates a new identifier for a blank node.
gen_blank_id() ->
    list_to_binary(io_lib:format("?_~p",[trunc(random:uniform() * 100000)])) .


%% @doc
%% [74]    	VAR1  	  ::=    	'?' VARNAME
%% [75]   	VAR2 	  ::=   	'$' VARNAME
var_parser( S ) ->
    case (parser:pAnd( [parser:pOr([parser:pString("?"),
                                    parser:pString("$")]),
                        fun( Inp ) ->
                                {Res, More} = (parser:pMany( fun parser:pAlphaNum/1 ))( Inp ),
                                case Res of
                                    []  ->  fail ;
                                    _Other -> {_Sp, MoreP} = parser:pSpaces(More),
                                              {plaza_utils:to_binary(Res),MoreP}
                                end
                        end]))( S ) of
        fail            -> fail ;
        {[_S,Var], SP}  -> {Var,SP}
    end .


%% @doc
%% [20]	GroupGraphPattern  ::= 	'{' TriplesBlock? ( ( GraphPatternNotTriples | Filter ) '.'? TriplesBlock? )* '}'
group_graph_pattern_parser( S ) ->
    case (parser:pAnd([parser:pString("{"),
                       fun parser:pSpaces/1,
                       fun triples_block_parser/1,
                       fun parser:pSpaces/1,
                       parser:pString("}")]))( S ) of
        error              -> error ;
        {["{",_,Ts,_,"}"],SP}  -> {Ts, SP}
    end .


%% @doc
%% [31]	ConstructTriples  ::=  	TriplesSameSubject ( '.' ConstructTriples? )?
triples_block_parser( S ) ->
    case (parser:pAnd([fun triples_same_subject_parser/1,
                       parser:pMaybe(parser:pAnd([fun parser:pSpaces/1,
                                                  parser:pString("."),
                                                  fun parser:pSpaces/1,
                                                  fun triples_block_parser/1]))]))( S ) of
        error                      -> error ;
        {[Ts,[[_,".",_,OTs]]], SP} -> {lists:flatten(Ts ++ OTs), SP} ;
        {[Ts, OTs],SP}             -> {lists:flatten(Ts ++ OTs), SP}
    end .


%% @doc
%% Parses a var or a term. This rule is NOT in the SPARQL grammar.
var_or_term_parser( S ) ->
    case (parser:pOr( [fun var_parser/1,
                       parser:pAnd([parser:pString("<"),
                                    parser:pMany(fun parser:pAlphaNum/1 ),
                                    parser:pString(">")])
                      ] ))( S ) of
        fail             -> fail ;
        {["<", Other, ">"], Sp}  -> {list_to_binary(Other), Sp} ;
        {Var, SP}                -> {list_to_binary([<<"?">>,Var]),SP} 
    end .


%% @doc
%% [34]	PropertyList  ::=  PropertyListNotEmpty?
%% property_list_parser( S ) ->
%%     case (parser:pAnd( [fun var_or_term_parser/1,
%%                         fun( Sp ) -> case (parser:pMany(parser:pAnd([fun parser:pSpaces/1,
%%                                                                      parser:pString(","),
%%                                                                      fun parser:pSpaces/1,
%%                                                                      fun var_or_term_parser/1])))( Sp ) of
%%                                          error        -> error ;
%%                                          {Terms, Spp} -> {lists:map(fun([_,",",_,VarOrTerm]) -> VarOrTerm end, Terms), Spp}
%%                                      end
%%                         end ] ))( S ) of
%%         fail             -> fail ;
%%         {[P,Ps],Sp}       -> {[P|Ps], Sp}
%%     end .


%% @doc
%% [35]	ObjectList  ::=	Object ( ',' Object )*
object_list_parser( S ) ->
    case (parser:pAnd( [parser:pOr([fun var_or_term_parser/1,
                                    fun triples_node_parser/1]),
                        fun( Sp ) -> case (parser:pMany(parser:pAnd([fun parser:pSpaces/1,
                                                                     parser:pString(","),
                                                                     fun parser:pSpaces/1,
                                                                     parser:pOr([fun var_or_term_parser/1,
                                                                                 fun triples_node_parser/1 ])]))) ( Sp ) of
                                         error        -> error ;
                                         {Terms, Spp} -> {lists:map(fun([_,",",_,VarOrTerm]) -> VarOrTerm end, Terms), Spp}
                                     end
                        end ] ))( S ) of
        fail             -> fail ;
        {[P,Ps],Sp}      -> {lists:map(fun(Obj) -> case Obj of
                                                       {blank,BId,Ts} -> {obj, BId, Ts } ;
                                                       _Other         -> {obj, Obj, [] }
                                                   end
                                       end, lists:flatten([P|Ps])), Sp}
    end .


%% @doc
%% [33]	PropertyListNotEmpty  ::=  Verb ObjectList ( ';' ( Verb ObjectList )? )*
property_list_not_empty_parser( S ) ->
    case (parser:pAnd( [fun var_or_term_parser/1,
                        fun object_list_parser/1,
                        parser:pMany(parser:pAnd([fun parser:pSpaces/1,
                                                  parser:pString(";"),
                                                  fun parser:pSpaces/1,
                                                  parser:pMaybe(parser:pAnd([fun var_or_term_parser/1,
                                                                             fun object_list_parser/1]))]))
                       ]))( S ) of
        fail   -> fail ;
        {[P,[],Triples],Sp} ->  {lists:map(fun({obj, O, Ts}) -> {predicate, P, O, Ts} end, Triples), Sp};
        {[P,O,Ps],Sp}       ->  {lists:flatten(
                                   lists:map(fun([Pr,Triples]) ->
                                                     lists:map(fun({obj, Ob, Ts}) -> {predicate, Pr, Ob, Ts} end,
                                                               Triples) end,
                                             [[P,O]|lists:map(fun([_,";",_,[[IP,IObjs]]]) -> [IP, IObjs] end, Ps)])), Sp}
    end .


%% Tests


sort_patterns_test() ->
    Tuples = [{<<"?x">>, <<"a">>, <<"?x">>},
              {<<"a">>, <<"a">>, <<"a">>},
              {<<"?x">>, <<"?x">>, <<"?x">>},
              {<<"a">>, <<"?x">>, <<"?x">>},
              {<<"a">>, <<"?x">>, <<"a">>} ],
    Sorted = sort_patterns(Tuples),
    ?assertEqual(Sorted,
                 [{<<"a">>, <<"?x">>, <<"a">>},
                  {<<"a">>, <<"?x">>, <<"?x">>},
                  {<<"?x">>, <<"a">>, <<"?x">>},
                  {<<"?x">>, <<"?x">>, <<"?x">>},
                  {<<"a">>, <<"a">>, <<"a">>}]) .

do_query_a_test() ->
    Query = [{<<"http://test.com/a">>, <<"http://test.com/b">>, <<"?p">>}],
    Vocabulary = [{test, "http://test.com/"}],
    Triples = plaza_triples:norm( plaza_vocabulary:make(Vocabulary),
                                  plaza_triples:set( [ plaza_triples:t(plaza_triples:uri(test,"a"), plaza_triples:uri(test,"b"), plaza_triples:uri(test,"c"), plaza_triples:uri(test, "g")),
                                                       plaza_triples:t(plaza_triples:uri(test,"a"), plaza_triples:uri(test,"b"), plaza_triples:uri(test,"d"), plaza_triples:uri(test, "g")),
                                                       plaza_triples:t(plaza_triples:uri(test,"a"), plaza_triples:uri(test,"c"), plaza_triples:uri(test,"e"), plaza_triples:uri(test, "g")),
                                                       plaza_triples:t(plaza_triples:uri(test,"a"), plaza_triples:uri(test,"b"), plaza_triples:uri(test,"f"), plaza_triples:uri(test, "g")) ]) ),
    Res = do_query(Query, Triples),
    Bindings = lists:flatten(lists:map(fun(R) -> dict:fetch(<<"p">>, R) end, Res)),
    ?assertEqual(3,length(Res)),
    ?assertEqual([<<"http://test.com/f">>,
                  <<"http://test.com/d">>,
                  <<"http://test.com/c">>],
                 Bindings) .


do_query_b_test() ->
    Query = [{<<"http://test.com/a">>, <<"http://test.com/b">>, <<"?p">>},
             {<<"?p">>, <<"?j">>, <<"?k">>}],
    Vocabulary = [{test, "http://test.com/"}],
    Triples = plaza_triples:norm( plaza_vocabulary:make(Vocabulary),
                                  plaza_triples:set( [ plaza_triples:t(plaza_triples:uri(test,"a"), plaza_triples:uri(test,"b"), plaza_triples:uri(test,"c"), plaza_triples:uri(test, "g")),
                                                       plaza_triples:t(plaza_triples:uri(test,"c"), plaza_triples:uri(test,"b"), plaza_triples:uri(test,"d"), plaza_triples:uri(test, "g")),
                                                       plaza_triples:t(plaza_triples:uri(test,"a"), plaza_triples:uri(test,"b"), plaza_triples:uri(test,"e"), plaza_triples:uri(test, "g")),
                                                       plaza_triples:t(plaza_triples:uri(test,"e"), plaza_triples:uri(test,"b"), plaza_triples:uri(test,"f"), plaza_triples:uri(test, "g")) ]) ),
    Res = do_query(Query, Triples),
    ?assertEqual(2,length(Res)),
    Keys1 = dict:fetch_keys(lists:nth(1,Res)),
    ?assertEqual([<<"k">>,<<"p">>,<<"j">>],Keys1),
    ?assertEqual({ok,[<<"http://test.com/f">>]},
                 dict:find(<<"k">>,lists:nth(1,Res))),
    ?assertEqual({ok,[<<"http://test.com/e">>]},
                 dict:find(<<"p">>,lists:nth(1,Res))),
    ?assertEqual({ok,[<<"http://test.com/b">>]},
                 dict:find(<<"j">>,lists:nth(1,Res))),
    Keys2 = dict:fetch_keys(lists:nth(2,Res)),
    ?assertEqual([<<"k">>,<<"p">>,<<"j">>],Keys2),
    ?assertEqual({ok,[<<"http://test.com/d">>]},
                 dict:find(<<"k">>,lists:nth(2,Res))),
    ?assertEqual({ok,[<<"http://test.com/c">>]},
                 dict:find(<<"p">>,lists:nth(2,Res))),
    ?assertEqual({ok,[<<"http://test.com/b">>]},
                 dict:find(<<"j">>,lists:nth(2,Res))) .


select_sparql_1_test() ->
    Res = plaza_sparql:parse_sparql_query("SELECT * WHERE { ?s ?p ?o . ?x ?U ?Z}"),
    ?assertEqual({"*",
                  [{<<"?s">>,<<"?p">>,<<"?o">>,undefined},
                   {<<"?x">>,<<"?U">>,<<"?Z">>,undefined}]},
                Res ) .
