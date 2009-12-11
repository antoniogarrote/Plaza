-module(plaza_formaters) .

-author("Antonio Garrote Hernandez") .

-include_lib("eunit/include/eunit.hrl").
-include_lib("http_records.hrl").
-include_lib("states.hrl").

-export([format/4, format/5, rdf_formatter/3]) .


%% Public API


format(Format, Vocabulary, Namespaces, Set) ->
    rdf_formatter(Format,plaza_triples:norm(Vocabulary,Set), Namespaces) .

format([{Type,Subtype} | Formats], Response, Vocabulary, {NsDirect, _NsInverse} = Namespaces, Resource) ->
    error_logger:info_msg("Ho let's go! ~p",[{Type,Subtype}]),
    Triples = plaza_triples:norm(plaza_vocabulary:merge([Vocabulary,NsDirect]),Response#response.body),
    case {Type,Subtype} of
        {"*","*"}            ->  {{Type, Subtype}, rdf_formatter(xml,Triples, Namespaces)} ; % by default we return RDF/XML
        {_Type,"xml"}        ->  {{Type, Subtype}, rdf_formatter(xml,Triples, Namespaces)} ;
        {_Type,"json"}       ->  {{Type, Subtype}, rdf_formatter(json,Triples, Namespaces)} ;
        {_Type,"javascript"} ->  {{Type, Subtype}, rdf_formatter(json,Triples, Namespaces)} ;
        {_Type,_Subtype}     ->  format(Formats, Response, Vocabulary, Namespaces, Resource)
    end .


%% private functions


%% RDF/XML formatter
rdf_formatter(xml, Triples, Ns) ->
    {UpdatedTriples, UsedNs} = plaza_namespaces:update_triples(Triples,Ns),
    Stances = lists:map(fun({S,P,O,_C}) ->
                                case O of
                                    <<"http://",_Rest/binary>>  -> [<<"<rdf:Description rdf:about=\"">>,S,<<"\"><">>, P, <<"><rdf:Description rdf:about=\"">>,O,<<"\"/></">>,P,<<"></rdf:Description>">>] ;
                                    {literal,L,undefined,_L}    -> [<<"<rdf:Description rdf:about=\"">>,S,<<"\"><">>, P, <<">">>,L,<<"</">>,P,<<"></rdf:Description>">>] ;
                                    {literal,L,T,_L}            -> [<<"<rdf:Description rdf:about=\"">>,S,<<"\"><">>, P, <<" rdf:datatype=\"">>, T,<<"\">">>,L,<<"</">>,P,<<"></rdf:Description>">>] ;
                                    _Other                      -> [<<"<rdf:Description rdf:about=\"">>,S,<<"\"><">>, P, <<">">>,O,<<"</">>,P,<<"></rdf:Description>">>]
                                end
                        end,
                        UpdatedTriples),
    NsBytes = lists:foldl(fun(N,Acum) -> [ <<"xmlns:">>, plaza_utils:to_binary(N) , <<"=\"">>, plaza_namespaces:resolve_prefix(Ns,N), <<"\" ">> ] ++ Acum end,
                          [],
                         lists:filter(fun(UNs) -> UNs =/= rdf end, UsedNs)),
    %Preamble = [<<"<?xml version=\"1.0\" encoding=\"utf-8\"?><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" ">>| NsBytes] ++ [ <<">">>],
    Preamble = [<<"<?xml version=\"1.0\" encoding=\"utf-8\"?><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" ">>| NsBytes] ++ [ <<">">>],
    Conclusion = <<"</rdf:RDF>">>,
    list_to_bitstring([[Preamble | Stances],Conclusion]) ;

%% "all things javascript" formatter
rdf_formatter(json, Triples, Ns) ->

    {UpdatedTriples, _UsedNs} = plaza_namespaces:update_triples(Triples,Ns),

    HashedTriples = hash_triples(UpdatedTriples, dict:new()),
    TriplesJson = dict_to_json(dict:fetch_keys(HashedTriples), HashedTriples, []),
    list_to_bitstring(TriplesJson) .


%% Auxiliary functions


%% @doc
%% Introduce a set of triples into a dict using the Subject of the triple
%% as the key in the dict.
hash_triples([], D) -> D ;
hash_triples([{S,P,O,_C} | Ts], D) ->
    hash_triples(Ts,dict:append(S, {S,P,O}, D)) .


dict_to_json([], _Dict, Acum) ->
    ObjP = lists:nthtail(1, lists:reverse(lists:foldl(fun(Bs, A) -> [Bs | [<<",">> | A]] end, [], Acum))),
    [<<"{">>, ObjP, <<"}">>] ;
dict_to_json([K | Ks], Dict, Acum) ->
    Obj = list_to_json(dict:fetch(K, Dict), []),
    ObjP = lists:nthtail(1, lists:reverse(lists:foldl(fun(Bs, A) -> [Bs | [<<",">> | A]] end, [], Obj))),
    AcumP = [list_to_bitstring([<<"'">>, K, <<"': {">>, ObjP, <<"}">>]) | Acum] ,
    dict_to_json(Ks, Dict, AcumP) .

list_to_json([], Acum) -> Acum ;
list_to_json([{_S, P, O} | Ts], Acum) ->
    Prop = case O of
               <<"http://",_Rest/binary>> -> [<<"'">>, P, <<"':'">>, O, <<"'">>] ;
               {literal,L,_T,_L}          -> [<<"'">>, P, <<"':'">>, L, <<"'">>] ;
               _Other                     -> [<<"'">>, P, <<"':'">>, O, <<"'">>]
           end,
    list_to_json(Ts,[list_to_bitstring(Prop) | Acum]) .


%% Tests


rdf_formatter_test() ->
    Triples = [{<<"http://test.com/a">>, <<"http://test.com/b">>,<<"http://test.com/c">>,undefined},
               {<<"http://test.com/d">>, <<"http://test.com/e">>,<<"http://test.com/f">>,<<"http://test.com/g">>}],
    Namespaces = plaza_namespaces:make([{test, "http://test.com/"}, {other, "http://other.com"}]),
    Result = rdf_formatter(xml,Triples, Namespaces),
    ?assertEqual(Result,
                 <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:test=\"http://test.com/\" ><rdf:Description rdf:about=\"http://test.com/d\"><test:e><rdf:Description rdf:about=\"http://test.com/f\"/></test:e></rdf:Description><rdf:Description rdf:about=\"http://test.com/a\"><test:b><rdf:Description rdf:about=\"http://test.com/c\"/></test:b></rdf:Description></rdf:RDF>">>) .

json_formatter_test() ->
    Triples = [{<<"http://test.com/a">>, <<"http://test.com/b">>,<<"http://test.com/c">>,undefined},
               {<<"http://test.com/d">>, <<"http://test.com/e">>,<<"http://test.com/f">>,<<"http://test.com/g">>}],
    Namespaces = plaza_namespaces:make([{test, "http://test.com/"}, {other, "http://other.com"}]),
    Result = rdf_formatter(json,Triples, Namespaces),
    ?assertEqual(Result,
                 <<"{'http://test.com/d': {'test:e'='http://test.com/f'},'http://test.com/a': {'test:b'='http://test.com/c'}}">>) .
