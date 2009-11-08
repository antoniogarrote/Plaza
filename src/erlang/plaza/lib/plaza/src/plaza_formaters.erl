-module(plaza_formaters) .

-author("Antonio Garrote Hernandez") .

-include_lib("eunit/include/eunit.hrl").
-include_lib("http_records.hrl").
-include_lib("states.hrl").

-export([format/4, format/5, rdf_formatter/3]) .


%% Public API


format(Format, Vocabulary, Namespaces, Set) ->
    rdf_formatter(Format,plaza_triples:norm(Vocabulary,Set), Namespaces) .

format([{Type,Subtype} | Formats], Response, Vocabulary, Namespaces, Resource) ->
    Triples = plaza_triples:norm(Vocabulary,Response#response.body),
    case {Type,Subtype} of
        {"*","*"}        ->  {{Type, Subtype}, rdf_formatter(xml,Triples, Namespaces)} ; % by default we return RDF/XML
        {_Type,"xml"}    ->  {{Type, Subtype}, rdf_formatter(xml,Triples, Namespaces)} ;
        {_Type,_Subtype} ->  format(Formats, Response, Vocabulary, Namespaces, Resource)
    end .


%% private functions


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
    list_to_bitstring([[Preamble | Stances],Conclusion]) .


%% Tests


rdf_formatter_test() ->
    Triples = [{<<"http://test.com/a">>, <<"http://test.com/b">>,<<"http://test.com/c">>,undefined},
               {<<"http://test.com/d">>, <<"http://test.com/e">>,<<"http://test.com/f">>,<<"http://test.com/g">>}],
    Namespaces = plaza_namespaces:make([{test, "http://test.com/"}, {other, "http://other.com"}]),
    Result = rdf_formatter(xml,Triples, Namespaces),
    io:format("Obtained:~p~n",[Result]),
    ?assertEqual(Result,
                 <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:test=\"http://test.com/\" ><rdf:Description rdf:about=\"http://test.com/d\"><test:e><rdf:Description rdf:about=\"http://test.com/f\"/></test:e></rdf:Description><rdf:Description rdf:about=\"http://test.com/a\"><test:b><rdf:Description rdf:about=\"http://test.com/c\"/></test:b></rdf:Description></rdf:RDF>">>) .
