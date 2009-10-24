-module(plaza_formaters) .

-author("Antonio Garrote Hernandez") .

-include_lib("eunit/include/eunit.hrl").
-include_lib("http_records.hrl").
-include_lib("states.hrl").

-export([format/4, rdf_formatter/2]) .


%% Public API


format([{Type,Subtype} | Formats], Response, Application, Resource) ->
    Triples = plaza_triples:norm(Application#plaza_app.vocabulary,Response#response.body),
    case {Type,Subtype} of
        {"*","*"}        ->  {{Type, Subtype}, rdf_formatter(xml,Triples)} ; % by default we return RDF/XML
        {_Type,"xml"}    ->  {{Type, Subtype}, rdf_formatter(xml,Triples)} ;
        {_Type,_Subtype} ->  format(Formats, Response, Application, Resource)
    end .


rdf_formatter(xml, Triples) ->
    Stances = lists:map(fun({S,P,O,_C}) ->
                                case O of
                                    <<"http://",_Rest/binary>> -> [<<"<rdf:Description about=\"">>,S,<<"\"><">>, P, <<"><rdf:Description about=\"">>,O,<<"\"/></">>,P,<<"></rdf:Description>">>] ;
                                    {L,undefined,_L}           -> [<<"<rdf:Description about=\"">>,S,<<"\"><">>, P, <<">">>,L,<<"</">>,P,<<"></rdf:Description>">>] ;
                                    {L,T,_L}                   -> [<<"<rdf:Description about=\"">>,S,<<"\"><">>, P, <<" rdf:datatype=\"">>, T,<<"\">">>,L,<<"</">>,P,<<"></rdf:Description>">>] ;
                                    _Other                     -> [<<"<rdf:Description about=\"">>,S,<<"\"><">>, P, <<">">>,O,<<"</">>,P,<<"></rdf:Description>">>]
                                end
                        end,
                        Triples),
    Preamble = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">">>,
    Conclusion = <<"</rdf:RDF>">>,
    list_to_bitstring([[Preamble | Stances],Conclusion]) .


%% Tests


rdf_formatter_test() ->
    Triples = [{<<"http://test.com/a">>, <<"http://test.com/b">>,<<"http://test.com/c">>,undefined},
               {<<"http://test.com/d">>, <<"http://test.com/e">>,<<"http://test.com/f">>,<<"http://test.com/g">>}],
    Result = rdf_formatter(xml,Triples),
    io:format("Obtained:~p~n",[Result]),
    ?assertEqual(Result,
                 <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"><rdf:Description about=\"http://test.com/a\"><http://test.com/b><rdf:Description about=\"http://test.com/c\"/></http://test.com/b></rdf:Description><rdf:Description about=\"http://test.com/d\"><http://test.com/e><rdf:Description about=\"http://test.com/f\"/></http://test.com/e></rdf:Description></rdf:RDF>">>) .
