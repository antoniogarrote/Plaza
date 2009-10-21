%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(plaza_core_ontology) .

-author("Antonio Garrote Hernandez") .


-include_lib("eunit/include/eunit.hrl").


-export([vocabulary/0]) .


vocabulary() ->
    plaza_vocabulary:make([{rdf,<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>}]) .
