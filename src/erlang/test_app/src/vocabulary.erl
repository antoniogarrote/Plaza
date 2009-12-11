-module(vocabulary) .

-author("Antonio Garrote Hernandez") .

-export([namespaces/0, vocabulary/0]) .

namespaces() ->
    blogs:namespaces() .

vocabulary() ->
    [ [{test_app, {ns, <<"http://plaza-framework.org/test_app#">>}}],
      %% Subset of the Dublin Core ontology
      blogs:vocabulary() ] .
