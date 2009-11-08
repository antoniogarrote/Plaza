-module(blogs_resource) .

-author("Antonio Garrote Hernandez") .

-import(demo_application, [domain/0]) .


-export([namespaces/0, vocabulary/0, generate_instance_uri/4, triple_space/0, uri/0, is_metaresource/0, operations/0, lifting/5, operation/5, lowering/5]) .
-export([read_tree/0, url_token/0]) .


triple_space() -> "http://" ++ domain() ++ "/blogs" .

uri() -> "http://" ++ domain() ++ "/blogs" .

is_metaresource() -> true .

read_tree() -> [] .

operations() -> ['GET', 'POST'] .

url_token() -> blogs .

generate_instance_uri(_Request, _Response, _Context, _Application) ->
    list_to_binary(uri() ++ "/" ++uuid:string()) .


%% Ontology


namespaces() ->
    [{dc, <<"http://purl.org/dc/elements/1.1/">>}] .

vocabulary() ->
    [ {title, <<"http://purl.org/dc/elements/1.1/Title">>},
      {creator, <<"http://purl.org/dc/elements/1.1/Creator">>} ] .


%% request : lifting -> operation -> lowering

lifting('POST', Request, Response, Context, Application) ->
    plaza_web:lifting('POST', Request, Response, Context, Application, ?MODULE) ;
lifting('GET', Request, Response, Context, Application) ->
    plaza_web:lifting('GET', Request, Response, Context, Application, ?MODULE) .


operation('POST', Request, Response, Context, Application) ->
    plaza_web:operation('POST', Request, Response, Context, Application, ?MODULE) ;
operation('GET', Request, Response, Context, Application) ->
    plaza_web:operation('GET', Request, Response, Context, Application, ?MODULE) .


lowering('POST', Request, Response, Context, Application) ->
    plaza_web:lowering('POST', Request, Response, Context, Application, ?MODULE) ;
lowering('GET', Request, Response, Context, Application) ->
    plaza_web:lowering('GET', Request, Response, Context, Application, ?MODULE) .
