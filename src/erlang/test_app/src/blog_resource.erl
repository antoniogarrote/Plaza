-module(blog_resource) .

-author("Antonio Garrote Hernandez") .

-import(demo_application, [domain/0]) .


-export([triple_space/0, uri/0, is_metaresource/0, operations/0, lifting/5, operation/5, lowering/5]) .
-export([read_tree/0, metaresource/0, url_token/0]) .


triple_space() -> "http://" ++ domain() ++ "/blogs" .

uri() -> "http://" ++ domain() ++ "/blogs/:id" .

is_metaresource() -> false .

read_tree() -> [] .

metaresource() -> blogs_resource .

url_token() -> blogs .

operations() -> ['GET'] .


%% GET request : lifting -> operation -> lowering

lifting('GET', Request, Response, Context, Application) ->
    plaza_web:lifting('GET', Request, Response, Context, Application, ?MODULE) ;
lifting('PUT', Request, Response, Context, Application) ->
    plaza_web:lifting('PUT', Request, Response, Context, Application, ?MODULE) .


operation('GET', Request, Response, Context, Application) ->
    plaza_web:operation('GET', Request, Response, Context, Application, ?MODULE) ;
operation('PUT', Request, Response, Context, Application) ->
    plaza_web:operation('PUT', Request, Response, Context, Application, ?MODULE) .


lowering('GET', Request, Response, Context, Application) ->
    plaza_web:lowering('GET', Request, Response, Context, Application, ?MODULE) ;
lowering('PUT', Request, Response, Context, Application) ->
    plaza_web:lowering('PUT', Request, Response, Context, Application, ?MODULE) .
