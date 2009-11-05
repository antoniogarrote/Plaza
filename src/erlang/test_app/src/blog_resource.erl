-module(blog_resource) .

-author("Antonio Garrote Hernandez") .

-import(demo_application, [domain/0]) .


-export([triple_space/0, uri/0, is_metaresource/0, operations/0, lifting/5, operation/5, lowering/5]) .
-export([resources_tree/0, metaresource/0]) .


triple_space() -> "http://" ++ domain() ++ "/Blogs" .

uri() -> "http://" ++ domain() ++ "/Blogs/:id" .

is_metaresource() -> false .

metaresource() -> blogs .


%% GET request : lifting -> operation -> lowering

lifting('GET', Request, Response, Context, Application) ->
    plaza_web:lifting('GET', Request, Response, Context, Application, ?MODULE) .


operation('GET', Request, Response, Context, Application) ->
    plaza_web:operation('GET', Request, Response, Context, Application, ?MODULE) .


lowering('GET', Request, Response, Context, Application) ->
    plaza_web:lowering('GET', Request, Response, Context, Application, ?MODULE) .
