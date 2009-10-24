-module(blog_resource) .

-author("Antonio Garrote Hernandez") .

-import(demo_application, [domain/0]) .



triple_space() -> "http://" ++ domain() ++ "/Books" .

uri() -> "http://" ++ domain() ++ "/Books/:id" .

is_metaresource() -> false .

operations() -> ['GET'] .


%% GET request : lifting -> operation -> lowering

lifting('GET', Request, Response, Context, Application) ->
    plaza_web:lifting('GET', Request, Response, Context, Application, ?MODULE) .


operation('GET', Request, Response, Context, Application) ->
    plaza_web:operation('GET', Request, Response, Context, Application, ?MODULE) .


lowering('GET', Request, Response, Context, Application) ->
    plaza_web:lowering('GET', Request, Response, Context, Application, ?MODULE) .
