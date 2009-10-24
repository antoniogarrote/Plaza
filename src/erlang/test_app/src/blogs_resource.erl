-module(blogs_resource) .

-author("Antonio Garrote Hernandez") .

-import(demo_application, [domain/0]) .



triple_space() -> "http://" ++ domain() ++ "/Blogs" .

uri() -> "http://" ++ domain() ++ "/Blogs" .

is_metaresource() -> true .

operations() -> ['POST'] .


generate_instance_uri(Request, Response, Context, Application) ->
    list_to_binary(uri() ++ "/" ++uuid:string()) .


%% POST request : lifting -> operation -> lowering

lifting('POST', Request, Response, Context, Application) ->
    plaza_web:lifting('POST', Request, Response, Context, Application, ?MODULE) .


operation('POST', Request, Response, Context, Application) ->
    plaza_web:operation('POST', Request, Response, Context, Application, ?MODULE) .


lowering('POST', Request, Response, Context, Application) ->
    plaza_web:lowering('POST', Request, Response, Context, Application, ?MODULE) .
