-module(demo_application) .

-author("Antonio Garrote Hernandez") .

-include_lib("../plaza/lib/plaza/src/http_records.hrl").

-export([repository_module/0, vocabulary_module/0, server_configuration/0, routes/0, domain/0]) .
-export([expose_request/4]) .


%% Configuration of the application


repository_module() -> repository .

vocabulary_module() -> vocabulary .

server_configuration() ->
    [{port, 7777}] .

domain() ->
    "localhost:7777" .

routes() ->
    [{"/expose",{demo_application, expose_request}}] .


%% handlers


expose_request(Request, Response, _State, _Application) ->
    Method = Request#request.method,
    Path = Request#request.path,
    Headers = Request#request.headers,
    Parameters = Request#request.parameters,
    Response#response{ code = 200,
                       headers = [{"Content-type", "plain/text"}],
                       body = lists:flatten(io_lib:format("Method:~p~nPath:~p~nHeaders:~p~nParameters:~p~n",[Method,Path,Headers,Parameters])) } .
