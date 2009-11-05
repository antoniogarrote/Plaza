-record(app_controller, {apps = []}) .

-record(plaza_app, { name :: string(),
                     application_module :: atom(),
                     repository_module :: atom(),
                     server_options :: term(),
                     routes :: term(),
                     webserver :: atom(),
                     environment :: atom(),
                     namespaces :: {dict(), dict()},
                     vocabulary :: dict()}) .
