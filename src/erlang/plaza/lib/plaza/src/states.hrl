-record(app_controller, {apps = [],
                         servers = []}) .

-record(plaza_app, { name :: string(),
                     application_module :: atom(),
                     repository_module :: atom(),
                     server_options :: term(),
                     environment :: atom(),
                     vocabulary :: dict()}) .
