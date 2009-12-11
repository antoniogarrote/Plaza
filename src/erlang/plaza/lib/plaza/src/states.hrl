-record(app_controller, {apps = []}) .

-record(plaza_app, { name :: string(),
                     application_module :: atom(),
                     repository_module :: atom(),
                     server_options :: term(),
                     write_tree :: term(),
                     routes :: term(),
                     webserver :: atom(),
                     environment :: atom(),
                     url_tokens :: dict(),
                     namespaces :: {dict(), dict()},
                     vocabulary :: dict() }) .

-record(triples_notification, { type :: create | update | destroy,
                                triples :: term(),
                                route :: term(),
                                is_metaresource :: boolean(),
                                resource :: term() }) .
