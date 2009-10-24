-record(request, { method :: atom(),
                   path :: string(),
                   headers,
                   raw :: term(),
                   parameters = [] :: [{string(), string()}] }) .

-record(response, { code :: integer(),
                    headers :: [{string(), string()}],
                    body :: binary() }) .

