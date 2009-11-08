%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2009 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(plaza_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, cmd_app_start/0, cmd_console_start/0, show_help/0, show_version/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case plaza_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% command line options start of the application
%%%===================================================================


%% Starts the server at the given Host and Port.
cmd_app_start() ->
    application:start(plaza),
    ParsedArguments = cmd_line_options(default_values(),[app_module, app_name, env]),
    gen_server:call(plaza_applications_controller, {start_plaza_application, ParsedArguments}) .


cmd_console_start() ->
    error_logger:info_msg("Starting with console...",[]),
    application:start(plaza),
    ParsedArguments = cmd_line_options(default_values(),[app_module, app_name, env]),
    console:start_link(ParsedArguments) .


%% Default values for the application
default_values() ->
    [{env, "development"}] .


%% Version of the server.
version() ->
    application:load(plaza),
    {ok,Vsn} = application:get_key(plaza,vsn),
    Vsn .


%% @doc
%% Parses command line arguments.
cmd_line_options(ApplicationArguments,Arguments) ->
    Options = command_line_options(Arguments, []),
    default_values(ApplicationArguments, Options) .

command_line_options([], Acum) -> Acum ;
command_line_options([Flag | T], Acum) ->
    Arg = init:get_argument(Flag),
    case Arg of
        {ok,[[Value]]} ->
            command_line_options(T,process_value(Flag, Value, Acum)) ;
        error ->
            command_line_options(T,Acum)
    end .


default_values([], Acum) ->
    Acum ;
default_values([{Default, DefaultValue} | Defaults], Acum) ->
    case proplists:get_value(Default, Acum) of
        undefined ->  default_values(Defaults, [{Default, DefaultValue} | Acum]) ;
        _Other    ->  default_values(Defaults, Acum)
    end .


process_value(Flag, Value, Acum) ->
    case Flag of
        app_name    -> [{name, Value} | Acum] ;
        app_module  -> [{app, Value} | Acum] ;
        env         -> [{env, Value} | Acum]
    end .

show_version() ->
    io:format("plaza version ~s~n", [version()]) .

show_help() ->
    io:format("plaza use: plaza -app_name application_name -app_module application_module -pa path_to_app_beam_files -sname node_name [-env environment] ~n", []) .

%%%===================================================================
%%% Internal functions
%%%===================================================================


