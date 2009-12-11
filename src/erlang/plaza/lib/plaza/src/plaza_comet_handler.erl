-module(plaza_comet_handler) .

-author("Antonio Garrote Hernandez") .

-behaviour(gen_server) .

-include_lib("rabbit_states.hrl").
-include_lib("states.hrl").

-export([start_link/4, notify_triple_changes/5]) .
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% @doc
%% Starts the backend
start_link(Type, Pid, Combinators, [Method, Request, Response, State, Application, Resource]) ->
    error_logger:info_msg("starting comet handler link",[]),
    Ref = list_to_atom(lists:flatten(io_lib:format("comet_handler:~p",[make_ref()]))),
    CometRequest = #comet_request{ type = Type,
                                   method = Method,
                                   request = Request,
                                   response = Response,
                                   state = State,
                                   application = Application,
                                   resource = Resource,
                                   combinators = Combinators,
                                   ref = Ref,
                                   request_handler = Pid },
    gen_server:start_link({local, Ref}, plaza_comet_handler, [CometRequest], []) .


%% @doc
%% Publish changes to a triple space through RabbitMQ
notify_triple_changes(Route, Triples, Request, Application, Resource) ->
    TripleSpaces = notification_handlers(Request, Application, Resource),
    SafeTripleSpaces = lists:map(fun(TS) ->
                                         plaza_utils:to_binary(TS)
                                 end, TripleSpaces),
    Notification = #triples_notification{ type = update,
                                          route = Route,
                                          triples = Triples,
                                          is_metaresource = Resource:is_metaresource(),
                                          resource = Resource },
    plaza_rabbit_backend:publish(Notification, SafeTripleSpaces) .


%% callbacks


%% @todo It is necessary to check if the connection in the push request is broken.
%% @todo I should deal somehow with the destruction of the resource's triple space.
init([#comet_request{ resource = Resource, request= Request, application = Application } = CometRequest]) ->
    Callback = fun(Message) ->
                       error_logger:info_msg("Callback comet handler, received some triples with count ~p, notifying handler",[Message#triples_msg.counter]),
                       gen_server:call(CometRequest#comet_request.ref, {notification, Message})
               end,
    TripleSpaces = subscription_handlers(Request, Application, Resource),
    SafeTripleSpaces = lists:map(fun(TS) ->
                                         plaza_utils:to_binary(TS)
                                 end, TripleSpaces),
    plaza_rabbit_backend:consume(Callback, SafeTripleSpaces),
    { ok, CometRequest#comet_request{ triple_spaces = TripleSpaces } } .

%% @ErrorResult deregister _From from the queue
handle_call({notification, Message}, From, CometRequest) ->
    Notification = Message#triples_msg.content,
    case Message#triples_msg.counter > CometRequest#comet_request.counter of
        false ->
            {reply, ok, CometRequest} ; % we have already processed this message in any other triple space
        true  ->
            case CometRequest#comet_request.type of
                push  -> % a client is subscribed to this triple space
                    case Notification#triples_notification.route =:= plaza_web:state_get(route,CometRequest#comet_request.state) of
                        true  -> process_push_notification(Message, Notification, From, CometRequest) ;
                        false -> {reply, ok, CometRequest#comet_request{ counter = CometRequest#comet_request.counter }}  % not our business
                    end ;
                block -> % a client is blocked waiting for some data
                    case (CometRequest#comet_request.resource):is_metaresource() of
                        true  -> process_notification(Message, Notification, From, CometRequest) ;
                        false -> case (Notification#triples_notification.route =:= plaza_web:state_get(route,CometRequest#comet_request.state) or
                                       lists:any(fun(R) -> R =:= Notification#triples_notification.resource end,CometRequest#comet_request.triple_spaces))
                                     of
                                     true  -> process_notification(Message, Notification, From, CometRequest) ;
                                     false -> {reply, ok, CometRequest#comet_request{ counter = CometRequest#comet_request.counter }}  % not our business
                                 end
                    end
            end
    end .

%% dummy callbacks so no warning are shown at compile time

handle_cast(_Msg, State) ->
    {noreply, State} .

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State} .


%% private functions


notification_handlers(_Request, Application, Resource) ->
    Base = case Resource:is_metaresource() of
               true  ->  Resource ;
               false ->  Resource:metaresource()
           end,
    error_logger:info_msg("Extracting base handlers from ~p for ~p",[Application#plaza_app.write_tree, Base]),
    BaseHandlers = plaza_ts_trees:path(Base, Application#plaza_app.write_tree),
    case Resource:is_metaresource() of
        true  -> [Base | BaseHandlers] ;
        false -> [{instance, Resource:metaresource()} | [Base | BaseHandlers]]
    end .


subscription_handlers(_Request, Application, Resource) ->
    Base = case Resource:is_metaresource() of
               true  ->  Resource ;
               false ->  Resource:metaresource()
           end,
    BaseHandlers = plaza_ts_trees:path(Base, Application#plaza_app.write_tree) ++ Base:read_tree(),
    case Resource:is_metaresource() of
        true  -> [Base | BaseHandlers] ;
        false -> [{instance, Resource:metaresource()} | BaseHandlers]
    end .


%% @todo Partial matching on Notification triples to avoid extra triple query
process_push_notification(Message, Notification, _From, CometRequest) ->
    error_logger:info_msg("Comet push handler, received triples from calllback ~p",[Message]),
    StateP  = plaza_web:state_update(xblocking, false, CometRequest#comet_request.state),
    StatePP = plaza_web:state_update(rdf_triples, Notification#triples_notification.triples, StateP),
    error_logger:info_msg("Enough triples back to push handler ~p",[CometRequest#comet_request.request_handler]),
    error_logger:info_msg("About to combine ~p",[CometRequest#comet_request.combinators]),
    Output = plaza_web:combine(CometRequest#comet_request.method,
                               CometRequest#comet_request.request,
                               CometRequest#comet_request.response,
                               StatePP,
                               CometRequest#comet_request.application,
                               CometRequest#comet_request.resource,
                               CometRequest#comet_request.combinators),
    error_logger:info_msg("I'm sending to handler the output ~p", [Output]),
    CometRequest#comet_request.request_handler ! Output,
    {reply, ok, CometRequest#comet_request{ counter = Message#triples_msg.counter }} .


%% @todo Partial matching on Notification triples to avoid extra triple query
process_notification(Message, _Notification, _From, CometRequest) ->
    StateP = plaza_web:state_update(xblocking, false, CometRequest#comet_request.state),
    Result = plaza_web:query_sparql_repository(CometRequest#comet_request.method,
                                               CometRequest#comet_request.request,
                                               CometRequest#comet_request.response,
                                               StateP,
                                               CometRequest#comet_request.application,
                                               CometRequest#comet_request.resource),
    error_logger:info_msg("Comet handler, received triples from calllback ~p",[Result]),
    case Result of
        {ok, [_Method, _Request, _Response, State, _Application, _Resource]} ->
            error_logger:info_msg("Comet handler, query result ok",[]),
            Triples = plaza_web:state_get(rdf_triples, State),
            error_logger:info_msg("Comet handler, the triples ~p",[Triples]),
            error_logger:info_msg("Comet handler, the triples length ~p",[length(gb_sets:to_list(Triples))]),
            case length(gb_sets:to_list(Triples)) > 0 of
                true   -> error_logger:info_msg("enough triples back to handler ~p",[CometRequest#comet_request.request_handler]),
                          CometRequest#comet_request.request_handler ! plaza_web:combine(CometRequest#comet_request.method,
                                                                                         CometRequest#comet_request.request,
                                                                                         CometRequest#comet_request.response,
                                                                                         State,
                                                                                         CometRequest#comet_request.application,
                                                                                         CometRequest#comet_request.resource,
                                                                                         CometRequest#comet_request.combinators),
                          {stop, normal, exit, CometRequest} ;
                false  -> {reply, ok, CometRequest#comet_request{ counter = Message#triples_msg.counter }}
            end ;
        ErrorResult ->
            error_logger:info_msg("Comet handler, query result nok",[]),
            CometRequest#comet_request.request_handler ! ErrorResult,
            {stop, normal, exit, CometRequest}
    end .
