-module(plaza_rabbit_backend) .

%% @doc
%% Functions for manipulating a rabbitmq system
%% used in the rabbitmq extension.

-author("Antonio Garrote Hernandez") .

-behaviour(gen_server) .

-include_lib("states.hrl").
-include_lib("rabbit_states.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client.hrl").

-export([start/0, start_link/0, consume/2, publish/2, create_queue/2, exchange_default_values/0]) .
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% public API


%% @doc
%% Starts the rabbitmq application
start() ->
    application:start(sasl) ,
    application:start(mnesia) ,
    application:start(os_mon) ,
    application:start(rabbit) .


%% @doc
%% Starts the backend
start_link() ->
    gen_server:start_link({local, plaza_rabbit_backend}, plaza_rabbit_backend, [], []) .


%% Creates a new queu with Options.
create_queue(QueueName, RoutingKey) ->
    gen_server:call(plaza_rabbit_backend, {create, [{name, QueueName},{routing_key, RoutingKey}]}) .


%% Publishes Content to the Queue using a collection of RoutingKeys
%% created when declaring queues with create_queue.
publish(Content, RoutingKeys) ->
    gen_server:call(plaza_rabbit_backend, {publish, Content, RoutingKeys}) .


%% creates a new process consuming messages from the Queue.
consume(F, RoutingKeys) ->
    gen_server:call(plaza_rabbit_backend, {consume, F, RoutingKeys}) .


%% callbacks


init(_Arguments) ->
    Params = #amqp_params{ username = <<"guest">>,
                           password = <<"guest">> },
    ConnectionPid = amqp_connection:start_direct(Params),
    {Channel, Ticket} = channel_setup(ConnectionPid),

    X = plaza_exchange(),
    ExchangeDeclare = #'exchange.declare'{ticket = Ticket,
                                          exchange = X,
                                          type= <<"direct">>,
                                          passive = false,
                                          durable = false,
                                          auto_delete=false,
                                          internal = false,
                                          nowait = false,
                                          arguments = []},

    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

    { ok, #rabbit_queue_state{ connection = ConnectionPid, channel = Channel, ticket = Ticket, exchange = ExchangeDeclare } } .


handle_call({create, Options}, _From, State) ->
    error_logger:info_msg("plaza_rabbit_backend handle_call create ~p",[Options]),
    AlreadyDeclared = proplists:is_defined(proplists:get_value(name,Options),State#rabbit_queue_state.queues),
    if AlreadyDeclared =:= false ->
            try declare_queue(Options, State) of
                {_Q,NewQueue} -> { reply, ok, State#rabbit_queue_state{ queues = [ NewQueue | State#rabbit_queue_state.queues ] } }
            catch
                Exception    -> error_logger:info_msg("Error declaring queue ~p with ~p",[Options, Exception]),
                                { reply, error, State }
            end ;
       true -> {reply, ok, State}
    end ;

handle_call({publish, Content, RoutingKeys}, _From, State) ->
    Counter = State#rabbit_queue_state.counter + 1,
    error_logger:info_msg("handling publish request for routing keys ~p and counter ~p",[RoutingKeys, Counter]),
    Msg = #triples_msg{ counter = Counter, content = Content },
    lists:foreach(fun(Key) ->
                          publish_content(Msg, Key, State)
                  end,
                  RoutingKeys),
    {reply, ok, State#rabbit_queue_state{ counter = Counter }} ;

handle_call({consume, Function, RoutingKeys}, _From, State) ->
    register_consumer(Function, RoutingKeys, State),
    {reply, ok, State} .


%% dummy callbacks so no warning are shown at compile time
handle_cast(_Msg, State) ->
    {noreply, State} .

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(shutdown, _State) ->
    ok.


%% private API

plaza_exchange() ->
    <<"plaza">> .


%% @doc
%% Default values for channel configuration
%% options of RabbitMQ
channel_default_values() ->
    [ {realm, <<"plaza">>},
      {exclusive, false},
      {passive, false},
      {active, true},
      {write, true},
      {read, true} ] .

%% @doc
%% Default values for exchange configuration
%% options of RabbitMQ
exchange_default_values() ->
    [ {type, <<"direct">>},
      {passive, false},
      {durable, false},
      {auto_delete, false},
      {internal, false},
      {nowait, false},
      {arguments, []} ] .


%% @doc
%% sets up a new channel over an already stablished connection
%% using the configuration values and the default ones
channel_setup(Connection) ->
    % @todo
    % Read configuration
    %Configuration = configuration:rabbit_channel_configuration(),
    Defaults = channel_default_values(),
    Access = #'access.request'{ realm = proplists:get_value(realm,  Defaults),
                                exclusive = proplists:get_value(exclusive,  Defaults),
                                passive = proplists:get_value(passive,  Defaults),
                                active = proplists:get_value(active,  Defaults),
                                write = proplists:get_value(write,  Defaults),
                                read = proplists:get_value(read,  Defaults) },
    Channel = amqp_connection:open_channel(Connection),
    #'access.request_ok'{ticket = Ticket} = amqp_channel:call(Channel, Access),
    {Channel, Ticket} .



%% @doc
%% Creates a new queue
declare_queue(Options, State) ->
    Q = proplists:get_value(name, Options),
    BindKey = proplists:get_value(routing_key, Options),
    X = plaza_exchange(),
    QueueDeclare = #'queue.declare'{ticket = State#rabbit_queue_state.ticket,
                                    queue = Q,
                                    passive = false,
                                    durable = false,
                                    exclusive = false,
                                    auto_delete = false,
                                    nowait = false,
                                    arguments = []},
    #'queue.declare_ok'{queue = Q,
                        message_count = _MessageCount,
                        consumer_count = _ConsumerCount}  = amqp_channel:call(State#rabbit_queue_state.channel, QueueDeclare),


    QueueBind = #'queue.bind'{ticket = State#rabbit_queue_state.ticket,
                              queue = Q,
                              exchange = X,
                              routing_key = BindKey,
                              nowait = false,
                              arguments = []},

    #'queue.bind_ok'{} = amqp_channel:call(State#rabbit_queue_state.channel, QueueBind),
    {Q, #rabbit_queue{ queue = Q, exchange = X, key = BindKey }} .


queue_for_routing_key(RoutingKey, [Q|Qs]) ->
    error_logger:info_msg("queue_for_routing_key... checking ~p",[Q]),
    case Q#rabbit_queue.key =:= RoutingKey of
        true  -> Q#rabbit_queue.queue ;
        false -> queue_for_routing_key(RoutingKey, Qs)
    end .


publish_content(Content, BindingKey, State) ->
    error_logger:info_msg("plaza_rabbit_backend publish_content :~p, ~p, ~p",
                          [{ticket, State#rabbit_queue_state.ticket},
                           {exchange, plaza_exchange()},
                           {routing_key, BindingKey}]),
    BasicPublish = #'basic.publish'{ticket = State#rabbit_queue_state.ticket,
                                    exchange = plaza_exchange(),
                                    routing_key = BindingKey,
                                    mandatory = false,
                                    immediate = false},
    Payload = #amqp_msg{payload = term_to_binary(Content)},
    amqp_channel:call(State#rabbit_queue_state.channel, BasicPublish, Payload) .


register_consumer(Function, RoutingKeys, State) ->
    ConsumerPid = spawn_consumer(Function, State#rabbit_queue_state.channel),
    lists:foreach(fun(Key) ->
                          Q = queue_for_routing_key(Key, State#rabbit_queue_state.queues),

                          BasicConsume = #'basic.consume'{ticket = State#rabbit_queue_state.ticket,
                                                          queue = Q,
                                                          consumer_tag = <<"">>,
                                                          no_local = false,
                                                          no_ack = true,
                                                          exclusive = false,
                                                          nowait = false},
                          amqp_channel:subscribe(State#rabbit_queue_state.channel, BasicConsume, ConsumerPid)
                  end,
                  RoutingKeys),
    ConsumerPid .


spawn_consumer(Function, Channel) ->
    spawn(fun() ->
                  ConsumeLoop = fun(F, CTags) ->

                                        receive
                                            %% If the registration was sucessful, the consumer will
                                            %% be notified
                                            #'basic.consume_ok'{consumer_tag = ConsumerTag} ->
                                                error_logger:info_msg("rabbit_backend spawned consumer basic_consume.ok",[]),
                                                F(F, [ConsumerTag | CTags]) ;

                                            {#'basic.deliver'{delivery_tag = _DeliveryTag}, Content} ->

                                                #amqp_msg{payload = DeliveredPayload} = Content,
                                                error_logger:info_msg("rabbit_backend spawned consumer delivered",[]),
                                                %% TODO pass the value read to the Function passed as a parameter
                                                Res = Function(binary_to_term(DeliveredPayload)),
                                                case Res of
                                                    ok   -> F(F, CTags);
                                                    exit -> lists:foreach(fun(ConsumerTag) ->
                                                                                  BasicCancel = #'basic.cancel'{consumer_tag = ConsumerTag,
                                                                                                                nowait = false},
                                                                                  #'basic.cancel_ok'{consumer_tag = ConsumerTag}
                                                                                      = amqp_channel:call(Channel,BasicCancel)
                                                                          end,
                                                                          CTags)
                                                end ;

                                            exit ->

                                                %% After the consumer is finished interacting with the
                                                %% queue, it can deregister itself

                                                lists:foreach(fun(ConsumerTag) ->
                                                                      BasicCancel = #'basic.cancel'{consumer_tag = ConsumerTag,
                                                                                                    nowait = false},
                                                                      #'basic.cancel_ok'{consumer_tag = ConsumerTag}
                                                                          = amqp_channel:call(Channel,BasicCancel)
                                                              end,
                                                              CTags)
                                        end
                                end,
                  ConsumeLoop(ConsumeLoop, [])
          end) .


code_change(_OldVsn, State, _Extra) ->
    {ok, State} .


%% tests


%% direct_queue_test() ->
%%     %% first get the connection
%%     Params = #amqp_params{ username = <<"guest">>,
%%                            password = <<"guest">> },
%%     ConnectionPid = amqp_connection:start_direct(Params),

%%     %% Get a channel
%%     Access = #'access.request'{ realm = <<"plaza">>,
%%                                 exclusive = false,
%%                                 passive = false,
%%                                 active = true,
%%                                 write = true,
%%                                 read = true },
%%     Channel = amqp_connection:open_channel(ConnectionPid),
%%     #'access.request_ok'{ticket = Ticket} = amqp_channel:call(Channel, Access),

%%     %% Declare a queue, exchange and binding
%%     Q = <<"test">>,
%%     X = plaza_exchange(),
%%     BindKey = <<"test_key">>,
%%     QueueDeclare = #'queue.declare'{ticket = Ticket,
%%                                     queue = Q,
%%                                     passive = false,
%%                                     durable = false,
%%                                     exclusive = false,
%%                                     auto_delete = false,
%%                                     nowait = false,
%%                                     arguments = []},
%%     #'queue.declare_ok'{queue = Q,
%%                         message_count = _MessageCount,
%%                         consumer_count = _ConsumerCount}  = amqp_channel:call(Channel, QueueDeclare),

%%     ExchangeDeclare = #'exchange.declare'{ticket = Ticket,
%%                                           exchange = X,
%%                                           type= <<"direct">>,
%%                                           passive = false,
%%                                           durable = false,
%%                                           auto_delete=false,
%%                                           internal = false,
%%                                           nowait = false,
%%                                           arguments = []},

%%     #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

%%     QueueBind = #'queue.bind'{ticket = Ticket,
%%                               queue = Q,
%%                               exchange = X,
%%                               routing_key = BindKey,
%%                               nowait = false,
%%                               arguments = []},

%%     #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

%%     %% Register a consumer
%%     BasicConsume = #'basic.consume'{ticket = Ticket,
%%                                     queue = Q,
%%                                     consumer_tag = <<"">>,
%%                                     no_local = false,
%%                                     no_ack = true,
%%                                     exclusive = false,
%%                                     nowait = false},

%%     ConsumerPid = spawn(fun() ->
%%                                 %% If the registration was sucessful, the consumer will
%%                                 %% be notified
%%                                 receive
%%                                     #'basic.consume_ok'{consumer_tag = _ConsumerTag} -> ok
%%                                 end,

%%                                 ConsumeLoop = fun(F) ->
%%                                         %% When a message is routed to the queue, it will be
%%                                         %% delivered to this consumer
%%                                         receive
%%                                             {#'basic.deliver'{delivery_tag = _DeliveryTag}, Content} ->
%%                                                 error_logger:info_msg("Message received!: ~p~n", [Content]),
%%                                                 #amqp_msg{payload = Payload} = Content,
%%                                                 %#content{payload_fragments_rev = Payload} = Content,
%%                                                 %% TODO pass the value read to the Function passed as a parameter
%%                                                 error_logger:info_msg("Message received: ~p~n", [binary_to_term(Payload)]),
%%                                                 F(F) ;
%%                                             Other ->  error_logger:info_msg("Something here!: ~p~n", [Other])
%%                                         end
%%                                 end,
%%                                 ConsumeLoop(ConsumeLoop)
%%                         end),

%%     amqp_channel:subscribe(Channel, BasicConsume, ConsumerPid),

%%     %% Let's publish something
%%     error_logger:info_msg("Publishing to: ~p ~p ~p ~p",[{queue, Q},
%%                                                         {ticket, Ticket},
%%                                                         {exchange, X},
%%                                                         {routing_key, BindKey}]),
%%     BasicPublish = #'basic.publish'{ticket = Ticket,
%%                                     exchange = X,
%%                                     routing_key = BindKey,
%%                                     mandatory = false,
%%                                     immediate = false},
%%     %Payload = #amqp_msg{payload = list_to_binary(["hola"])},
%%     Payload = #amqp_msg{payload = term_to_binary({test, 1 ,[1,2,3]})},
%%     amqp_channel:call(Channel, BasicPublish, Payload) .
