-record(rabbit_queue_state, { connection :: pid(),
                              counter = 0,
                              channel,
                              ticket,
                              exchange,
                              queues = [] }) .

-record(rabbit_queue, { queue, exchange, key}) .

-record(triples_msg, { counter,
                       content }) .

-record(comet_request, { type :: (push | block),
                         counter = -1,
                         method,
                         request,
                         response,
                         state,
                         application,
                         resource,
                         combinators,
                         triple_spaces,
                         ref,
                         request_handler }) .
