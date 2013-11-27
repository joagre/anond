-ifndef(NODE_PATH_COST_HRL).
-define(NODE_PATH_COST_HRL, true).

-record(echo_request, {
          sequence_number :: 1..4095,
          unique_id :: 0..65535,
          timestamp :: 0..4294967295
         }).

-record(echo_reply, {
          sequence_number :: 1..4095,
          unique_id :: 0..65535,
          timestamp :: 0..4294967295
         }).

-endif.
