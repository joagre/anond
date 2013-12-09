%% -*- erlang -*-
{application, util,
 [{description,"Jocke's utility library"},
  {vsn, "1.0"},
  {modules, [config_json_serv, httplib, jsonrpc_serv, log_serv, mime_types
             net_tools, serv, stderr, tcp_serv, timelib]},
  {applications, [kernel, sasl, stdlib]}]}.
