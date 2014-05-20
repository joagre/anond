%% -*- erlang -*-
{application, ds,
 [{description,"The anond ds application"},
  {vsn, "1.0"},
  {modules, [ds_app, ds_jsonrpc, ds_jsonrpc_serv, ds_serv, ds_sup,
             ds_udp_serv]},
  {registered, [ds_serv, ds_sup, ds_udp_serv]},
  {mod, {ds_app, []}},
  {env, []},
  {applications, [kernel, sasl, stdlib]}]}.
