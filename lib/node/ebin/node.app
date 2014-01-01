%% -*- erlang -*-
{application, node,
 [{description,"The anond node application"},
  {vsn, "1.0"},
  {modules, [node_app, node_instance_sup, node_jsonrpc, node_path_cost_serv,
             node_recv_serv, node_route, node_route_jsonrpc,
             node_route_jsonrpc_serv, node_route_serv, node_send_serv,
             node_send_sup, node_starter_serv, node_sup, node_tun_serv]},
  {mod, {node_app, []}},
  {env, []},
  {applications, [kernel, sasl, stdlib, ds]}]}.
