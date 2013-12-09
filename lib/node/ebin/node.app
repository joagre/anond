%% -*- erlang -*-
{application, node,
 [{description,"The anond node application"},
  {vsn, "1.0"},
  {modules, [node_app, node_instance_sup, node_route,
             node_route_serv, node_starter_serv, node_sup,
             node_tunnel_recv_serv, node_tunnel_send_serv, node_tunnel,
             node_tunnel]},
  {mod, {node_app, []}},
  {env, []},
  {applications, [kernel, sasl, stdlib, ds]}]}.
