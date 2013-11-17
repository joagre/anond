%% -*- erlang -*-
{application, node,
 [{description,"The anond node application"},
  {vsn, "1.0"},
  {modules, [node_app, node_dummy_tcp_serv, node_multi_sup, node_route,
             node_serv, node_tun_serv, node_sup]},
  {mod, {node_app, []}},
  {env, []},
  {applications, [kernel, sasl, stdlib, ds]}]}.
