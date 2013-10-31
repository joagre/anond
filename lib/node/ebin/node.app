%% -*- erlang -*-
{application, node,
 [{description,"The anond node server"},
  {vsn, "1.0"},
  {modules, [node_app, node_route, node_serv, node_sup]},
  {env, []},
  {applications, [kernel, sasl, stdlib, ds]}]}.
