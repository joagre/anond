%% -*- erlang -*-
{application, util,
 [{description,"Jocke's utility library"},
  {vsn, "1.0"},
  {modules, [config_serv, httplib, net_tools, serv, tcp_serv, tree_store]},
  {applications, [kernel, sasl, stdlib, xmerl]}]}.
