%% -*- erlang -*-
{application, overseer,
 [{description,"The anond overseer server"},
  {vsn, "1.0"},
  {modules, [overseer_app, overseer_serv, overseer_sup]},
  {registered, [overseer_serv, overseer_sup]},
  {mod, {overseer_app, []}},
  {env, []},
  {applications, [kernel, sasl, stdlib, ds, node]}]}.
