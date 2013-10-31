%% -*- erlang -*-
{application, ds,
 [{description,"The anond ds server"},
  {vsn, "1.0"},
  {modules, [ds_app, ds_serv, ds_sup]},
  {registered, [ds_serv, ds_sup]},
  {mod, {ds_app, []}},
  {env, []},
  {applications, [kernel, sasl, stdlib]}]}.
