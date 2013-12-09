%% -*- erlang -*-
{application, common,
 [{description,"The scrupless configuration and log servers"},
  {vsn, "1.0"},
  {modules, [common_app, common_config_json_serv, common_log_serv, common_sup]},
  {registered, [common_sup]},
  {mod, {common_app, []}},
  {env, []},
  {applications, [kernel, sasl, stdlib]}]}.
