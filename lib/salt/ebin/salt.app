%% -*- erlang -*-
{application,salt,
 [{description, "Erlang bindings for NaCl library."},
  {vsn, "1.0.0"},
  {modules, [salt, salt_app, salt_nif, salt_server, salt_sup]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {salt_app, []}},
  {env, []}]}.
