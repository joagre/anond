%% -*- erlang -*-
{application, socks,
 [{description,"The anond socks server"},
  {vsn, "1.0"},
  {modules, [socks_app, socks_serv, socks_sup]},
  {registered, [socks_serv, socks_sup]},
  {mod, {socks_app, []}},
  {env, []},
  {applications, [kernel, sasl, stdlib]}]}.
