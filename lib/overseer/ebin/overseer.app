%% -*- erlang -*-
{application, overseer,
 [{description,"The anond overseer application"},
  {vsn, "1.0"},
  {modules, [overseer]},
  {env, []},
  {applications, [kernel, sasl, stdlib, ds, node]}]}.
