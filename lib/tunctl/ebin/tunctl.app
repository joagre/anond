%% -*- erlang -*-
{application,tunctl,
 [{description,"TUN/TAP interface"},
  {vsn,"0.2.0"},
  {modules,[tuncer,tunctl,tunctl_darwin,tunctl_freebsd,
            tunctl_linux,tunctl_netbsd]}]}.
