-ifndef(DS_HRL).
-define(DS_HRL, true).

-include_lib("node/include/node.hrl").

-record(peer, {
	  ip           :: ip(),
          public_key   :: public_key:rsa_public_key(),
          last_updated :: timelib:gsecs(),
          flags = 0    :: integer()
	 }).

-define(F_DS_EXPERIMENTAL, (1 bsl 1)).

-endif.
