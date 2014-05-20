%% Copyright (c) 2014, Patrik Winroth <patrik@bwi.se>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%
%% @doc Module that contains functions for handling a pseudo path.
%% @end

-module(node_psp).

-export([ init/0
        , new/1
        , is_loop/2
        , add_me/2
        ]).

%% WHAT?
%%-type psp() :: <<_:2560>>.
-type psp() :: binary().
-type psp_db() :: ets:tid().
-export_type([psp/0, psp_db/0]).

%%%_* API ======================================================================
%% @doc Initializes a random 256 bit crypto key, a random string and a counter.
-spec init() -> psp_db().
init() ->
  PspDB = init_ets(),
  reset_crypto(PspDB),
  PspDB.

%% @doc Generate a new PSP where add_me/1 gives the only non-random entry.
-spec new(psp_db()) -> psp().
new(PspDB) ->
  PSP = salt:crypto_random_bytes(16*20),
  add_me(PspDB, PSP).

%% @doc Checks if there is an entry in the PSP that is us, i.e. we are looping.
-spec is_loop(psp_db(), psp()) -> boolean().
is_loop(PspDB, PSP) ->
  [loop || <<Encrypted:128>> <= PSP, is_me(PspDB, <<Encrypted:128>>)] =/= [].

%% @doc Add an entry to the PSP for this node.
-spec add_me(psp_db(), psp()) -> psp().
add_me(PspDB, PSP) ->
  <<PS:2432, _:128>> = PSP,
  Cnt = inc_cnt(PspDB),
  Str = get_key(PspDB, str),
  Me = encrypt(PspDB, <<Str/binary, Cnt:48>>),
  <<Me/binary, PS:2432>>.

%%%_* Internal =================================================================
table() ->
  psp.

is_me(PspDB, Encrypted) ->
  CurrentCnt = get_key(PspDB, cnt),
  <<Str:80>> = get_key(PspDB, str),
  case catch decrypt(PspDB, Encrypted) of
    <<Str:80, Cnt:48>> when Cnt < CurrentCnt ->
      true;
    _ ->
      false
  end.

encrypt(PspDB, B) ->
  salt:crypto_stream_xor(B, get_key(PspDB, nc), get_key(PspDB, key)).

decrypt(PspDB, B) ->
  salt:crypto_stream_xor(B, get_key(PspDB, nc), get_key(PspDB, key)).

init_ets() ->
  ets:new(table() , [ set, public
		    , {read_concurrency, true}
                    , {write_concurrency, true}
                    ]).

reset_crypto(PspDB) ->
  Key = salt:crypto_random_bytes(32),
  Nc = salt:crypto_random_bytes(24),
  Str = salt:crypto_random_bytes(10),
  Cnt = salt:crypto_random_bytes(6),
  <<IntCnt:48>> = Cnt,
  ets:insert(PspDB, [ {key, Key}, {nc, Nc}
                    , {str, Str}, {cnt, IntCnt}]).

inc_cnt(PspDB) ->
  case ets:update_counter(PspDB, cnt, {2, 1, 281474976710655, 0}) of
    0 = N ->
      reset_crypto(PspDB),
      N;
    N ->
      N
  end.

get_key(PspDB, Key) ->
  [{_, Val}] = ets:lookup(PspDB, Key),
  Val.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
