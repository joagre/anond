%% Copyright (c) 2013, Patrik Winroth <patrik@bwi.se>
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

-opaque psp() :: <<_:2560>>.
-opaque psp_db() :: ets:tid().
-export_type([psp/0, psp_db/0]).

%%%_* API ======================================================================
%% @doc Initializes a random 128 bit crypto key, a random string and a counter.
-spec init() -> {'ok', psp_db()}.
init() ->
  PspDB = init_ets(),
  reset_crypto(PspDB),
  {ok, PspDB}.

%% @doc Generate a new PSP where add_me/1 gives the only non-random entry.
-spec new(psp_db()) -> {'ok', psp()}.
new(PspDB) ->
  PSP = crypto:rand_bytes(16*20),
  add_me(PspDB, PSP).

%% @doc Checks if there is an entry in the PSP that is us, i.e. we are looping.
-spec is_loop(psp_db(), psp()) -> boolean().
is_loop(PspDB, PSP) ->
  [loop || <<Encrypted:128>> <= PSP, is_me(PspDB, <<Encrypted:128>>)] =/= [].

%% @doc Add an entry to the PSP for this node.
-spec add_me(psp_db(), psp()) -> {'ok', psp()}.
add_me(PspDB, PSP) ->
  <<PS:2432, _:128>> = PSP,
  Cnt = inc_cnt(PspDB),
  Str = get_key(PspDB, str),
  Me = encrypt(PspDB, <<Str/binary, Cnt:48>>),
  {ok, <<Me/binary, PS:2432>>}.

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
  crypto:block_encrypt(aes_cbc128, get_key(PspDB, key), get_key(PspDB, iv), B).

decrypt(PspDB, B) ->
  crypto:block_decrypt(aes_cbc128, get_key(PspDB, key), get_key(PspDB, iv), B).

init_ets() ->
  ets:new(table() , [ set, public
		    , {read_concurrency, true}
                    , {write_concurrency, true}
                    ]).

reset_crypto(PspDB) ->
  IV = crypto:rand_bytes(16),  
  Key = crypto:rand_bytes(16),
  Str = crypto:rand_bytes(10),
  Cnt = crypto:rand_bytes(6),
  <<IntCnt:48>> = Cnt,
  ets:insert(PspDB, [ {iv, IV}, {key, Key}
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
