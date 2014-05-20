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
%%% @doc Module that contains functions for handling a path cost.
%%% @end

-module(node_path_cost_auth).

-export([ new/0
        , add_cost/2
        , cost/1
        , r0/1
        ]).

-type hash() :: <<_:512>>.
-type random_integer() :: <<_:512>>.
-type costs() :: [{pos_integer(), hash()}].
-type unallocated_cost() :: [random_integer()].
-type path_cost() :: {costs(), unallocated_cost()}.
-type cost() :: non_neg_integer().
-export_type([ path_cost/0
             , cost/0
             , hash/0
             ]).

%%%_* API ======================================================================

%% @doc Create an empty path cost vector with a Hamming weight of 256 bits.
%% 256 bits element sizes and 512 bits hash output ... 256 bit security.
-spec new() -> path_cost().
new() ->
  V1 = [salt:crypto_random_bytes(64) || _ <- lists:seq(1, 256)],
  {[], V1}.

%% @doc Add a path cost, in total a maximum Hamming weight of 256 is supported.
-spec add_cost(path_cost(), cost()) -> path_cost().
add_cost({CV, V1}, 0) ->
  {compact(CV), V1};
add_cost({CV, V1}, Cost) when Cost > 0 ->
  add_cost(do_add_cost({CV, V1}), Cost - 1).

%% @doc Given a path cost, return cost.
-spec cost(path_cost()) -> cost().
cost({_CV, V1}) ->
  256 - length(V1).

%% @doc Calculate r0, given a path cost vector. Intended to be compared to the
%%      inital path cost as signed by the issuing OA.
-spec r0(path_cost()) -> hash().
r0({CV, Vn}) ->
  VnH = [hash(Xn) || Xn <- Vn],
  calc_r0(expand(CV) ++ VnH).

%%%_* Internal =================================================================

calc_r0([_,_|_] = T) ->
  calc_r0(hash_row(T));
calc_r0([{256, R0}]) ->
  R0;
calc_r0([R0]) ->
  R0.

hash_row([{L,H},{L,H}|Vn]) ->
  [{L,H} | hash_row(Vn)];
hash_row([{_L,H1}, H2|Vn]) ->
  [hash([H1,H2]) | hash_row(Vn)];
hash_row([H1,H2|Vn]) ->
  [hash([H1,H2]) | hash_row(Vn)];
hash_row([]) ->
  [].

do_add_cost({CV, [X1 | Xn]}) ->
  {CV ++ [{1, hash(X1)}], Xn};
do_add_cost({CV, []}) ->
  %% Already at max cost
  {CV, []}.

compact(CV0) ->
  case compact(CV0, [], 0) of
    {CV, N} when N > 0 ->
      compact(CV);
    {CV, 0} ->
      CV
  end.

compact([{L,H1},{L,H2} | CV], Acc, N) ->
  compact(CV, [{L*2, hash([H1,H2])} | Acc], N+1);
compact([{L,H1} | CV], Acc, N) ->
  compact(CV, [{L,H1} | Acc], N);
compact([], Acc, N) ->
  {lists:reverse(Acc), N}.

expand([{N,H} | CV]) ->
  lists:duplicate(N, {N,H}) ++ expand(CV);
expand([]) ->
  [].

hash(Xn) ->
  salt:crypto_hash(Xn).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
