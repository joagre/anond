-module(node_crypto).

%%% external exports
-export([generate_pki_keys/1, read_pki_key/1]).

%%% internal exports

%%% include files
-include_lib("util/include/log.hrl").

%%% constants

%%% records

%%% types
-type pki_key() :: binary().

%%%
%%% exported: generate_pki_keys
%%%

-spec generate_pki_keys([string()]) -> 'ok'.

generate_pki_keys([PublicKeyFile, PrivateKeyFile]) ->
    salt_server:start_link(),
    {PublicKey, PrivateKey} = salt_server:make_sign_keypair(),
    case file:write_file(PublicKeyFile, base64:encode(PublicKey)) of
        ok ->
            case file:write_file(PrivateKeyFile, base64:encode(PrivateKey)) of
                ok ->
                    erlang:halt(0);
                {error, Reason} ->
                    stderr:print(1, true, "~s: ~s",
                                 [PrivateKeyFile, file:format_error(Reason)])
            end;
        {error, Reason} ->
            stderr:print(2, true, "~s: ~s",
                         [PublicKeyFile, file:format_error(Reason)])
    end.

%%%
%%% exported: read_pki_key
%%%

-spec read_pki_key(string()) -> pki_key().

read_pki_key(Filename) ->
    case file:read_file(Filename) of
        {ok, Key} ->
            base64:decode(Key);
        {error, Reason} ->
            ?daemon_log("~s: ~s", [Filename, file:format_error(Reason)]),
            <<>>
    end.
