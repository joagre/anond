-module(cryptolib).

%%% external exports
-export([create_sign_keypair_files/1]).
-export([read_key_file/1]).
-export([sign_data/1]).

%%% internal exports

%%% include files
-include_lib("util/include/log.hrl").

%%% constants

%%% records

%%% types

%%%
%%% exported: create_sign_keypair_files
%%%

-spec create_sign_keypair_files([string()]) -> 'ok'.

create_sign_keypair_files(KeyFilenames) ->
    create_keypair_files(KeyFilenames, make_sign_keypair).

create_keypair_files([PublicKeyFile, SecretKeyFile], KeyFunction) ->
    salt_server:start_link(),
    {PublicKey, SecretKey} = salt_server:KeyFunction(),
    case file:write_file(PublicKeyFile, [base64:encode(PublicKey), $\n]) of
        ok ->
            case file:write_file(SecretKeyFile,
                                 [base64:encode(SecretKey), $\n]) of
                ok ->
                    erlang:halt(0);
                {error, Reason} ->
                    stderr:print(1, true, "~s: ~s",
                                 [SecretKeyFile, file:format_error(Reason)])
            end;
        {error, Reason} ->
            stderr:print(2, true, "~s: ~s",
                         [PublicKeyFile, file:format_error(Reason)])
    end.

%%%
%%% exported: sign_data
%%%

-spec sign_data([string()]) -> 'ok'.

sign_data([SecretKeyFile, DataFile]) ->
    salt_server:start_link(),
    case file:read_file(SecretKeyFile) of
        {ok, SecretKey} ->
            case catch base64:decode(SecretKey) of
                DecodedSecretKey when is_binary(DecodedSecretKey) ->
                    case file:read_file(DataFile) of
                        {ok, Data} ->
                            Signature =
                                base64:encode(
                                  salt:crypto_sign(salt:crypto_hash(Data),
                                                   DecodedSecretKey)),
                            io:format("~s~n", [Signature]),
                            erlang:halt(0);
                        {error, Reason} ->
                            stderr:print(1, true, "~s: ~s",
                                         [DataFile, file:format_error(Reason)])
                    end;
                _ ->
                    stderr:print(2, true, "~s: Invalid secret key", [DataFile])
            end;
        {error, Reason} ->
            stderr:print(3, true, "~s: ~s",
                         [DataFile, file:format_error(Reason)])
    end.

%%%
%%% exported: read_key_file
%%%

-spec read_key_file(string()) -> binary().

read_key_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Key} ->
            base64:decode(Key);
        {error, Reason} ->
            ?daemon_log("~s: ~s", [Filename, file:format_error(Reason)]),
            <<>>
    end.
