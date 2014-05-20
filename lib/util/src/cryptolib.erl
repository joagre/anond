-module(cryptolib).

%%% external exports
-export([create_sign_keypair_files/1]).
-export([read_key_file/1]).

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

create_keypair_files([PublicKeyFile, PrivateKeyFile], KeyFunction) ->
    salt_server:start_link(),
    {PublicKey, PrivateKey} = salt_server:KeyFunction(),
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
