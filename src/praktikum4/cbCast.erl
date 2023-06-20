-module(cbCast).

-export([init/0]).
-export([stop/1]).
-export([send/2]).
-export([received/1]).
-export([read/1]).

-include_lib("eunit/include/eunit.hrl").

-import(vsutil, [get_config_value/2, now2string/1]).
-import(util, [logging/2]).
-import(io_lib, [format/2]).
-import(io, [format/1]).

-define(DELAY, 3000).

init() ->
    spawn(fun start/0).

stop(CommPID) ->
    CommPID ! {self(), stop},
    receive
        ok ->
            done
    after ?DELAY ->
        io:format("keine Antwort von Kommunikationseinheit erhalten"),
        nok
    end.

send(CommPID, Message) ->
    CommPID ! {self(), {send, Message}},
    receive
        ok ->
            ok
    after ?DELAY ->
        io:format("keine Antwort von Kommunikationseinheit erhalten"),
        nok
    end.

received(CommPID) ->
    CommPID ! {self(), received},
    receive
        {ok, Message} ->
            Message
    after ?DELAY ->
        io:format("keine Antwort von Kommunikationseinheit erhalten"),
        nok
    end.

-spec read(CommPID) -> Message | null
    when CommPID :: pid(),
         Message :: string().
read(CommPID) ->
    CommPID ! {self(), read},
    receive
        {ok, Message} ->
            Message
    after ?DELAY ->
        io:format("keine Antwort von Kommunikationseinheit erhalten"),
        nok
    end.

%% Hilfsfunktionen %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    todo.
