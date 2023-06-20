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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Schnittstellen für den Anwender
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

-spec read(CommPID) -> Message | null when
    CommPID :: pid(),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hilfsfunktionen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 11
start() ->
    % 11.1
    {ok, CBCconfig} = file:consult("towerCBC.cfg"),
    {ok, Name} = get_config_value(servername, CBCconfig),
    {ok, Node} = get_config_value(servernode, CBCconfig),
    TowerCBC = {Name, Node},
    % 11.2
    {VecID, Vektor} = vectorC:initVT(),

    {ok, HostName} = inet:gethostname(),
    LogFile = format("cbCast~B@~s.log", [VecID, HostName]),

    % 11.3
    TowerCBC ! {self(), {register, VecID}},
    % 11.4
    receive
        {replycbc, ok_registered} ->
            logging(
                LogFile,
                format("~s: Erfolgreich beim TowerCBC registriert!", [
                    now2string(erlang:timestamp())
                ])
            );
        {replycbc, ok_existing} ->
            logging(
                LogFile,
                format("~s: Kommunikationseinheit war bereits registriert.", [
                    now2string(erlang:timestamp())
                ])
            )
    after 5000 ->
        logging(
            LogFile,
            format("~s: Registrierung nicht erfolgreich. Keine Antwort von TowerCBC erhalten.", [
                now2string(erlang:timestamp())
            ])
        )
    end,
    % 11.5
    HBQ = initHBQ(),
    DLQ = initDLQ(),
    % 11.6
    loop({VecID, Vektor}, DLQ, HBQ, TowerCBC, LogFile).

% 12
loop(MyVT = {VecID, Vektor}, DLQ, HBQ, TowerCBC, LogFile) ->
    % 12.1 & 12.2
    receive
        % 13
        {PID, {castMessage, {Message, MessageVT}}} -> pass;
        % 14
        {From, stop} -> pass;
        % 15
        {From, {send, Message}} -> pass;
        % 16
        {From, received} -> pass;
        % 17
        {From, read} -> pass
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Schnitstellen der HBQ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 18
initHBQ() ->
    [].

% 19
addToHBQ(HBQ, {Message, MessageVT}) ->
    pass.

% 20
checkDeliverable(VT, MessageVT) ->
    pass.

% 21
moveDeliverable(HBQ, DLQ, VT) ->
    pass.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Schnitstellen der DLQ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 22
initDLQ() ->
    [].

% 23
addToDLQ(DLQ, {Message, MessageVT}) ->
    pass.

% 24
getMessage(DLQ) ->
    pass.
