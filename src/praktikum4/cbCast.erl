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
loop(MyVT, DLQ, HBQ, TowerCBC, LogFile) ->
    % 12.1 & 12.2
    receive
        % 13
        {_PID, {castMessage, {Message, MessageVT}}} ->
            % 13.1
            IsDeliverable = checkDeliverable(MyVT, MessageVT),
            if
                % 13.2
                IsDeliverable ->
                    NewDLQ = addToDLQ(DLQ, {Message, MessageVT}),
                    loop(MyVT, NewDLQ, HBQ, TowerCBC, LogFile);
                % 13.3
                true ->
                    NewHBQ = addToHBQ(HBQ, {Message, MessageVT}),
                    loop(MyVT, DLQ, NewHBQ, TowerCBC, LogFile)
            end;
        % 14
        {From, stop} ->
            % 14.2
            From ! ok;
        % 14.1 -> kein loop-Call
        % 15
        {From, {send, Message}} ->
            % 15.1
            NewVT = vectorC:tickVT(MyVT),
            % 15.2
            Msg = {Message, NewVT},
            % 15.3
            NewDLQ = addToDLQ(DLQ, Msg),
            % 15.4
            TowerCBC ! {self(), {multicastNB, Msg}},
            % 15.5
            From ! ok,
            loop(NewVT, NewDLQ, HBQ, TowerCBC, LogFile);
        % 16
        {From, received} ->
            Msg = getMessage(DLQ),
            if Msg == null ->
                   received_loop()
            end

            loop(MyVT, DLQ, HBQ, TowerCBC, LogFile);
        % 17
        {From, read} ->
            % 17.1
            Msg = getMessage(DLQ),
            if
                % 17.2
                Msg == null -> From ! {ok, null};
                % 17.3
                true -> {Message, _MessageVT} = Msg,
                % 17.4
                From ! {ok, Message}
            end
    end.

received_loop() ->
    receive
        {PID, {castMessage, {Message, MessageVT}}} ->
            % 13.1
            IsDeliverable = checkDeliverable(MyVT, MessageVT),
            if
                % 13.2
                IsDeliverable ->
                    NewDLQ = addToDLQ(DLQ, {Message, MessageVT}),
                    loop(MyVT, NewDLQ, HBQ, TowerCBC, LogFile);
                % 13.3
                true ->
                    NewHBQ = addToHBQ(HBQ, {Message, MessageVT}),
                    loop(MyVT, DLQ, NewHBQ, TowerCBC, LogFile)
            end;
        % 14
        {From, stop} ->
            % 14.2
            From ! ok
        % 14.1 -> kein loop-Call
    after 10000 ->
        logging(
            LogFile,
            format("~s: did not receive message from ~p", [
                now2string(erlang:timestamp()), HandleFrom
            ])
        ),
        {MyVT, DLQ, HBQ, TowerCBC, LogFile}
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Schnittstellen der HBQ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 18
initHBQ() ->
    % 18.1
    [].

% 19
addToHBQ([], {Message, MessageVT}) ->
    % 19.1
    [{Message, MessageVT}];
addToHBQ([H | T], {Message, MessageVT}) ->
    % 19.2
    [H | addToHBQ(T, {Message, MessageVT})].

% 20
checkDeliverable(VT, MessageVT) ->
    % 20.1
    Return = vectorC:aftereqVTJ(VT, MessageVT),
    if
        % 20.2
        Return == {aftereqVTJ, -1} -> true;
        % 20.3
        true -> false
    end.

% 21
moveDeliverable([], DLQ, _VT) ->
    {[], DLQ};
moveDeliverable([H = {_, MessageVT} | T], DLQ, VT) ->
    {NewHBQ, NewDLQ} = moveDeliverable(T, DLQ, VT),
    IsDeliverable = checkDeliverable(VT, MessageVT),
    if
        IsDeliverable -> {NewHBQ, addToDLQ(NewDLQ, H)};
        true -> {[H | NewHBQ], NewDLQ}
    end.

moveDeliverable_test() ->
    pass.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Schnittstellen der DLQ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 22
initDLQ() ->
    % 22.1
    [].

% 23
addToDLQ([], {Message, MessageVT}) ->
    % 23.1
    [{Message, MessageVT}];
addToDLQ([H | T], {Message, MessageVT}) ->
    % 23.2
    [H | addToDLQ(T, {Message, MessageVT})].

% 24
getMessage([]) ->
    % 24.1
    null;
getMessage([{Message, MessageVT} | _T]) ->
    % 24.2
    {Message, MessageVT}.
