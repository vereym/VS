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

-define(DELAY, 30000).
-define(LogFile, "cbcast_interface.log").
-define(TIME, now2string(erlang:timestamp())).

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
        logging(?LogFile, format("~sstop: keine Antwort von Kommunikationseinheit erhalten~n", [?TIME])),
        nok
    end.

send(CommPID, Message) ->
    CommPID ! {self(), {send, Message}},
    receive
        ok ->
            ok
    after ?DELAY ->
        logging(?LogFile, format("~ssend: keine Antwort von Kommunikationseinheit erhalten~n", [?TIME])),
        nok
    end.

received(CommPID) ->
    CommPID ! {self(), received},
    receive
        {ok, Message} ->
            logging(?LogFile, format("~sreceived: Nachricht=~p bekommen~n", [?TIME, Message])),
            Message
    after ?DELAY ->
        logging(
            ?LogFile, format("~sreceived: keine Antwort von Kommunikationseinheit erhalten~n", [?TIME])
        ),
        nok
    end.

-spec read(CommPID) -> Message | null when
    CommPID :: pid(),
    Message :: string().
read(CommPID) ->
    CommPID ! {self(), read},
    receive
        {ok, Message} ->
            logging(?LogFile, format("~sread: Nachricht=~p bekommen~n", [?TIME, Message])),
            Message
    after ?DELAY ->
        logging(?LogFile, format("~sread: keine Antwort von Kommunikationseinheit erhalten~n", [?TIME])),
        nok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hilfsfunktionen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 11
start() ->
    % 11.1
    logging("com_einheit_test.log", format("kommunikationseinheit in start~n")),
    {ok, CBCconfig} = file:consult("towerCBC.cfg"),
    {ok, Name} = get_config_value(servername, CBCconfig),
    {ok, Node} = get_config_value(servernode, CBCconfig),
    TowerCBC = {Name, Node},
    % 11.2
    VT = vectorC:initVT(),
    VecID = vectorC:myVTid(VT),

    {ok, HostName} = inet:gethostname(),
    LogFile = format("cbCast~B@~s.log", [VecID, HostName]),

    % 11.3
    TowerCBC ! {self(), {register, self()}},
    % 11.4
    receive
        {replycbc, ok_registered} ->
            logging(LogFile, format("~s: Erfolgreich beim TowerCBC registriert!~n", [?TIME]));
        {replycbc, ok_existing} ->
            logging(LogFile, format("~s: Kommunikationseinheit war bereits registriert.~n", [?TIME]))
    after ?DELAY ->
        logging(
            LogFile,
            format("~s: Registrierung nicht erfolgreich. Keine Antwort von TowerCBC erhalten.~n", [
                ?TIME
            ])
        )
    end,
    % 11.5
    HBQ = initHBQ(),
    DLQ = initDLQ(),
    % 11.6
    loop(VT, DLQ, HBQ, TowerCBC, LogFile).

% 12
loop(MyVT, DLQ, HBQ, TowerCBC, LogFile) ->
    % 12.1 & 12.2
    receive
        % 13
        {_PID, {castMessage, {Message, MessageVT}}} ->
            MessageVTID = vectorC:myVTid(MessageVT),
            MyVTID = vectorC:myVTid(MyVT),
            % 13.1
            IsDeliverable = checkDeliverable(MyVT, MessageVT),
            if
                %% eigene Nachricht ignorieren
                MessageVTID == MyVTID ->
                    loop(MyVT, DLQ, HBQ, TowerCBC, LogFile);
                % 13.2
                IsDeliverable ->
                    NewDLQ = addToDLQ(DLQ, {Message, MessageVT}),
                    loop(MyVT, NewDLQ, HBQ, TowerCBC, LogFile);
                % 13.3
                true ->
                    NewHBQ = addToHBQ(HBQ, {Message, MessageVT}, LogFile),
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
            logging(LogFile, format("~s: Msg=~p gesendet~n", [?TIME, Msg])),
            % 15.5
            From ! ok,
            loop(NewVT, NewDLQ, HBQ, TowerCBC, LogFile);
        % 16
        {From, received} ->
            logging(LogFile, format("~s: received von ~p erhalten~n", [?TIME, From])),
            % 16.1
            {Msg, NewDLQ} = getMessage(DLQ),
            % 16.2 & 16.3
            {{Message, MessageVT}, NewHBQ} =
                if
                    Msg == null ->
                        received_loop(HBQ, MyVT, LogFile);
                    true ->
                        {Msg, HBQ}
                end,
            % 16.4
            From ! {ok, Message},
            % 16.5
            NewVT = vectorC:syncVT(MyVT, MessageVT),
            % 16.6
            {NewerHBQ, NewerDLQ} = moveDeliverable(NewHBQ, NewDLQ, NewVT),
            loop(NewVT, NewerDLQ, NewerHBQ, TowerCBC, LogFile);
        % 17
        {From, read} ->
            logging(LogFile, format("~s: read von ~p erhalten~n", [?TIME, From])),
            % 17.1
            {Msg, NewDLQ} = getMessage(DLQ),
            if
                % 17.2
                Msg == null ->
                    From ! {ok, null},
                    loop(MyVT, DLQ, HBQ, TowerCBC, LogFile);
                % 17.3
                true ->
                    {Message, MessageVT} = Msg,
                    % 17.4
                    From ! {ok, Message},
                    % 17.5
                    NewVT = vectorC:syncVT(MyVT, MessageVT),
                    % 17.6
                    {NewHBQ, NewerDLQ} = moveDeliverable(HBQ, NewDLQ, NewVT),
                    loop(MyVT, NewerDLQ, NewHBQ, TowerCBC, LogFile)
            end
    end.

received_loop(HBQ, VT, LogFile) ->
    receive
        {_PID, {castMessage, {Message, MessageVT}}} ->
            logging(
                LogFile, format("~s: in received_loop Message=~p erhalten~n", [?TIME, Message])
            ),
            case checkDeliverable(VT, MessageVT) of
                true -> {{Message, MessageVT}, HBQ};
                false -> received_loop(addToHBQ(HBQ, {Message, MessageVT}, LogFile), VT, LogFile)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Schnittstellen der HBQ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type hbq() :: [msg()].

% 18
initHBQ() ->
    % 18.1
    [].

% 19
-spec addToHBQ(hbq(), msg(), LogFile) -> hbq() when
    LogFile :: string().
addToHBQ(HBQ, {Message, MessageVT}, LogFile) ->
    logging(LogFile, format("~s: fuege {~s, ~p} in HBQ ein~n", [?TIME, Message, MessageVT])),
    % 19.1, 19.2
    [{Message, MessageVT} | HBQ].

% 20
-spec checkDeliverable(VT, MessageVT) -> true | false when
    VT :: vectorC:vectorTimestamp(),
    MessageVT :: vectorC:vectorTimestamp().
checkDeliverable(VT, MessageVT) ->
    % 20.1
    Return = vectorC:aftereqVTJ(VT, MessageVT),
    if
        % 20.2
        Return == {aftereqVTJ, -1} -> true;
        % 20.3
        true -> false
    end.

checkDeliverable_test() ->
    VT2 = {2, [0, 1]},
    VT3 = {3, [1, 0, 0]},
    MyVT = {4, [0, 0, 0, 0]},
    ?assertEqual(true, checkDeliverable(MyVT, VT2)),
    ?assertEqual(false, checkDeliverable(MyVT, VT3)),
    ok.

% 21
-spec moveDeliverable(HBQ, DLQ, VT) -> {NewHBQ, NewDLQ} when
    HBQ :: hbq(),
    DLQ :: dlq(),
    VT :: vectorC:vectorTimestamp(),
    NewHBQ :: hbq(),
    NewDLQ :: dlq().
moveDeliverable([], DLQ, _VT) ->
    {[], DLQ};
%% [Neu Alt Aelter]
moveDeliverable([H = {_, MessageVT} | T], DLQ, VT) ->
    {NewHBQ, NewDLQ} = moveDeliverable(T, DLQ, VT),
    IsDeliverable = checkDeliverable(VT, MessageVT),
    if
        IsDeliverable -> {NewHBQ, addToDLQ(NewDLQ, H)};
        true -> {[H | NewHBQ], NewDLQ}
    end.

moveDeliverable_test() ->
    VT1 = {1, [1]},
    VT2 = {2, [0, 1]},
    VT3 = {3, [1, 0, 0]},
    MyVT = {4, [0, 0, 0, 0]},
    HBQ = initHBQ(),
    DLQ = initDLQ(),
    HBQ1 = addToHBQ(HBQ, {"test", VT1}, ""),
    HBQ2 = addToHBQ(HBQ1, {"test", VT2}, ""),
    HBQ3 = addToHBQ(HBQ2, {"test", VT3}, ""),
    Outcome = {[{"test", VT3}], [{"test", VT1}, {"test", VT2}]},
    ?assertEqual(Outcome, moveDeliverable(HBQ3, DLQ, MyVT)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Schnittstellen der DLQ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type dlq() :: [msg()].

% 22
initDLQ() ->
    % 22.1
    [].

% 23
-spec addToDLQ(dlq(), msg()) -> dlq().
addToDLQ([], {Message, MessageVT}) ->
    % 23.1
    [{Message, MessageVT}];
addToDLQ([H | T], {Message, MessageVT}) ->
    % 23.2
    [H | addToDLQ(T, {Message, MessageVT})].

% 24
-spec getMessage(dlq()) -> {null, dlq()} | {msg(), dlq()}.
getMessage([]) ->
    % 24.1
    {null, []};
getMessage([{Message, MessageVT} | T]) ->
    % 24.2
    {{Message, MessageVT}, T}.

getMessage_test() ->
    DLQ = [],
    Message1 = {"test", {1, [0]}},
    NewDLQ = addToDLQ(DLQ, Message1),
    ?assertEqual({Message1, []}, getMessage(NewDLQ)),

    Message2 = {"test2", {2, [0, 0]}},
    NewerDLQ = addToDLQ(NewDLQ, Message2),
    ?assertEqual({Message1, [Message2]}, getMessage(NewerDLQ)),
    ok.

-type msg() :: {string(), vectorC:vectorTimestamp()}.
