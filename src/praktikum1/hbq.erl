-module(hbq).

-export([initHBQ/2]).

-import(dlq, [initDLQ/2]).
-import(dlq, [expectedNr/1]).
-import(dlq, [deliverMSG/4]).
-import(dlq, [push2DLQ/3]).
-import(util, [logging/2]).
-import(vsutil, [now2string/1]).

%% @doc startet den HBQ-Prozess und initialisiert die HBQ sowie die DLQ.
initHBQ(DLQLimit, HBQName) ->
    % 1
    {ok, HostName} = inet:gethostname(),
    LogFile = io_lib:format("hbq_dlq~s.log", [HostName]),
    logging(
        LogFile, io_lib:format("~s: HBQ wurde initialisiert~n", [now2string(erlang:timestamp())])
    ),
    %       2
    DLQ = initDLQ(DLQLimit, LogFile),
    % 3
    HBQ = [],
    % 5       4            7
    PID = spawn(fun() -> loop(DLQ, HBQ, LogFile) end),
    % 6
    register(HBQName, PID),
    PID.

%% @doc Empfängt und verarbeitet kontinuierlich Befehle vom Server im HBQ-Prozess,
%% bis dieser beendet wird.
%%
%% Es gibt die folgenden Schnittstellen:
%%
%% - pushHBQ: fügt eine mitgesendete Nachricht in die HBQ ein und updatet ggf. die DLQ
%%
%% - deliverMSG: leitet eine Anfrage zum Ausliefern einer Nachricht an die DLQ weiter
%%   und sendet im Anschluss die Nachrichtennummer der tatsächlich versendeten Nachricht
%%   an den Server zurück.
%%
%% - listHBQ/listDLQ: bewirkt ein Logging der HBQ/DLQ in einer Datei
%%
%% - delHBQ: beendet den HBQ-Prozess
loop(DLQ, HBQ, LogFile) ->
    % 1
    {ServerID, Message} =
        receive
            Any ->
                Any
        end,

    case Message of
        % 2
        {request, pushHBQ, Msg} ->
            logging(LogFile, io_lib:format("HBQ: Nachricht ~p ~p bekommen.~n", [pushHBQ, Msg])),
            % 3
            {Return, NewHBQ} = pushHBQ(HBQ, DLQ, LogFile, Msg),
            % 4
            NewDLQ = pushDLQ(Return, HBQ, DLQ, LogFile),
            % 5
            ServerID ! {reply, ok},
            loop(NewDLQ, NewHBQ, LogFile);
        % 6
        {request, deliverMSG, NNr, ToClient} ->
            % 7
            SendNNr = deliverMSG(NNr, ToClient, DLQ, LogFile),
            % 8
            ServerID ! {reply, SendNNr},
            loop(DLQ, HBQ, LogFile);
        % 9
        {request, listHBQ} ->
            % 10
            logging(LogFile, io_lib:format("HBQ = ~p~n", [HBQ])),
            % 11
            ServerID ! {reply, ok},
            loop(DLQ, HBQ, LogFile);
        % 12
        {request, listDLQ} ->
            % 13
            logging(LogFile, io_lib:format("DLQ = ~p~n", [DLQ])),
            % 14
            ServerID ! {reply, ok},
            loop(DLQ, HBQ, LogFile);
        % 15
        {request, delHBQ} ->
            % 16
            ServerID ! ok
    end.

%% @doc Fügt eine Nachricht in die HBQ ein.
%% Verwirft die Nachricht, wenn ihre Nachrichtennummer bereits in der DLQ war.
%%
%% Außerdem werden hier ggf. Fehlernachrichten erstellt und in die DLQ eingefügt,
%% sollten sich zu viele Nachrichten in der HBQ befinden.
pushHBQ(HBQ, DLQ, LogFile, Message = [NNr | _]) ->
    % 1
    ExpectedNNr = dlq:expectedNr(DLQ),
    %     2
    case NNr < ExpectedNNr of
        true ->
            % 3
            logging(LogFile, io_lib:format("HBQ: ~p wurde verworfen~n", [Message])),
            % 4
            discarded;
        %                5
        false ->
            TShbqin = erlang:timestamp(),
            [NNr, Msg, TSclientout] = Message,
            NewMessage = [NNr, Msg ++ now2string(TShbqin), TSclientout, TShbqin],
            NewHBQ = add2HBQ(NewMessage, HBQ),
            % 7
            CapacityDLQ = dlq:lengthDLQ(DLQ),
            %     6            8
            case length(HBQ) < 2 / 3 * CapacityDLQ of
                true ->
                    % 9
                    {ok, NewHBQ};
                false ->
                    %   10
                    [[SmallestHBQ | _] | _] = HBQ,
                    %      11
                    LastMissingNNr = SmallestHBQ - 1,
                    % 12, 13
                    Fehlernachricht =
                        io_lib:format(
                            "HBQ: ***Fehlernachricht fuer Nachrichtennummern ~B bis ~B um ~s~n",
                            [SmallestHBQ, LastMissingNNr, now2string(TShbqin)]
                        ),
                    % 14
                    logging(LogFile, io_lib:format("HBQ: ~p~n", [Fehlernachricht])),
                    % 15
                    {Fehlernachricht, NewHBQ}
            end
    end.

add2HBQ(Elem, []) ->
    [Elem];
add2HBQ(
    Elem = [ElemNNr | _ElemTail],
    Messages = [Msg = [MsgNNr | _MsgTail] | MessagesTail]
) ->
    case ElemNNr < MsgNNr of
        true ->
            [Elem | Messages];
        false ->
            [Msg | add2HBQ(Elem, MessagesTail)]
    end.

%% @doc Verschiebt so viele Nachrichten wie möglich aus der HBQ in die DLQ.
%% Dies führt zu einem Leeren der HBQ, solange es dort keine Lücken gibt.
%% @end
%%       3   4
pushDLQ(ok, [], DLQ, _LogFile) ->
    % 11
    DLQ;
%%             6                     7
pushDLQ(ok, _HBQ = [FirstMsg = [FirstNNr | _] | Tail], DLQ, LogFile) ->
    % 5
    ExpectedNNr = expectedNr(DLQ),
    %              8
    case ExpectedNNr == FirstNNr of
        true ->
            %   9
            NewDLQ = push2DLQ(FirstMsg, DLQ, LogFile),
            %            10
            pushDLQ(ok, Tail, NewDLQ, LogFile);
        false ->
            % 11
            DLQ
    end;
%%           1
pushDLQ(Fehlernachricht, HBQ, DLQ, LogFile) ->
    %% TODO: wie kann man feststellen, dass es sich um die Fehlernachricht handelt?
    %% alles andere als ok?
    logging(LogFile, io_lib:format("HBQ: ~p wurde zur DLQ gepusht~n", [Fehlernachricht])),
    %                   2
    NewDLQ = push2DLQ(Fehlernachricht, DLQ, LogFile),
    %            4
    pushDLQ(ok, HBQ, NewDLQ, LogFile).
