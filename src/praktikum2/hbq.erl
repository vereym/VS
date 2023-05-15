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
    LogFile = io_lib:format("hbq_dlq@~s.log", [HostName]),
    logging(LogFile,
            io_lib:format("~s: HBQ wurde initialisiert~n", [now2string(erlang:timestamp())])),
    %       2
    DLQ = initDLQ(DLQLimit, LogFile),
    % 3
    HBQ = [],
    % 5     4             7
    PID = spawn(fun() -> loop(DLQ, DLQLimit, HBQ, LogFile) end),
    % 6
    register(HBQName, PID),
    % 8
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
loop(DLQ, DLQCapacity, HBQ, LogFile) ->
    % 1
    {ServerID, Nachricht} =
        receive
            Any ->
                Any
        end,

    case Nachricht of
        % 2
        {request, pushHBQ, Message} ->
            logging(LogFile,
                    io_lib:format("HBQ ~s: request ~p mit Message = ~p bekommen.~n",
                                  [now2string(erlang:timestamp()), pushHBQ, Message])),
            % 3
            {Return, NewHBQ} = pushHBQ(HBQ, DLQ, DLQCapacity, LogFile, Message),
            % 4
            {NewDLQ, UpdatedHBQ} = pushDLQ(Return, NewHBQ, DLQ, LogFile),
            % 5
            ServerID ! {reply, ok},
            loop(NewDLQ, DLQCapacity, UpdatedHBQ, LogFile);
        % 6
        {request, deliverMSG, NNr, ToClient} ->
            logging(LogFile,
                    io_lib:format("HBQ ~s: Nachricht ~p ~p bekommen.~n",
                                  [now2string(erlang:timestamp()), deliverMSG, NNr])),
            % 7
            SendNNr = deliverMSG(NNr, ToClient, DLQ, LogFile),
            % 8
            ServerID ! {reply, SendNNr},
            loop(DLQ, DLQCapacity, HBQ, LogFile);
        % 9
        {request, listHBQ} ->
            logging(LogFile,
                    io_lib:format("HBQ ~s: Nachricht ~p bekommen.~n",
                                  [now2string(erlang:timestamp()), listHBQ])),
            % 10
            logging(LogFile,
                    io_lib:format("um ~s war HBQ = ~p~n", [now2string(erlang:timestamp()), HBQ])),
            % 11
            ServerID ! {reply, ok},
            loop(DLQ, DLQCapacity, HBQ, LogFile);
        % 12
        {request, listDLQ} ->
            logging(LogFile,
                    io_lib:format("HBQ ~s: Nachricht ~p bekommen.~n",
                                  [now2string(erlang:timestamp()), listDLQ])),
            % 13
            logging(LogFile,
                    io_lib:format("um ~s war DLQ = ~p~n", [now2string(erlang:timestamp()), DLQ])),
            % 14
            ServerID ! {reply, ok},
            loop(DLQ, DLQCapacity, HBQ, LogFile);
        % 15
        {request, dellHBQ} ->
            logging(LogFile, io_lib:format("HBQ: Nachricht ~p bekommen.~n", [dellHBQ])),
            % 16
            ServerID ! {reply, ok}
    end.

%% @doc Fügt eine Nachricht in die HBQ ein.
%% Verwirft die Nachricht, wenn ihre Nachrichtennummer bereits in der DLQ war.
%%
%% Außerdem werden hier ggf. Fehlernachrichten erstellt und in die DLQ eingefügt,
%% sollten sich zu viele Nachrichten in der HBQ befinden.
pushHBQ(HBQ, DLQ, DLQCapacity, LogFile, Message = [NNr | _]) ->
    % 1
    ExpectedNNr = dlq:expectedNr(DLQ),
    %     2
    case NNr < ExpectedNNr of
        true ->
            % 3
            logging(LogFile,
                    io_lib:format("HBQ ~s: ~p wurde verworfen~n",
                                  [now2string(erlang:timestamp()), Message])),
            % 4
            {discarded, HBQ};
        false ->
            TShbqin = erlang:timestamp(),
            [NNr, Msg, TSclientout] = Message,
            %                            5
            NewMessage = [NNr, Msg ++ now2string(TShbqin), TSclientout, TShbqin],

            %           6
            NewHBQ = add2HBQ(NewMessage, HBQ),
            %       7           9              8
            case length(NewHBQ) < 2 / 3 * DLQCapacity of
                true ->
                    %   10
                    {ok, NewHBQ};
                false ->
                    %   11
                    [[SmallestHBQ | _] | _] = NewHBQ,
                    %      12
                    LastMissingNNr = SmallestHBQ - 1,
                    % 13
                    Fehlernachricht =
                        % 14
                        [LastMissingNNr,
                         io_lib:format("HBQ ~s: ***Fehlernachricht fuer Nachrichtennummern ~B bis ~B "
                                       "um ~s~n",
                                       [now2string(erlang:timestamp()),
                                        SmallestHBQ,
                                        LastMissingNNr,
                                        now2string(TShbqin)])],
                    % 15
                    logging(LogFile,
                            io_lib:format("HBQ ~s: ~p~n",
                                          [now2string(erlang:timestamp()), Fehlernachricht])),
                    % 16
                    {Fehlernachricht, NewHBQ}
            end
    end.

add2HBQ(Elem, []) ->
    [Elem];
add2HBQ(Elem = [ElemNNr | _ElemTail],
        Messages = [Msg = [MsgNNr | _MsgTail] | MessagesTail]) ->
    case ElemNNr < MsgNNr of
        true ->
            [Elem | Messages];
        false ->
            [Msg | add2HBQ(Elem, MessagesTail)]
    end.

%% @doc Verschiebt so viele Nachrichten wie möglich aus der HBQ in die DLQ.
%% Dies führt zu einem Leeren der HBQ, solange es dort keine Lücken gibt.
%% @end von uns, nicht in der Schnittstelle
%%       3   4
pushDLQ(ok, [], DLQ, _LogFile) ->
    % 11
    {DLQ, []};
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
            {DLQ, Tail}
    end;
pushDLQ(discarded, HBQ, DLQ, _LogFile) ->
    {DLQ, HBQ};
%%           1
pushDLQ(Fehlernachricht, HBQ, DLQ, LogFile) ->
    logging(LogFile,
            io_lib:format("HBQ ~s: ~p wurde zur DLQ gepusht~n",
                          [now2string(erlang:timestamp()), Fehlernachricht])),
    %                   2
    NewDLQ = push2DLQ(Fehlernachricht, DLQ, LogFile),
    %            4
    pushDLQ(ok, HBQ, NewDLQ, LogFile).
