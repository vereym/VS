-module(hbq).

-export([initHBQ/2]).

-import(dlq, [initDLQ/2]).
-import(dlq, [pushDLQ/4]).

%% @doc startet den HBQ-Prozess und initialisiert die HBQ sowie die DLQ.
initHBQ(DLQLimit, HBQName) ->
    % 1
    LogFile = "hbq_dlq.log",
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
            {ServerID, Message} ->
                {ServerID, Message}
        end,

    case Message of
        % 2
        {request, pushHBQ, Msg = [NNr, Msg, TSclientout]} ->
            % 3
            Return = pushHBQ(HBQ, DLQ, LogFile, Msg),
            % 4
            NewDLQ = pushDLQ(Return, HBQ, DLQ, LogFile),
            % 5
            ServerID ! {reply, ok},
            loop(NewDLQ, HBQ, LogFile);
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
            vsutil:logging(LogFile, io_lib:format("HBQ = ~p", HBQ)),
            % 11
            ServerID ! {reply, ok},
            loop(DLQ, HBQ, LogFile);
        % 12
        {request, listDLQ} ->
            % 13
            vsutil:logging(LogFile, io_lib:format("DLQ = ~p", DLQ)),
            % 14
            ServerID ! {reply, ok},
            loop(DLQ, HBQ, LogFile);
        % 15
        {request, delHBQ} ->
            % 16
            ServerID ! ok
    end.

deliverMSG(_, _, _, _) ->
  ok.

pushHBQ(_, _, _, _) ->
    ok.
