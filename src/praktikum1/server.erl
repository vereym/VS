-module(server).

-export([start/0]).

-import(vsutil, [get_config_value/2, now2string/1, reset_timer/3]).
-import(hbq, [initHBQ/2]).
-import(cmem, [initCMEM/2, getClientNNr/2, updateClient/4, listCMEM/1]).
-import(util, [logging/2]).
-import(io_lib, [format/2]).

% Initialisiert und startet den Server.
start() ->
    % 1
    {ok, ServerConfig} = file:consult("server.cfg"),
    {ok, DLQLimit} = get_config_value(dlqlimit, ServerConfig),
    {ok, HBQName} = get_config_value(hbqname, ServerConfig),
    {ok, HBQNode} = get_config_value(hbqnode, ServerConfig),
    {ok, ServerName} = get_config_value(servername, ServerConfig),
    {ok, RemTime} = get_config_value(clientlifetime, ServerConfig),
    {ok, Latency} = get_config_value(latency, ServerConfig),
    % 2
    %% HBQ = initHBQ(DLQLimit, HBQName),
    {ok, HostName} = inet:gethostname(),
    % 3
    LogFile = format("Server@~s.log", [HostName]),

    % HBQ auf eigener Node starten
    startHBQNode(HBQName, HBQNode, DLQLimit, LogFile),
    HBQ = {HBQName, HBQNode},

    % 4
    CMEM = initCMEM(RemTime, LogFile),
    % 5
    register(ServerName, self()),
    % 6
    {ok, Timer} = timer:send_after(Latency * 1000, {terminateServer}),
    % 7
    loop(1, Latency, HBQ, CMEM, Timer, LogFile).

startHBQNode(HBQName, HBQNode, DLQLimit, LogFile) ->
    case net_adm:ping(HBQNode) of
        pang ->
            logging(
                LogFile,
                format("HBQNode ~s konnte nicht gefunden werden.~n", [HBQNode])
            );
        pong ->
            spawn(HBQNode,fun() -> hbq:initHBQ(DLQLimit, HBQName) end)
    end.
    

% @doc Empfängt und verarbeitet kontinuierlich Nachrichten im Server-Prozess, bis dieser automatisch nach einer Zeit von Latency beendet wird.
% HBQ ist hier die PID des HBQ-Prozesses. Es gibt die folgenden Schnittstellen:
% - getmessages: liefert eine weitere Nachricht an den Client aus und updated anschließend das CMEM
%
% - dropmessage: speichert die mitgelieferte Nachricht in der HBQ ab, dazu wird die Anfrage an den HBQ-Prozess weitergeleitet
%
% - getmsgid: sendet dem Client die nächste eindeutige Nachrichtennummer zurück und erhöht anschließend den NNr-Counter
%
% - listDLQ/HBQ/CMEM: bewirkt ein Logging der DLQ/HBQ/CMEM in einer Datei
loop(NNrCounter, Latency, HBQ, CMEM, Timer, LogFile) ->
    % 1
    receive
        % 3
        {ClientID, getmessages} ->
            logging(LogFile, format("server hat getmessages erhalten~n", [])),
            % 2
            NewTimer = reset_timer(Timer, Latency, {terminateServer}),
            % 4
            ClientNNr = getClientNNr(CMEM, ClientID),
            % 5
            HBQ ! {self(), {request, deliverMSG, ClientNNr, ClientID}},
            ActualNNr =
                receive
                    {reply, SendNNr} ->
                        SendNNr
                after 7000 ->
                    logging(LogFile, format("Keine Antwort von der HBQ erhalten. Server terminiert~n", [])),
                    exit(normal)
                end,
            % 6
            if
                ActualNNr == -1 -> NewCMEM = CMEM;
                %                       7
                true -> NewCMEM = updateClient(CMEM, ClientID, ActualNNr, LogFile)
            end,
            loop(NNrCounter, Latency, HBQ, NewCMEM, NewTimer, LogFile);
        % 8
        {dropmessage, Message = [_NNR, _Msg, _TSclientout]} ->
            % 2
            NewTimer = reset_timer(Timer, Latency, {terminateServer}),
            % 9 & 10
            send_msg(HBQ, {self(), {request, pushHBQ, Message}}, LogFile),
            loop(NNrCounter, Latency, HBQ, CMEM, NewTimer, LogFile);
        % 11
        {ClientID, getmsgid} ->
            % 2
            NewTimer = reset_timer(Timer, Latency, {terminateServer}),
            % 12
            ClientID ! {nid, NNrCounter},
            %    13
            loop(NNrCounter + 1, Latency, HBQ, CMEM, NewTimer, LogFile);
        % 14
        {_ClientID, listDLQ} ->
            % 2
            NewTimer = reset_timer(Timer, Latency, {terminateServer}),
            % 15 & 16
            send_msg(HBQ, {self(), {request, listDLQ}}, LogFile),
            loop(NNrCounter, Latency, HBQ, CMEM, NewTimer, LogFile);
        % 17
        {_ClientID, listHBQ} ->
            % 2
            NewTimer = reset_timer(Timer, Latency, {terminateServer}),
            % 18 & 19
            send_msg(HBQ, {self(), {request, listHBQ}}, LogFile),
            loop(NNrCounter, Latency, HBQ, CMEM, NewTimer, LogFile);
        % 20
        {_ClientID, listCMEM} ->
            % 2
            NewTimer = reset_timer(Timer, Latency, {terminateServer}),
            % 21
            logging(LogFile, format("~p~n", [listCMEM(CMEM)])),
            loop(NNrCounter, Latency, HBQ, CMEM, NewTimer, LogFile);
        % Beenden des Servers bei Ablauf vom Timer.
        {terminateServer} ->
            send_msg(HBQ, {self(), {request, dellHBQ}}, LogFile),
            logging(LogFile, "HBQ terminiert.~n"),
            logging(LogFile, "Server nach Ablauf seiner Latency terminiert.~n"),
            exit(normal);
        % Abfangen jeder beliebigen Nachricht.
        Any ->
            logging(
                LogFile,
                format(
                    "Server hat folgende Nachricht empfangen, konnte damit jedoch nichts anfangen: ~p~n",
                    [Any]
                )
            ),
            loop(NNrCounter, Latency, HBQ, CMEM, Timer, LogFile)
    after Latency * 1000 ->
        send_msg(HBQ, {self(), {request, dellHBQ}}, LogFile),
        logging(LogFile, "HBQ terminiert.~n"),
        logging(LogFile, "Server nach Ablauf seiner Latency terminiert.~n"),
        exit(normal)
    end.

% Hilfsfunktion für loop/6.
% Sendet eine Nachricht an die HBQ und wartet auf ein {reply, ok} als Antwort.
send_msg(HBQ, Message, LogFile) ->
    HBQ ! Message,
    receive
        {reply, ok} ->
            ok
    after 7000 ->
        logging(LogFile, format("Keine Antwort von der HBQ erhalten. Server terminiert~n", [])),
        exit(normal)
    end.
