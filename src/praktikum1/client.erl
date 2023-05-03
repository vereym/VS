-module(client).

-export([start/0]).

-import(util, [logging/2, randomliste/3]).
-import(vsutil, [get_config_value/2, now2string/1, now2stringD/1, meinSleep/1, validTS/1, diffTS/2]).
-import(io_lib, [format/2]).

% Initialisiert und startet alle Client-Prozesse.
start() ->
    % 1
    {ok, ClientConfig} = file:consult("client.cfg"),
    {ok, Clients} = get_config_value(clients, ClientConfig),
    {ok, LifeTime} = get_config_value(lifetime, ClientConfig),
    {ok, SendeIntervall} = get_config_value(sendeintervall, ClientConfig),
    {ok, ServerName} = get_config_value(servername, ClientConfig),
    {ok, ServerNode} = get_config_value(servernode, ClientConfig),
    {ok, HostName} = inet:gethostname(),
    % 2
    startClients(Clients, LifeTime, SendeIntervall, ServerName, ServerNode, HostName, Clients).

% Hilfsfunktion für start/0.
% Stellt die Schleife im Entwurf dar.
startClients(Clients, _LifeTime, _SendeIntervall, _ServerName, _ServerNode, HostName, 0) ->
    LogFile = format("ClientStarter@~s.log", [HostName]),
    logging(
        LogFile,
        format("~s: Alle ~B Clients erfolgreich gestartet!~n", [
            now2string(erlang:timestamp()), Clients
        ])
    );
startClients(Clients, LifeTime, SendeIntervall, ServerName, ServerNode, HostName, Counter) ->
    % 3 (Gruppe 2 Team 10)
    ClientName = format("Client~B@~s210", [Clients - Counter, HostName]),
    % 4
    startClient(LifeTime, SendeIntervall, ServerName, ServerNode, ClientName),
    startClients(Clients, LifeTime, SendeIntervall, ServerName, ServerNode, HostName, Counter - 1).

% Initialisiert und startet einen Client-Prozess.
startClient(LifeTime, SendeIntervall, ServerName, ServerNode, ClientName) ->
    % 1
    {IntervalMin, IntervalMax} = SendeIntervall,
    [Delay] = randomliste(1, IntervalMin, IntervalMax),
    % 2
    Server = {ServerName, ServerNode},
    % 3
    LogFile = format("~s.log", [ClientName]),
    case net_adm:ping(ServerNode) of
        pang ->
            logging(
                LogFile,
                format("Server-Node ~s konnte nicht gefunden werden.~n", [ServerNode])
            );
        pong ->
            % 4
            spawn(
                fun() ->
                    startLoop(LifeTime, Delay, Server, ClientName, LogFile)
                end
            )
    end.

% Hilfsfunktion für startClient/5.
startLoop(LifeTime, Delay, Server, ClientName, LogFile) ->
    % 5
    timer:send_after(LifeTime * 1000, {terminateClient}),
    % 6 & Schritt 1 im Entwurf von loop
    loop([], LifeTime, Delay, Server, ClientName, LogFile).

% Wechselt kontinuierlich zwischen Redakteur und Leser, bis der Client beendet wird.
% Merkt sich die bereits erhaltenen Nachrichtennummern auch zwischen den Leser-Wechseln im RMEM.
loop(RMEM, LifeTime, Delay, Server, ClientName, LogFile) ->
    % 3
    redakteur(Delay, Server, ClientName, LogFile),
    % 4
    NewRMEM = leser(RMEM, Server, ClientName, LogFile),
    % 5
    NewDelay = randomizeDelay(Delay),
    % 2
    loop(NewRMEM, LifeTime, NewDelay, Server, ClientName, LogFile).

% Sendet in regelmäßigen Abständen 5 Nachrichten an einen Server, fragt anschließend eine weitere
% Nachrichtennummer an und bricht dann ab. Die dadurch angefragte aber "vergessene" Nachricht wird
% in einer Datei geloggt.
redakteur(Delay, Server, ClientName, LogFile) ->
    % 1
    redakteurLoop(Delay, Server, ClientName, LogFile, 5),
    % 8 & 9
    NNr = getNNr(Server, ClientName, LogFile),
    Now = now2string(erlang:timestamp()),
    % 10
    logging(LogFile, format("~Bte_Nachricht um ~svergessen zu senden ******", [NNr, Now])).

% Hilfsfunktion für redakteur/4.
% Stellt die Schleife im Entwurf dar.
redakteurLoop(_Delay, _Server, _ClientName, _LogFile, 0) ->
    ok;
redakteurLoop(Delay, Server, ClientName, LogFile, Counter) ->
    % 2 & 3
    NNr = getNNr(Server, ClientName, LogFile),
    % 4
    TSclientout = erlang:timestamp(),
    % 5
    Msg = format("~s: ~Bte_Nachricht. C Out: ~s~n", [ClientName, NNr, now2string(TSclientout)]),
    % 6
    Server ! {dropmessage, [NNr, Msg, TSclientout]},
    % 7
    meinSleep(Delay * 1000),
    redakteurLoop(Delay, Server, ClientName, LogFile, Counter - 1).

% Hilfsfunktion für redakteur/4.
% Fragt eine eindeutige Nachrichtennummer beim Server an.
getNNr(Server, ClientName, LogFile) ->
    Server ! {self(), getmsgid},
    receive
        {nid, NNr} ->
            NNr;
        {terminateClient} ->
            logging(LogFile, format("~s nach Ablauf seiner Lifetime terminiert.~n", [ClientName])),
            exit(normal)
    after 7000 ->
        logging(
            LogFile,
            format("~s aufgrund eines Fehlers terminiert. Keine Antwort vom Server erhalten.~n", [
                ClientName
            ])
        ),
        exit(normal)
    end.

% Vergrößert oder verkleinert zufällig das übergebene Delay um 50%.
% Hierbei wird jedoch nie ein Minimum von 2 Sekunden unterschritten.
randomizeDelay(Delay) ->
    % 3
    [Random] = randomliste(1, 0, 1),
    % 1 & 4
    if
        (0.5 * Delay) < 2 ->
            % 2
            Delay * 1.5;
        true ->
            % 3
            if
                Random == 1 -> Delay * 1.5;
                true -> Delay * 0.5
            end
    end.

% Liest Nachrichten vom Server und loggt diese in einer Datei. Außerdem werden die erhaltenen Nachrichten um Informationen zu folgenden Ereignissen ergänzt:
% - die Nachricht stammt vom eigenen Redakteur ("*******")
% - die Nachricht kommt "aus der Zukunft" 
% - die Nachricht wurde zum wiederholten Mal erhalten
leser(RMEM, Server, ClientName, LogFile) ->
    % 1
    leserLoop(RMEM, Server, ClientName, LogFile).

leserLoop(RMEM, Server, ClientName, LogFile) ->
    {Message, Terminated} = getNewMessage(Server, ClientName, LogFile),
    [NNr, Msg, TSclientout, TShbqin, _TSdlqin, TSdlqout] = Message,
    TSclientin = erlang:timestamp(),
    {Repetition, Amount, NewRMEM} = updateReadMsgMEM(RMEM, NNr),
    % Andere Schritte überspringen, wenn Nachricht zum wiederholten Mal empfangen wird.
    MsgString =
        if
            Repetition ->
                format(">>>Wiederholung<<<: Nummer ~B zum ~B-ten mal erhalten.", [NNr, Amount]);
            true ->
                IsEigenerRedakteur = isEigenerRedakteur(Msg, ClientName),
                NewMsg =
                    if
                        IsEigenerRedakteur -> Msg ++ "*******";
                        true -> Msg
                    end,
                checkFuture(NewMsg, TSclientout, TShbqin, TSdlqout, TSclientin, LogFile, NNr)
        end,
    logging(LogFile, format("~s ; C In: ~s", [MsgString, now2string(TSclientin)])),
    if
        Terminated ->
            logging(LogFile, "Leser terminiert~n");
        true ->
            leserLoop(NewRMEM, Server, ClientName, LogFile)
    end,
    NewRMEM.

getNewMessage(Server, ClientName, LogFile) ->
    Server ! {self(), getmessages},
    receive
        {reply, Message, Terminated} ->
            {Message, Terminated};
        {terminateClient} ->
            logging(LogFile, format("~s nach Ablauf seiner Lifetime terminiert.~n", [ClientName])),
            exit(normal)
    after 7000 ->
        logging(
            LogFile,
            format("~s aufgrund eines Fehlers terminiert. Keine Antwort vom Server erhalten.~n", [
                ClientName
            ])
        ),
        exit(normal)
    end.

isEigenerRedakteur(MsgString, ClientName) ->
    Name = parseUntilChar(MsgString, ":"),
    if
        Name == ClientName -> true;
        true -> false
    end.

parseUntilChar(String, SplitChar) ->
    parseLogic(String, hd(SplitChar)).

parseLogic([], _SplitChar) ->
    [];
parseLogic([SplitChar | _T], SplitChar) ->
    [];
parseLogic([H | T], SplitChar) ->
    [H | parseLogic(T, SplitChar)].

checkFuture(MsgString, TSclientout, TShbqin, TSdlqout, TSclientin, LogFile, NNr) ->
    case validateTimestamps(TSclientout, TShbqin, TSdlqout, TSclientin) of
        {false, false, _, _} ->
            logging(
                LogFile,
                format(
                    "Nachricht #~B: Ungueltige Zeitstempel. Ueberpruefung fuer Nachricht aus "
                    "der Zukunft beim Server kann nicht durchgeführt werden.",
                    [NNr]
                )
            ),
            MsgString;
        {_, _, false, false} ->
            logging(
                LogFile,
                format(
                    "Nachricht #~B: Ungueltige Zeitstempel. Ueberpruefung fuer Nachricht aus "
                    "der Zukunft beim Leser kann nicht durchgeführt werden.",
                    [NNr]
                )
            ),
            MsgString;
        _AllTrue ->
            TimeDifferenceServer = diffTS(TShbqin, TSclientout),
            TDServerInS = tsToSeconds(TimeDifferenceServer),
            TimeDifferenceLeser = diffTS(TSclientin, TSdlqout),
            TDLeserInS = tsToSeconds(TimeDifferenceLeser),
            ZukunftServer = ">**Nachricht aus der Zukunft fuer Server:",
            ZukunftLeser = ">**Nachricht aus der Zukunft fuer Leser:",
            NewMsgString =
                if
                    TDServerInS < 0 ->
                        MsgString ++ ZukunftServer ++ now2string(TimeDifferenceServer);
                    true ->
                        MsgString
                end,
            if
                TDLeserInS < 0 ->
                    NewMsgString ++ ZukunftLeser ++ now2string(TimeDifferenceLeser);
                true ->
                    NewMsgString
            end
    end.

% Hilfsfunktion für checkFuture/7.
% Wandelt einen Zeitstempel in eine Zeit in Sekunden um.
tsToSeconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.

validateTimestamps(TS1, TS2, TS3, TS4) ->
    {validTS(TS1), validTS(TS2), validTS(TS3), validTS(TS4)}.

updateReadMsgMEM(RMEM, NNr) ->
    {Bool, Amount} = readFromRMEM(RMEM, NNr),
    NewRMEM = updateRMEM(RMEM, NNr),
    {Bool, Amount, NewRMEM}.

updateRMEM([], NNr) ->
    [{NNr, 1}];
updateRMEM([{NNr, Amount} | T], NNr) ->
    [{NNr, Amount + 1} | T];
updateRMEM([H | T], NNr) ->
    [H | updateRMEM(T, NNr)].

readFromRMEM([], _NNr) ->
    {false, 0};
readFromRMEM([{NNr, Amount} | _T], NNr) ->
    {true, Amount};
readFromRMEM([_H | T], NNr) ->
    readFromRMEM(T, NNr).
