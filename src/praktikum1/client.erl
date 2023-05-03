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
            format("~s aufgrund eines Fehlers terminiert. Keine Antwort vom Server erhalten (Redakteur).~n", [
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

% Hilfsfunktion für leser/4.
% Stellt die Schleife im Entwurf dar.
leserLoop(RMEM, Server, ClientName, LogFile) ->
    % 2 & 3
    {Message, Terminated} = getNewMessage(Server, ClientName, LogFile),
    % 4
    [NNr, Msg, TSclientout, TShbqin, _TSdlqin, TSdlqout] = Message,
    % 8
    TSclientin = erlang:timestamp(),
    % 14
    {Repetition, Amount, NewRMEM} = updateReadMsgMEM(RMEM, NNr),
    % Andere Schritte überspringen, wenn Nachricht zum wiederholten Mal empfangen wird.
    MsgString =
        % 15
        if
            Repetition ->
                % 16
                format(">>>Wiederholung<<<: Nummer ~B zum ~B-ten mal erhalten.", [NNr, Amount]);
            true ->
                % 5 & 6
                IsEigenerRedakteur = isEigenerRedakteur(Msg, ClientName),
                NewMsg = %Msg,
                    if
                        % 7
                        IsEigenerRedakteur -> Msg ++ "*******";
                        true -> Msg
                    end,
                % 10 - 13
                checkFuture(NewMsg, TSclientout, TShbqin, TSdlqout, TSclientin, LogFile, NNr)
        end,
    % 17                                                 9 & 16
    logging(LogFile, format("~s ; C In: ~s", [MsgString, now2string(TSclientin)])),
    % 1
    if
        Terminated ->
            logging(LogFile, "Leser terminiert~n");
        true ->
            leserLoop(NewRMEM, Server, ClientName, LogFile)
    end,
    % 18
    NewRMEM.

% Hilfsfunktion für leser/4.
% Fragt eine neue Nachricht beim Server an.
getNewMessage(Server, ClientName, LogFile) ->
    % 2
    Server ! {self(), getmessages},
    receive
        % 3
        {reply, Message, Terminated} ->
            {Message, Terminated};
        {terminateClient} ->
            logging(LogFile, format("~s nach Ablauf seiner Lifetime terminiert.~n", [ClientName])),
            exit(normal)
    after 7000 ->
        logging(
            LogFile,
            format("~s aufgrund eines Fehlers terminiert. Keine Antwort vom Server erhalten. (Leser)~n", [
                ClientName
            ])
        ),
        exit(normal)
    end.

% Hilfsfunktion für leserLoop/4.
% Überprüft, ob eine erhaltene Nachricht vom eigenen Redakteur stammt.
isEigenerRedakteur(MsgString, ClientName) ->
    % 5
    Name = parseUntilChar(MsgString, ":"),
    % 6
    if
        Name == ClientName -> true;
        true -> false
    end.

% Hilfsfunktion für isEigenerRedakteur/2.
% Parsed einen String und gibt alle Character bis zu einem übergebenen Symbol zurück.
parseUntilChar(String, SplitChar) ->
    parseLogic(String, hd(SplitChar)).

% Hilfsfunktion für parseUntilChar/2.
parseLogic([], _SplitChar) ->
    [];
parseLogic([SplitChar | _T], SplitChar) ->
    [];
parseLogic([H | T], SplitChar) ->
    [H | parseLogic(T, SplitChar)].

% Hilfsfunktion für leserLoop/4.
% Überprüft, ob eine Nachricht aus Zukunft erhalten wurde und macht ggf. Anpassungen an diese.
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
            % 10
            TimeDifferenceServer = diffTS(TShbqin, TSclientout),
            TDServerInS = tsToSeconds(TimeDifferenceServer),
            % 12
            TimeDifferenceLeser = diffTS(TSclientin, TSdlqout),
            TDLeserInS = tsToSeconds(TimeDifferenceLeser),
            % 11
            ZukunftServer = ">**Nachricht aus der Zukunft fuer Server:",
            % 13
            ZukunftLeser = ">**Nachricht aus der Zukunft fuer Leser:",
            NewMsgString =
                % 10
                if
                    TDServerInS < 0 ->
                        % 11
                        MsgString ++ ZukunftServer ++ now2string(TimeDifferenceServer);
                    true ->
                        MsgString
                end,
            if
                % 12
                TDLeserInS < 0 ->
                    % 13
                    NewMsgString ++ ZukunftLeser ++ now2string(TimeDifferenceLeser);
                true ->
                    NewMsgString
            end
    end.

% Hilfsfunktion für checkFuture/7.
% Wandelt einen Zeitstempel in eine Zeit in Sekunden um.
tsToSeconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.

% Hilfsfunktion für checkFuture/7.
% Validiert die übergebenen Zeitstempel.
validateTimestamps(TS1, TS2, TS3, TS4) ->
    {validTS(TS1), validTS(TS2), validTS(TS3), validTS(TS4)}.

% Aktualisiert das ReadMsgMEM. Gibt zurück, ob dieübergebene NNr bereits im RMEM enthalten war,
% die Anzahl der Wiederholungen der angefragten Nachricht, sowie das aktualisierte RMEM.
updateReadMsgMEM(RMEM, NNr) ->
    %                1
    {Bool, Amount} = readFromRMEM(RMEM, NNr),
    %         1
    NewRMEM = updateRMEM(RMEM, NNr),
    % 8
    {Bool, Amount, NewRMEM}.

% Hilfsfunktion für updateReadMsgMEM/2.
% Prüft, ob eine übergebene Nachrichtennummer bereits erhalten wurde und wie oft dies geschehen ist.
%            2
readFromRMEM([], _NNr) ->
    % 7
    {false, 0};
%            2        3
readFromRMEM([{NNr, Amount} | _T], NNr) ->
    % 5
    {true, Amount};
readFromRMEM([_H | T], NNr) ->
    readFromRMEM(T, NNr).

% Hilfsfunktion für updateReadMsgMEM/2.
% Aktualisiert das RMEM für eine übergebene Nachrichtennummer.
%          2
updateRMEM([], NNr) ->
    % 6
    [{NNr, 1}];
%          2        3
updateRMEM([{NNr, Amount} | T], NNr) ->
    %          4
    [{NNr, Amount + 1} | T];
updateRMEM([H | T], NNr) ->
    [H | updateRMEM(T, NNr)].
