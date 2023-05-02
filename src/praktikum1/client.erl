-module(client).

-export([start/0]).

-import(util, [logging/2, writelist/2, randomliste/3]).
-import(vsutil, [get_config_value/2, now2string/1, now2stringD/1, meinSleep/1, validTS/1, diffTS/2]).
-import(io_lib, [format/2]).

start() ->
    % config auslesen
    {ok, ClientConfig} = file:consult("client.cfg"),
    {ok, Clients} = get_config_value(clients, ClientConfig),
    {ok, LifeTime} = get_config_value(lifetime, ClientConfig),
    {ok, SendeIntervall} = get_config_value(sendeintervall, ClientConfig),
    {ok, ServerName} = get_config_value(servername, ClientConfig),
    {ok, ServerNode} = get_config_value(servernode, ClientConfig),

    startClients(Clients, LifeTime, SendeIntervall, ServerName, ServerNode, Clients).

startClients(Clients, _LifeTime, _SendeIntervall, _ServerName, _ServerNode, 0) ->
    logging(
        "ClientStarter.log",
        format("~s: Alle ~p Clients erfolgreich gestartet!~n", [
            now2string(erlang:timestamp()), Clients
        ])
    );
startClients(Clients, LifeTime, SendeIntervall, ServerName, ServerNode, Counter) ->
    {IntervalMin, IntervalMax} = SendeIntervall,
    [Delay] = randomliste(1, IntervalMin, IntervalMax),
    {ok, HostName} = inet:gethostname(),
    % Gruppe 2 Team 10
    ClientName = format("Client~p@~s210", [Clients - Counter, HostName]),
    LogFile = format("~s.log", [ClientName]),
    spawn(fun() -> startClient(LifeTime, Delay, ServerName, ServerNode, ClientName, LogFile) end),
    startClients(Clients, LifeTime, SendeIntervall, ServerName, ServerNode, Counter - 1).

startClient(LifeTime, Delay, ServerName, ServerNode, ClientName, LogFile) ->
    case net_adm:ping(ServerNode) of
        pang ->
            logging(
                LogFile,
                format("Server-Node ~s konnte nicht gefunden werden.~n", [ServerNode])
            );
        pong ->
            erlang:send_after(LifeTime * 1000, self(), {terminateClient}),
            loop([], LifeTime, Delay, {ServerName, ServerNode}, ClientName, LogFile)
    end.

loop(RMEM, LifeTime, Delay, Server, ClientName, LogFile) ->
    %logging(LogFile, format("~p, ~p, ~p, ~s~n", [LifeTime, Delay, Server, ClientName])),
    redakteur(Delay, Server, ClientName, LogFile),
    NewRMEM = leser(RMEM, Server, ClientName, LogFile),
    NewDelay = randomizeDelay(Delay),
    loop(NewRMEM, LifeTime, NewDelay, Server, ClientName, LogFile).

redakteur(Delay, Server, ClientName, LogFile) ->
    redakteurLoop(Delay, Server, ClientName, LogFile, 5),
    NNr = getNNr(Server, ClientName, LogFile),
    Now = now2string(erlang:timestamp()),
    logging(LogFile, format("~pte_Nachricht um ~svergessen zu senden ******", [NNr, Now])).

redakteurLoop(_Delay, _Server, _ClientName, _LogFile, 0) ->
    ok;
redakteurLoop(Delay, Server, ClientName, LogFile, Counter) ->
    NNr = getNNr(Server, ClientName, LogFile),
    TSclientout = erlang:timestamp(),
    Msg = format("~s: ~pte_Nachricht. C Out: ~s~n", [ClientName, NNr, now2string(TSclientout)]),
    Server ! {dropmessage, [NNr, Msg, TSclientout]},
    meinSleep(Delay * 1000),
    redakteurLoop(Delay, Server, ClientName, LogFile, Counter - 1).

getNNr(Server, ClientName, LogFile) ->
    Server ! {self(), getmsgid},
    receive
        {nid, NNr} ->
            NNr;
        {terminateClient} ->
            logging(LogFile, format("~s nach Ablauf seiner Lifetime terminiert.~n", [ClientName])),
            exit(normal)
    after 3000 ->
        logging(
            LogFile,
            format("~s aufgrund eines Fehlers terminiert. Keine Antwort vom Server erhalten.~n", [
                ClientName
            ])
        ),
        exit(normal)
    end.

randomizeDelay(Delay) ->
    [Random] = randomliste(1, 0, 1),
    Return =
        if
            (0.5 * Delay) < 2 ->
                Delay * 1.5;
            true ->
                if
                    Random == 1 -> Delay * 1.5;
                    true -> Delay * 0.5
                end
        end,
    Return.

leser(RMEM, Server, ClientName, LogFile) ->
    leserLoop(RMEM, Server, ClientName, LogFile),
    RMEM.

leserLoop(RMEM, Server, ClientName, LogFile) ->
    {Message, Terminated} = getNewMessage(Server, ClientName, LogFile),
    [NNr, Msg, TSclientout, TShbqin, _TSdlqin, TSdlqout] = Message,
    TSclientin = erlang:timestamp(),
    {Repetition, Amount, NewRMEM} = updateReadMsgMEM(RMEM, NNr),
    % andere Schritte überspringen, wenn Nachricht zum wiederholten Mal empfangen wird
    MsgString =
        if
            Repetition ->
                format(">>>Wiederholung<<<: Nummer ~p zum ~p-ten mal erhalten.", [NNr, Amount]);
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
            leserLoop(NewRMEM, Server, ClientName, LogFile);
        true ->
            logging(LogFile, "Leser terminiert~n")
    end.

getNewMessage(Server, ClientName, LogFile) ->
    Server ! {self(), getmessages},
    receive
        {reply, Message, Terminated} ->
            {Message, Terminated};
        {terminateClient} ->
            logging(LogFile, format("~s nach Ablauf seiner Lifetime terminiert.~n", [ClientName])),
            exit(normal)
    after 3000 ->
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
                    "Nachricht #~p: Ungueltige Zeitstempel. Ueberpruefung fuer Nachricht aus "
                    "der Zukunft beim Server kann nicht durchgeführt werden.",
                    [NNr]
                )
            ),
            MsgString;
        {_, _, false, false} ->
            logging(
                LogFile,
                format(
                    "Nachricht #~p: Ungueltige Zeitstempel. Ueberpruefung fuer Nachricht aus "
                    "der Zukunft beim Leser kann nicht durchgeführt werden.",
                    [NNr]
                )
            ),
            MsgString;
        _AllTrue ->
            TimeDifferenceServer = diffTS(TShbqin, TSclientout),
            TimeDifferenceLeser = diffTS(TSclientin, TSdlqout),
            ZukunftServer = ">**Nachricht aus der Zukunft fuer Server:",
            ZukunftLeser = ">**Nachricht aus der Zukunft fuer Leser:",
            NewMsgString =
                if
                    TimeDifferenceServer < 0 ->
                        MsgString ++ ZukunftServer ++ now2string(TimeDifferenceServer);
                    true ->
                        MsgString
                end,
            if
                TimeDifferenceLeser < 0 ->
                    NewMsgString ++ ZukunftLeser ++ now2string(TimeDifferenceLeser);
                true ->
                    NewMsgString
            end
    end.

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
