-module(client).

-export([start/0, randomizeDelay/1]).

-import(util, [logging/2, writelist/2, randomliste/3]).
-import(vsutil, [get_config_value/2, now2string/1, now2stringD/1, meinSleep/1]).
-import(io_lib, [format/2]).

start() ->
    % config auslesen
    {ok, ClientConfig} = file:consult("client.cfg"),
    {ok, Clients} = get_config_value(clients, ClientConfig),
    {ok, LifeTime} = get_config_value(lifetime, ClientConfig),
    {ok, SendeIntervall} = get_config_value(sendeintervall, ClientConfig),
    {ok, ServerName} = get_config_value(servername, ClientConfig),
    {ok, ServerNode} = get_config_value(servernode, ClientConfig),
    
    Server = {ServerName, ServerNode},

    startClients(Clients, LifeTime, SendeIntervall, Server, Clients),

    case net_adm:ping(ServerNode) of
        pang -> nok; % Server Node konnte nicht gefunden werden
        pong -> startClients(Clients, LifeTime, SendeIntervall, Server, Clients)
    end.


startClients(_Clients, _LifeTime, _SendeIntervall, _Server, 0) ->
    ok;

startClients(Clients, LifeTime, SendeIntervall, Server, Counter) ->
    {IntervalMin, IntervalMax} = SendeIntervall,
    [Delay] = randomliste(1, IntervalMin, IntervalMax),
    {ok, HostName} = inet:gethostname(),
    ClientName = format("Client~p@~s210", [Clients - Counter, HostName]), % Gruppe 2 Team 10
    LogFile = format("~s.log", [ClientName]),
    spawn(fun() -> loop([], LifeTime, Delay, Server, ClientName, LogFile) end),
    startClients(Clients, LifeTime, SendeIntervall, Server, Counter - 1).


% TODO: terminieren nach LifeTime Sekunden + Testing
loop(RMEM, LifeTime, Delay, Server, ClientName, LogFile) ->
    logging(LogFile, format("~p, ~p, ~p, ~p~n", [LifeTime, Delay, Server, ClientName])),
    redakteur(Delay, Server, ClientName, LogFile),
    NewRMEM = leser(RMEM, Server, ClientName, LogFile),
    NewDelay = randomizeDelay(Delay),
    loop(NewRMEM, LifeTime, NewDelay, Server, ClientName, LogFile).


redakteur(Delay, Server, ClientName, LogFile) ->
    sendLoop(Delay, Server, ClientName, LogFile, 5),
    NNr = getNNr(Server, LogFile),
    Now = now2string(erlang:timestamp()),
    logging(LogFile, format("~pte_Nachricht um ~p|vergessen zu senden ******", [NNr, Now])).


sendLoop(_Delay, _Server, _ClientName, _LogFile, 0) ->
    ok;

sendLoop(Delay, Server, ClientName, LogFile, Counter) ->
    NNr = getNNr(Server, LogFile),
    TSclientout = erlang:timestamp(),
    Msg = format("~s: ~pte_Nachricht. C Out: ~p~n", [ClientName, NNr, now2string(TSclientout)]),
    Server ! {dropmessage, [NNr, Msg, TSclientout]},
    meinSleep(Delay * 1000),
    sendLoop(Delay, Server, ClientName, LogFile, Counter - 1).


% TODO fix unsafe receive
getNNr(Server, LogFile) ->
    Server ! {self(), getmsgid},
    NNr = receive
        {nid, NNr} -> NNr;
        Answer -> logging(LogFile, format("Unexpected answer from server: ~p~n", [Answer]))
    end,
    NNr.


randomizeDelay(Delay) ->
    [Random] = randomliste(1, 0, 1),
    Return = if (0.5 * Delay) < 2 -> Delay * 1.5;
            true -> if Random == 1 -> Delay * 1.5;
                    true -> Delay * 0.5
                    end
            end,
    Return.


% TODO Leser + ReadMsgMEM
leser(RMEM, Server, ClientName, LogFile) -> RMEM.
