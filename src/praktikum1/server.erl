-module(server).

-export([start/0]).

-import(vsutil, [get_config_value/2, now2string/1, reset_timer/3]).
-import(hbq, [initHBQ/2]).
-import(cmem, [initCMEM/2, getClientNNr/2, updateClient/4, listCMEM/1]).
-import(util, [logging/2]).
-import(io_lib, [format/2]).

start() ->
    % config auslesen
    {ok, ServerConfig} = file:consult("server.cfg"),
    {ok, DLQLimit} = get_config_value(dlqlimit, ServerConfig),
    {ok, HBQName} = get_config_value(hbqname, ServerConfig),
    {ok, ServerName} = get_config_value(servername, ServerConfig),
    {ok, RemTime} = get_config_value(clientlifetime, ServerConfig),
    {ok, Latency} = get_config_value(latency, ServerConfig),

    HBQ = initHBQ(DLQLimit, HBQName),
    {ok, HostName} = inet:gethostname(),
    LogFile = format("Server@~s.log", [HostName]),
    CMEM = initCMEM(RemTime, LogFile),
    register(ServerName, self()),
    Timer = timer:send_after(Latency * 1000, {terminateServer}),
    loop(0, Latency, HBQ, CMEM, Timer, LogFile).

loop(NNrCounter, Latency, HBQ, CMEM, Timer, LogFile) ->
    % 3 - neue Nachricht empfangen
    receive
        {ClientID, getmessages} ->
            NewTimer = reset_timer(Timer, Latency, {terminateServer}),
            ClientNNr = getClientNNr(CMEM, ClientID),
            HBQ ! {self(), {request, deliverMSG, ClientNNr, ClientID}},
            ActualNNr =
                receive
                    {reply, SendNNr} ->
                        SendNNr;
                    Any ->
                        logging(
                            LogFile, format("Unerwartete Antwort von der HBQ erhalten: ~p~n", [Any])
                        ),
                        -1
                after 7000 ->
                    logging(LogFile, "Keine Antwort von der HBQ erhalten. Server terminiert"),
                    exit(normal)
                end,
            if
                ActualNNr == -1 -> NewCMEM = CMEM;
                true -> NewCMEM = updateClient(CMEM, ClientID, ActualNNr + 1, LogFile)
            end,
            loop(NNrCounter, Latency, HBQ, NewCMEM, NewTimer, LogFile);
        {dropmessage, Message = [_NNR, _Msg, _TSclientout]} ->
            NewTimer = reset_timer(Timer, Latency, {terminateServer}),
            send_msg(HBQ, {self(), {request, pushHBQ, Message}}, LogFile),
            loop(NNrCounter, Latency, HBQ, CMEM, NewTimer, LogFile);
        {ClientID, getmsgid} ->
            NewTimer = reset_timer(Timer, Latency, {terminateServer}),
            ClientID ! {nid, NNrCounter},
            loop(NNrCounter + 1, Latency, HBQ, CMEM, NewTimer, LogFile);
        % 16
        {_ClientID, listDLQ} ->
            NewTimer = reset_timer(Timer, Latency, {terminateServer}),
            send_msg(HBQ, {self(), {request, listDLQ}}, LogFile),
            loop(NNrCounter, Latency, HBQ, CMEM, NewTimer, LogFile);
        % 19
        {_ClientID, listHBQ} ->
            NewTimer = reset_timer(Timer, Latency, {terminateServer}),
            send_msg(HBQ, {self(), {request, listHBQ}}, LogFile),
            loop(NNrCounter, Latency, HBQ, CMEM, NewTimer, LogFile);
        % 22
        {_ClientID, listCMEM} ->
            NewTimer = reset_timer(Timer, Latency, {terminateServer}),
            logging(LogFile, format("~p~n", [listCMEM(CMEM)])),
            loop(NNrCounter, Latency, HBQ, CMEM, NewTimer, LogFile);
        {terminateServer} ->
            logging(LogFile, "Server nach Ablauf seiner Latency terminiert.~n"),
            exit(normal);
        Any ->
            logging(
                LogFile,
                format(
                    "Server hat folgende Nachricht empfangen, konnte damit jedoch nichts anfangen: ~p~n",
                    [Any]
                )
            ),
            ok
    end.

send_msg(PID, Message, LogFile) ->
    PID ! Message,
    receive
        {reply, ok} ->
            ok;
        Any ->
            logging(
                LogFile, format("Unerwartete Antwort von der HBQ erhalten: ~p~n", [Any])
            )
    after 7000 ->
        logging(LogFile, "Keine Antwort von der HBQ erhalten. Server terminiert"),
        exit(normal)
    end.
