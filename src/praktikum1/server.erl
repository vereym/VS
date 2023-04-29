-module(server).

-export([start/0]).

-import(vsutil, [now2string/1]).
-import(hbq, [initHBQ/2]).
-import(cmem, [initCMEM/2]).

start() ->
    % config auslesen
    {ok, ServerConfig} = file:consult("server.cfg"),
    {ok, DLQLimit} = vsutil:get_config_value(dlqlimit, ServerConfig),
    {ok, HBQName} = vsutil:get_config_value(hbqname, ServerConfig),
    {ok, ServerName} = vsutil:get_config_value(servername, ServerConfig),
    {ok, RemTime} = vsutil:get_config_value(clientlifetime, ServerConfig),
    {ok, Latency} = vsutil:get_config_value(latency, ServerConfig),
    % lists:foreach(fun(S) -> io:write(S)end, [DLQLimit, HBQName, ServerName, RemTime, Latency]),
    HBQ = initHBQ(DLQLimit, HBQName),
    LogFile = "server_log.log",
    CMEM = initCMEM(RemTime, LogFile),
    ServerID = spawn(fun() -> loop(0, Latency, HBQ, CMEM, LogFile)end),
    register(ServerName, ServerID),
    ServerID.

loop(NNrCounter, Latency, HBQ, CMEM, LogFile) ->
    % 3 - neue Nachricht empfangen
    receive
        {_ClientID, getmessages} ->
            io:write([getmessages, "\n"]),
            % loop(0, Latency, HBQ, CMEM, LogFile),
            ok;
        {dropmessage, Message = [NNR, Msg, TSclientout]} ->
            io:write(Message),
            ok;
        {_ClientID, getmsgid} ->
            io:write(getmsgid),
            ok;
        {_ClientID, listDLQ} ->
            io:write(listDLQ),
            ok;
        {_ClientID, listHBQ} ->
            io:write(listHBQ),
            ok;
        {_ClientID, listCMEM} ->
            io:write(listCMEM),
            ok;
        _Any ->
            ok
    end,
    loop(NNrCounter, Latency, HBQ, CMEM, LogFile).

