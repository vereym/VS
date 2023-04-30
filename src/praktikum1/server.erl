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
    ServerID = spawn(fun() -> loop(0, Latency, HBQ, CMEM, LogFile) end),
    register(ServerName, ServerID),
    ServerID.

loop(NNrCounter, Latency, HBQ, CMEM, LogFile) ->
    % 3 - neue Nachricht empfangen
    receive
        {ClientID, getmessages} ->
            % TODO
            ClientNNr = getClientNNr(CMEM, ClientID),
            HBQ ! {self(), {request, deliverMSG, ClientNNr, ClientID}},
            receive
                ActualNNr ->
                    ActualNNr
            end,
            io:write(ActualNNr),
            io:write([getmessages, "\n"]),
            % loop(0, Latency, HBQ, CMEM, LogFile),
            ok;

        {dropmessage, Message = [_NNR, _Msg, _TSclientout]} ->
            io:write(Message),
            ok;

        {ClientID, getmsgid} ->
            io:write(getmsgid),
            ClientID ! {};

        % 16
        {_ClientID, listDLQ} ->
            io:write(listDLQ),
            Reply = send_msg(HBQ, {self(), {request, listHBQ}}),
            if Reply == {reply, ok} -> ok;
                true -> log(LogFile, Reply)
            end;

        % 19
        {_ClientID, listHBQ} ->
            io:write(listHBQ),
            Reply = send_msg(HBQ, {self(), {request, listHBQ}}),
            if Reply == {reply, ok} -> ok;
                true -> log(LogFile, Reply)
            end;

        % 22
        {_ClientID, listCMEM} ->
            io:write(listCMEM),
            log(LogFile, CMEM);

        Any ->
            log(LogFile, "Server hat " ++ [Any] ++ "empfangen, konnte damit jedoch nichts anfangen."),
            ok
    end,
    loop(NNrCounter, Latency, HBQ, CMEM, LogFile).

%% @doc log `Data` to `LogFile`
log(LogFile, Data) ->
    file:write_file(LogFile, Data, [append]),
    io:format(">: ~p~n", Data),
    ok.

getClientNNr(_, _) ->
    ok.

send_msg(PID, Message) ->
    PID ! Message,
    receive
        Any ->
            Any
    end.
