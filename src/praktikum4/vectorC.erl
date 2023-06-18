-module(vectorC).

-export([initVT/0]).

-import(vsutil, [get_config_value/2, now2string/1]).
-import(util, [logging/2]).
-import(io_lib, [format/2, format/1]).

initVT() ->
    {ok, HostName} = inet:gethostname(),
    LogFile = format("vectorC@~s.log", [HostName]),

    %% 4.1. Auslesen von servername und servernode aus der Konfigureationsdatei
    TowerClockConfig =
        case file:consult("towerClock.cfg") of
            {ok, File} ->
                File;
            {error, Reason} ->
                io:format("~nProblem mit towerClock.cfg: ~s~n", [Reason]),
                exit(bad_config)
        end,
    {ok, ServerName} = get_config_value(servername, TowerClockConfig),
    {ok, ServerNode} = get_config_value(servernode, TowerClockConfig),

    %% 4.2.
    {ok, PID} =
        case net_adm:ping(ServerNode) of
            pang ->
                ErrReason = format("towerClock konnte nicht gefunden werden~n", []),
                logging(LogFile, ErrReason),
                {error, ErrReason};
            pong ->
                {ServerName, ServerNode} ! {getVecID, self()},
                receive
                    Anwser ->
                        {ok, Anwser}
                after 3000 ->
                    {error, format("keine Antwort von towerClock erhalten")}
                end
        end,

    %% 4.3.
    towerClock ! {getVecID,self()},
    receive
        Any -> Any ++ todo
    end,
    register(ServerName, PID).
