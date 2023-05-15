-module(starter).

-export([start/1]).

-import(util, [logging/2, randomliste/3]).
-import(vsutil, [get_config_value/2, now2string/1, now2stringD/1]).
-import(io_lib, [format/2]).

start(StarterNum) ->
    {ok, GGTConfig} = file:consult("ggt.cfg"),
    {ok, NameServiceNode} = get_config_value(nameservicenode, GGTConfig),
    {ok, KoordinatorName} = get_config_value(koordinatorname, GGTConfig),
    {ok, Gruppe} = get_config_value(praktikumsgruppe, GGTConfig),
    {ok, Team} = get_config_value(teamnummer, GGTConfig),
    {ok, HostName} = inet:gethostname(),
    LogFile = format("Starter@~s", [HostName]),

    case net_adm:ping(NameServiceNode) of
        pang ->
            logging(LogFile, format("Nameservice-Node konnte nicht gefunden werden~n", []));
        pong ->
            NameService = global:whereis_name(nameservice),
            NameService ! {self(), {lookup, KoordinatorName}},
            Koordinator = receive
                {pin, {Name, Node}} -> {Name, Node}
            end,
            Koordinator ! {self(), getsteeringval},
            receive
                {steeringval, {AZMin, AZMax}, TermZeit, Anzahl} ->
                    startLoop(Anzahl, {AZMin, AZMax}, TermZeit, StarterNum, Gruppe, Team, NameService, Koordinator, LogFile, Anzahl)
            end
    end.

startLoop(Anzahl, {_AZMin, _AZMax}, _TermZeit, _StarterNum, _Gruppe, _Team, _NameService, _Koordinator, LogFile, 0) ->
    Time = now2string(erlang:timestamp()),
    logging(LogFile, format("~s: Alle ~B ggT-Prozesse gestartet!~n", [Time, Anzahl]));
startLoop(Anzahl, {AZMin, AZMax}, TermZeit, StarterNum, Gruppe, Team, NameService, Koordinator, LogFile, Counter) ->
    [Delay] = randomliste(1, AZMin, AZMin),
    GGTNum = Anzahl - Counter,
    spawn(fun() -> ggt:start(Delay, TermZeit, GGTNum, StarterNum, Gruppe, Team, NameService, Koordinator) end),

    startLoop(Anzahl, {AZMin, AZMax}, TermZeit, StarterNum, Gruppe, Team, NameService, Koordinator, LogFile, Counter - 1).
