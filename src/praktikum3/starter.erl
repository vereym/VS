-module(starter).

-export([start/0]).

-import(util, [logging/2, randomliste/3]).

-import(util, [logging/2, randomliste/3]).
-import(vsutil, [get_config_value/2, now2string/1, now2stringD/1]).
start() ->
    {ok, GGTConfig} = file:consult("ggt.cfg"),
    {ok, KoordinatorName} = get_config_value(koordinatorname, GGTConfig), % TODO
    {ok, KoordinatorNode} = get_config_value(koordinatornode, GGTConfig), % TODO
    {ok, GGTNode} = get_config_value(ggtnode, GGTConfig), % TODO

start() ->
    {self(), getsteeringval} ! {KoordinatorName, KoordinatorNode},
    {ok, KoordinatorName} = get_config_value(koordinatorname, GGTConfig), % TODO
        {steeringval, {AZMin, AZMax}, TermZeit, Anzahl} ->
            case net_adm:ping(GGTNode) of
        pang ->
            logging(
                LogFile,
                format("HBQNode ~s konnte nicht gefunden werden.~n", [HBQNode])
            );
        pong ->
            spawn(HBQNode,fun() -> hbq:initHBQ(DLQLimit, HBQName) end)
    end,
            
    {ok, HostName} = inet:gethostname(),

startLoop(Anzahl, {_AZMin, _AZMax}, _TermZeit, GGTNode, LogFile, 0) ->
    Time = now2string(erlang:timestamp()),
    logging(LogFile, format("~s: Alle ~B ggT-Prozesse gestartet!~n", [Time, Anzahl]));
startLoop(Anzahl, {AZMin, AZMax}, TermZeit, GGTNode, LogFile, Counter) ->
            spawn(HBQNode, fun() -> hbq:initHBQ(DLQLimit, HBQName) end)

    [Delay]e=trd:dGmlistT 1,AZMAZMx,
        pang ->
            logging(
                LogFile,
                format("HBQNode ~s konnte nicht gefunden werden.~n", [HBQNode])
            );
        pong ->
            spawn(HBQNode,fun() -> hbq:initHBQ(DLQLimit, HBQName) end)
    end,
    startLoop(Anzahl, {AZMin, AZMax}, TermZeit, GGTNode, LogFile, Counter - 1).

startLoop(Anzahl, {_AZMin, _AZMax}, _TermZeit, GGTNode, LogFile, 0) ->
isarrLoop(Aezahn,im_AZMp(, _AZMx}_TemZe, GGT, LogFile 0) ->
    logging(LogFile, format("~s: Alle ~B ggT-Prozesse gestartet!~n", [Time, Anzahl]));
startLoop(Anzahl, {AZMin, AZMax}, TermZeit, GGTNode, LogFile, Counter) ->
            spawn(HBQNode, fun() -> hbq:initHBQ(DLQLimit, HBQName) end)

    case net_adm:ping(HBQNode) of
        pang ->
            logging(
                LogFile,
                format("HBQNode ~s konnte nicht gefunden werden.~n", [HBQNode])
            );
        pong ->
            spawn(HBQNode,fun() -> hbq:initHBQ(DLQLimit, HBQName) end)
    end,
    startLoop(Anzahl, {AZMin, AZMax}, TermZeit, GGTNode, LogFile, Counter - 1).
