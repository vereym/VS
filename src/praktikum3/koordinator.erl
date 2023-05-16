-module(koordinator).

-export([start/0]).

-import(vsutil, [get_config_value/2, now2string/1, now2stringD/1]).
-import(util, [logging/2]).
-import(io_lib, [format/2, format/1]).

start() ->
    {ok, HostName} = inet:gethostname(),
    LogFile = format("koordinator@~s", [HostName]),

    %% config auslesen
    {ok, KoordinatorConfig} = file:consult("koordinator.cfg"),
    {ok, NameServiceNode} = get_config_value(nameservicenode, KoordinatorConfig),
    {ok, GGTProzessanzahl} = get_config_value(ggtprozessanzahl, KoordinatorConfig),
    {ok, TermZeit} = get_config_value(arbeitszeit, KoordinatorConfig),
    {ok, KoordinatorNode} = get_config_value(termzeit, KoordinatorConfig),
    {ok, Koorigieren} = get_config_value(korrigieren, KoordinatorConfig),

    NameService =
        case net_adm:ping(NameServiceNode) of
            pang ->
                logging(LogFile, format("nameservice konnte nicht gefunden werden~n"));
            pong ->
                global:whereis_name(nameservice)
        end,

    spawn(fun() ->
             loop([NameService, GGTProzessanzahl, TermZeit, KoordinatorNode, Koorigieren, LogFile])
          end),
    ok.

loop(X = [NameService,
          GGTProzessanzahl,
          TermZeit,
          KoordinatorNode,
          Koorigieren,
          LogFile]) ->
    receive
        {From, getsteeringval} ->
            %% AZMin, AZMax = simulierte Verzögerungszeit zur Berechnung in Sekunden %% TODO
            %% TermZeit = Wartezeit in Sekunden, bis eine Wahl für eine Terminierung initiiert wird
            %% GGTProzessnummer = Anzahl zu startender ggT-Prozesse
            From ! Msg = {steeringval, {AZMin = 1, AZMax = 3}, TermZeit, GGTProzessanzahl},
            logging(LogFile, format("~p an ~p geschickt~n", [Msg, From])),
            loop(X);
        {hello, Clientname} ->
            ok,
            loop(X);
        {briefmi, {Clientname, CMi, CZeit}} ->
            ok,
            loop(X);
        {From, briefterm, {Clientname, CMi, CZeit}} ->
            ok,
            loop(X);
        {getinit, From} ->
            ok,
            loop(X);
        {calc, WggT} ->
            ok,
            loop(X);
        reset ->
            ok,
            loop(X);
        step ->
            ok,
            loop(X);
        prompt ->
            ok,
            loop(X);
        nudge ->
            ok,
            loop(X);
        toggle ->
            ok,
            loop(X);
        toggle_ggt ->
            ok,
            loop(X);
        kill ->
            ok,
            loop(X)
    end.
