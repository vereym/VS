-module(koordinator).

-export([start/0]).

start() ->
    {ok, GGTConfig} = file:consult("koordinator.cfg"),
    {ok, KoordinatorName} = get_config_value(koordinatorname, GGTConfig), % TODO
    {ok, KoordinatorNode} = get_config_value(koordinatornode, GGTConfig), % TODO
    {ok, GGTNode} = get_config_value(ggtnode, GGTConfig), % TODO
    spawn(fun() -> loop([]) end),
    ok.

loop(X) ->
    receive
        {From, getsteeringval} ->
            %% AZMin, AZMax = simulierte Verzögerungszeit zur Berechnung in Sekunden
            %% TermZeit = Wartezeit in Sekunden, bis eine Wahl für eine Terminierung initiiert wird
            %% GGTProzessnummer = Anzahl zu startender ggT-Prozesse
            From ! {steeringval, {AZMin = 1, AZMax = 3}, TermZeit = 1, Anzahl = 1},
            ok,
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
