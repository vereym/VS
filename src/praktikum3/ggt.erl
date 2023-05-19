-module(ggt).
-export([start/8]).
-import(util, [logging/2, randomliste/3]).
-import(vsutil, [get_config_value/2, now2string/1]).
-import(io_lib, [format/2]).

% TODO: 17-23

start(Delay, TermZeit, GGTNum, StarterNum, Gruppe, Team, NameService, Koordinator) ->
    % 15
    GGTName = list_to_atom(format('~B~B~B~B', [Gruppe, Team, GGTNum, StarterNum])),
    {ok, HostName} = inet:gethostname(),
    LogFile = format("GGTP_~s@~s", [GGTName, HostName]),
    % 16
    register(GGTName, self()),
    NameService ! {self(), {rebind, GGTName, node()}},
    receive
        ok_overwrite ->
            logging(LogFile, format("Erneut beim Namensdienst registriert!~n", []));
        ok_new ->
            logging(LogFile, format("Erfolgreich beim Namensdienst registriert!~n", []))
    end,
    Koordinator ! {self(), hello},
    Neighbors =
        receive
            {setneighbors, LeftN, RightN} ->
                {LeftN, RightN}
        end,
    receive
        % 17
        {setpm, MiNeu} ->
            loop([Delay, TermZeit, GGTName, NameService, Koordinator, Neighbors], MiNeu)
    end.

loop(Constants = [Delay, TermZeit, GGTName, NameService, Koordinator, Neighbors], Mi) ->
    receive
        % 17
        {setpm, MiNeu} ->
            loop(Constants, MiNeu);
        {sendy, Y} ->
            loop(Constants, Mi);
        {calc, start} ->
            loop(Constants, Mi);
        {From, {vote, Initiator, MiIn}} ->
            loop(Constants, Mi);
        {voteYes, Name} ->
            loop(Constants, Mi);
        {From, toggle} ->
            loop(Constants, Mi);
        {From, tellmi} ->
            loop(Constants, Mi);
        {From, pingGGT} ->
            loop(Constants, Mi);
        kill ->
            ok
    after TermZeit * 1000 ->
        % TODO properly terminate
        pass
    end.
