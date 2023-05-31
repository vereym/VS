-module(ggt).
-export([start/8]).
-import(util, [logging/2]).
-import(vsutil, [now2string/1]).
-import(io_lib, [format/2]).

start(Delay, TermZeit, GGTNum, StarterNum, Gruppe, Team, NameService, Koordinator) ->
    Korrigieren = true,
    % 1
    GGTName = list_to_atom(format('~B~B~B~B', [Gruppe, Team, GGTNum, StarterNum])),
    {ok, HostName} = inet:gethostname(),
    LogFile = format("GGTP_~s@~s.log", [GGTName, HostName]),
    % 2
    register(GGTName, self()),
    NameService ! {self(), {rebind, GGTName, node()}},
    Time = now2string(erlang:timestamp()),
    receive
        ok_overwrite ->
            logging(LogFile, format("~s: Erneut beim Namensdienst registriert.~n", [Time]));
        ok_new ->
            logging(
                LogFile,
                format("~s: Erfolgreich beim Namensdienst registriert.~n", [
                    now2string(erlang:timestamp())
                ])
            )
    end,
    % 3
    Koordinator ! {hello, GGTName},
    Time2 = now2string(erlang:timestamp()),
    logging(LogFile, format("~s: hello an den Koordinator gesendet.~n", [Time2])),
    % 4
    Neighbors =
        receive
            {setneighbors, LeftN, RightN} ->
                Time3 = now2string(erlang:timestamp()),
                logging(
                    LogFile, format("~s: Nachbarn vom Koordinator erhalten und gesetzt.~n", [Time3])
                ),
                {LeftN, RightN}
        end,
    loop_initial(
        [Delay, TermZeit, GGTName, NameService, Koordinator, Neighbors, LogFile], Korrigieren, 0, 0
    ).

loop_initial(
    Constants = [_Delay, _TermZeit, GGTName, NameService, Koordinator, _Neighbors, LogFile],
    Korrigieren,
    AnzahlTerm,
    Mi
) ->
    receive
        % 5
        {setpm, MiNeu} ->
            Time = now2string(erlang:timestamp()),
            logging(LogFile, format("~s: {setpm, ~B} erhalten.~n", [Time, MiNeu])),
            loop(Constants, Korrigieren, AnzahlTerm, MiNeu);
        % 6
        {calc, start} ->
            Koordinator ! {getinit, self()},
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format(
                    "~s: {calc, start} erhalten und initialen Wert beim Koordinator angefragt.~n", [
                        Time
                    ]
                )
            ),
            loop(Constants, Korrigieren, AnzahlTerm, Mi);
        % 10a
        {_From, toggle} ->
            Toggled =
                if
                    Korrigieren ->
                        false;
                    true ->
                        true
                end,
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format("~s: toggle-Befehl erhalten. Neuer Wert des Flags: ~s~n", [Time, Toggled])
            ),
            loop_initial(Constants, Toggled, AnzahlTerm, Mi);
        % 10b
        {From, tellmi} ->
            From ! {mi, Mi},
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format(
                    "~s: tellmi-Befehl erhalten. Anfragender Prozess ueber aktuelles Mi informiert.~n",
                    [Time]
                )
            ),
            loop_initial(Constants, Korrigieren, AnzahlTerm, Mi);
        % 10c
        {From, pingGGT} ->
            From ! {pongGGT, GGTName},
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format(
                    "~s: pingGGT-Befehl erhalten. pongGGT an den anfragenden Prozess gesendet.~n",
                    [Time]
                )
            ),
            loop_initial(Constants, Korrigieren, AnzahlTerm, Mi);
        % 11b
        {From, {vote, Initiator, MiIn}} ->
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile, format("~s: Terminierungsanfrage von ~s erhalten:~n", [Time, Initiator])
            ),
            doVote(Mi, MiIn, GGTName, Initiator, From, Korrigieren, LogFile),
            loop_initial(Constants, Korrigieren, AnzahlTerm, Mi);
        % 13
        kill ->
            NameService ! {self(), {unbind, GGTName}},
            receive
                ok ->
                    Time = now2string(erlang:timestamp()),
                    logging(LogFile, format("~s: Prozess erfolgreich terminiert!~n", [Time]))
            after 7000 ->
                Time = now2string(erlang:timestamp()),
                logging(
                    LogFile,
                    format(
                        "~s: Es konnte sich nicht beim Namensdienst abgemeldet werden. Prozess wird trotzdem beendet.~n",
                        [Time]
                    )
                )
            end
    end.

loop(
    Constants = [Delay, TermZeit, GGTName, NameService, Koordinator, _Neighbors, LogFile],
    Korrigieren,
    AnzahlTerm,
    Mi
) ->
    receive
        % 5
        {setpm, MiNeu} ->
            Time = now2string(erlang:timestamp()),
            logging(LogFile, format("~s: {setpm, ~B} erhalten.~n", [Time, MiNeu])),
            loop(Constants, Korrigieren, AnzahlTerm, MiNeu);
        % 6
        {calc, start} ->
            Koordinator ! {getinit, self()},
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format(
                    "~s: {calc, start} erhalten und initialen Wert beim Koordinator angefragt.~n", [
                        Time
                    ]
                )
            ),
            loop(Constants, Korrigieren, AnzahlTerm, Mi);
        % 7-9
        {sendy, Y} ->
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format("~s: {sendy, ~B} erhalten. ggT-Algorithmus wird ausgefuehrt:~n", [Time, Y])
            ),
            MiNeu = handleSendy(Mi, Y, Delay, GGTName, Koordinator, NameService, LogFile),
            loop(Constants, Korrigieren, AnzahlTerm, MiNeu);
        % 10a
        {_From, toggle} ->
            Toggled =
                if
                    Korrigieren ->
                        false;
                    true ->
                        true
                end,
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format("~s: toggle-Befehl erhalten. Neuer Wert des Flags: ~s~n", [Time, Toggled])
            ),
            loop(Constants, Toggled, AnzahlTerm, Mi);
        % 10b
        {From, tellmi} ->
            From ! {mi, Mi},
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format(
                    "~s: tellmi-Befehl erhalten. Anfragender Prozess ueber aktuelles Mi informiert.~n",
                    [Time]
                )
            ),
            loop(Constants, Korrigieren, AnzahlTerm, Mi);
        % 10c
        {From, pingGGT} ->
            From ! {pongGGT, GGTName},
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format(
                    "~s: pingGGT-Befehl erhalten. pongGGT an den anfragenden Prozess gesendet.~n",
                    [Time]
                )
            ),
            loop(Constants, Korrigieren, AnzahlTerm, Mi);
        % 11b
        {From, {vote, Initiator, MiIn}} ->
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile, format("~s: Terminierungsanfrage von ~s erhalten:~n", [Time, Initiator])
            ),
            doVote(Mi, MiIn, GGTName, Initiator, From, Korrigieren, LogFile),
            loop(Constants, Korrigieren, AnzahlTerm, Mi);
        % 13
        kill ->
            NameService ! {self(), {unbind, GGTName}},
            receive
                ok ->
                    Time = now2string(erlang:timestamp()),
                    logging(LogFile, format("~s: Prozess erfolgreich terminiert!~n", [Time]))
            after 7000 ->
                Time = now2string(erlang:timestamp()),
                logging(
                    LogFile,
                    format(
                        "~s: Es konnte sich nicht beim Namensdienst abgemeldet werden. Prozess wird trotzdem beendet.~n",
                        [Time]
                    )
                )
            end
        % 11
    after TermZeit * 1000 ->
        handleTermination(Constants, Korrigieren, AnzahlTerm, Mi)
    end.

% 7 & 8
handleSendy(Mi, Y, Delay, GGTName, Koordinator, NameService, LogFile) ->
    logging(LogFile, format("Mi: ~B | Y: ~B~n", [Mi, Y])),
    % 7 & 8
    Return =
        if
            % 7
            Y < Mi ->
                MiNeu = (Mi - 1) rem Y + 1,
                % 8a
                NameService ! {self(), {twocast, tell, MiNeu}},
                Time = erlang:timestamp(),
                % 8b
                Koordinator ! {briefmi, {GGTName, MiNeu, Time}},
                logging(
                    LogFile,
                    format(
                        "~s: Mi wurde angepasst (neuer Wert: ~B). Zwei andere ggT-Prozesse und Koordinator wurden informiert.~n",
                        [now2string(Time), MiNeu]
                    )
                ),
                MiNeu;
            true ->
                Time = now2string(erlang:timestamp()),
                logging(
                    LogFile,
                    format(
                        "~s: Mi wurde nicht angepasst.~n",
                        [Time]
                    )
                ),
                Mi
        end,
    % 9
    timer:sleep(Delay * 1000),
    Return.

handleTermination(
    Constants = [Delay, TermZeit, GGTName, NameService, Koordinator, {LeftN, RightN}, LogFile],
    Korrigieren,
    AnzahlTerm,
    Mi
) ->
    % 11a
    LeftN ! {self(), {vote, GGTName, Mi}},
    RightN ! {self(), {vote, GGTName, Mi}},
    % 11e
    {ok, Timer} = timer:send_after(TermZeit * 1000, {timeout}),
    receive
        % 5 & 11e
        {setpm, MiNeu} ->
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format("~s: {setpm, ~B} erhalten. Terminierungsabstimmung abgebrochen.~n", [
                    Time, MiNeu
                ])
            ),
            loop(Constants, Korrigieren, AnzahlTerm, MiNeu);
        % 7-9 & 11e
        {sendY, Y} ->
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format("~s: {sendy, ~B} erhalten. Terminierungsabstimmung abgebrochen:~n", [Time, Y])
            ),
            MiNeu = handleSendy(Mi, Y, Delay, GGTName, Koordinator, NameService, LogFile),
            loop(Constants, Korrigieren, AnzahlTerm, MiNeu);
        % 11e
        {timeout} ->
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format(
                    "~s: Keine voteYes-Antwort erhalten. Terminierungsabstimmung nicht erfolgreich.~n",
                    [
                        Time
                    ]
                )
            ),
            loop(Constants, Korrigieren, AnzahlTerm, Mi);
        % 11d
        {voteYes, VoteGGTName1} ->
            Time = now2string(erlang:timestamp()),
            logging(
                LogFile,
                format(
                    "~s: voteYes-Antwort von ~s erhalten.~n",
                    [
                        Time, VoteGGTName1
                    ]
                )
            ),
            receive
                % 5 & 11e
                {setpm, MiNeu} ->
                    Time2 = now2string(erlang:timestamp()),
                    logging(
                        LogFile,
                        format("~s: {setpm, ~B} erhalten. Terminierungsabstimmung abgebrochen.~n", [
                            Time2, MiNeu
                        ])
                    ),
                    loop(Constants, Korrigieren, AnzahlTerm, MiNeu);
                % 7-9 & 11e
                {sendY, Y} ->
                    Time2 = now2string(erlang:timestamp()),
                    logging(
                        LogFile,
                        format("~s: {sendy, ~B} erhalten. Terminierungsabstimmung abgebrochen:~n", [
                            Time2, Y
                        ])
                    ),
                    MiNeu = handleSendy(
                        Mi, Y, Delay, GGTName, Koordinator, NameService, LogFile
                    ),
                    loop(Constants, Korrigieren, AnzahlTerm, MiNeu);
                % 11e
                {timeout} ->
                    Time2 = now2string(erlang:timestamp()),
                    logging(
                        LogFile,
                        format(
                            "~s: Nur eine voteYes-Antwort erhalten. Terminierungsabstimmung nicht erfolgreich.~n",
                            [
                                Time2
                            ]
                        )
                    ),
                    loop(Constants, Korrigieren, AnzahlTerm, Mi);
                % 11d
                {voteYes, VoteGGTName2} ->
                    timer:cancel(Timer),
                    Time2 = erlang:timestamp(),
                    logging(
                        LogFile,
                        format(
                            "~s: voteYes-Antwort von ~s erhalten.~n",
                            [
                                now2string(Time2), VoteGGTName2
                            ]
                        )
                    ),
                    Koordinator ! {self(), briefterm, {GGTName, Mi, Time}},
                    % 12
                    AnzahlTermNeu = AnzahlTerm + 1,
                    logging(
                        LogFile,
                        format("~s: Terminierungsabstimmung #~B erfolgreich gemeldet!~n", [
                            now2string(Time2),
                            AnzahlTermNeu
                        ])
                    ),
                    loop_initial(Constants, Korrigieren, AnzahlTermNeu, Mi)
            end
    end.

% 11
doVote(Mi, MiIn, GGTName, InitiatorName, InitiatorPID, Korrigieren, LogFile) ->
    if
        % 11b
        Mi == MiIn ->
            InitiatorPID ! {voteYes, GGTName},
            logging(
                LogFile,
                format("   Mi-Werte stimmen ueberein. voteYes an ~s gesendet.~n", [
                    InitiatorName
                ])
            );
        true ->
            if
                % 11c
                Korrigieren and (Mi < MiIn) ->
                    InitiatorPID ! {sendY, Mi},
                    logging(
                        LogFile,
                        format(
                            "   Eigener Mi-Wert ist kleiner als erhaltenes MiIn. Korrigierend eingegriffen und ~s informiert.~n",
                            [InitiatorName]
                        )
                    );
                true ->
                    logging(
                        LogFile,
                        format(
                            "   Mi-Werte stimmen nicht ueberein. Es wurde ebenfalls nicht korrigierend eingegriffen.~n",
                            []
                        )
                    )
            end
    end.
