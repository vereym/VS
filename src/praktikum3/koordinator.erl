-module(koordinator).

-export([start/0]).

-import(vsutil, [get_config_value/2, now2string/1]).
-import(util, [logging/2]).
-import(io_lib, [format/2, format/1]).

-define(stime, now2string(erlang:timestamp())).

start() ->
    {ok, HostName} = inet:gethostname(),
    LogFile = format("Koordinator@~s.log", [HostName]),

    %% 1. Auslesen der Konfigureationsdatei
    KoordinatorConfig =
        case file:consult("koordinator.cfg") of
            {ok, File} ->
                File;
            {error, Reason} ->
                io:format("~nProblem mit koordinator.cfg: ~s~n", [Reason]),
                exit(bad_config)
        end,

    {ok, Arbeitszeit} = get_config_value(arbeitszeit, KoordinatorConfig),
    {ok, TermZeit} = get_config_value(termzeit, KoordinatorConfig),
    {ok, GGTProzessanzahl} = get_config_value(ggtprozessanzahl, KoordinatorConfig),
    {ok, NameServiceNode} = get_config_value(nameservicenode, KoordinatorConfig),
    {ok, KoordinatorName} = get_config_value(koordinatorname, KoordinatorConfig),
    {ok, KorrigierenConfig} = get_config_value(korrigieren, KoordinatorConfig),
    Korrigieren =
        case KorrigierenConfig of
            0 ->
                false;
            1 ->
                true
        end,

    %% 2. bei lokalen Erlang-Node und Namensdienst registrieren
    PID = case net_adm:ping(NameServiceNode) of
              pang ->
                  logging(LogFile, format("nameservice konnte nicht gefunden werden~n", [])),
                  ok;
              pong ->
                  NameService = {nameservice, NameServiceNode},
                  nameservice_rebind(NameService, KoordinatorName, LogFile),
                  spawn(fun() ->
                           initial_state_loop({Arbeitszeit,
                                               TermZeit,
                                               GGTProzessanzahl,
                                               NameService,
                                               KoordinatorName},
                                              _State = {Korrigieren, infinity, [], 0},
                                              _GGTClients = [],
                                              LogFile)
                        end)
          end,
    register(KoordinatorName, PID).

-type state() ::
    {Korrigieren :: true | false,
     SmallestKnownNumber :: integer() | infinity,
     Mis :: list(),
     StarterCount :: integer()}.
-type params() ::
    {{integer(), integer()},
     TermZeit :: integer(),
     GGTProzessanzahl :: integer(),
     NameService :: {atom(), atom()},
     KoordinatorName :: atom()}.

%% @doc Bildet den bereit-Zustand ab.
initial_state_loop(Params =
                       {_Arbeitszeit = {AZMin, AZMax},
                        TermZeit,
                        GGTProzessanzahl,
                        NameService,
                        _KoordinatorName},
                   State = {Korrigieren, SmallestKnownNumber, Mis, StarterCount},
                   GGTClients,
                   LogFile) ->
    logging(LogFile, format("koordinator ist in initial_state_loop~n", [])),
    receive
        %% 3. den Startern Auskunft zu Steuerungswerten geben
        {From, getsteeringval} ->
            %% AZMin, AZMax = simulierte Verzögerungszeit zur Berechnung in Sekunden
            %% TermZeit = Wartezeit in Sekunden, bis eine Wahl für eine Terminierung initiiert wird
            %% GGTProzessanzahl = Anzahl zu startender ggT-Prozesse
            From ! Msg = {steeringval, {AZMin, AZMax}, TermZeit, GGTProzessanzahl},
            logging(LogFile, format("~p an ~p geschickt~n", [Msg, From])),
            initial_state_loop(Params,
                               {Korrigieren, SmallestKnownNumber, Mis, StarterCount + 1},
                               GGTClients,
                               LogFile);
        {hello, Clientname} ->
            Client = nameservice_lookup(NameService, Clientname, LogFile),
            {ok, NewGGTClients} = dict_insert([Clientname, Client, {none, none}], GGTClients),
            initial_state_loop(Params, State, NewGGTClients, LogFile);
        %% 4. durch step wechselt der Koordinator in den bereit Zustand
        step ->
            MissingGGTs = length(GGTClients) - StarterCount * GGTProzessanzahl,
            logging(LogFile,
                    format("~s: Starte Berechnung, vermisse ~B ggT-Prozesse.~n",
                           [?stime, MissingGGTs])),
            %% 5. für die Berechnung baut der Koordinator einen Ring an ggT-Prozessen auf
            NewGGTClients = build_ggt_circle(GGTClients, LogFile),
            foreach(fun([_, Client, {N1, N2}]) -> Client ! {setneighbors, N1, N2} end,
                    NewGGTClients),
            ready_state_loop(Params, State, NewGGTClients, LogFile);
        %% 9.d.
        toggle ->
            New_Korrigieren = toggle_koordinator_handler(Korrigieren, LogFile),
            initial_state_loop(Params, [New_Korrigieren], GGTClients, LogFile);
        %% 9.e.
        toggle_ggt ->
            toggle_ggt_handler(GGTClients, LogFile);
        Any ->
            manual_interface(Any, Params, State, GGTClients, LogFile),
            initial_state_loop(Params, State, GGTClients, LogFile)
    end,
    ok.

%% @doc 5. Teilt den regestrierten GGTs ihre Nachbarn zu.
build_ggt_circle(GGTClients, LogFile) ->
    logging(LogFile, format("~s: Koordinator baut Ring auf.~n", [?stime])),
    %% 5.a.
    ShuffledGGTClients = util:shuffle(GGTClients),
    set_neighbours(ShuffledGGTClients).

set_neighbours(GGTClients) ->
    set_neighbours(GGTClients, []).

set_neighbours([], Out) ->
    Out;
%% 5.b.
set_neighbours([[Clientname, Client, {none, none}], [BClientname, BClient, {none, none}]
                | Tail],
               Out) ->
    NewAggt = [Clientname, Client, {none, BClientname}],
    NewBggt = [BClientname, BClient, {Clientname, none}],
    %%                                 5.c.
    set_neighbours([NewBggt | Tail], [NewAggt | Out]);
%% 5.d. hier sind wir beim vorletzten Client in GGTClients angekommen
%% und verbinden den letzen Client in GGTClients mit dem ersten in Out
set_neighbours([[Clientname, Client, {N1, none}], [BClientname, BClient, {none, none}]],
               Out) ->
    [FClientname, FClient, {none, FN2}] = lists_nth(length(Out), Out),
    NewFirst = [FClientname, FClient, {BClientname, FN2}],
    NewAggt = [Clientname, Client, {N1, BClientname}],
    NewBggt = [BClientname, BClient, {Clientname, FClientname}],
    set_neighbours([], [NewFirst, NewBggt, NewAggt | Out]);
%% 5.b.
set_neighbours([[Clientname, Client, {N1, none}], [BClientname, BClient, {none, none}]
                | Tail],
               Out) ->
    NewAggt = [Clientname, Client, {N1, BClientname}],
    NewBggt = [BClientname, BClient, {Clientname, none}],
    %%                                 5.c.
    set_neighbours([NewBggt | Tail], [NewAggt | Out]).

%% @doc bildet den "bereit" Zustand des Koordinators ab
ready_state_loop(Params,
                 State = {Korrigieren, SmallestKnownNumber, Mis, StarterCount},
                 GGTClients,
                 LogFile) ->
    logging(LogFile, format("koordinator ist in ready_state_loop~n", [])),
    receive
        %% 6. startet die ggT-Berechnung indem ein initiales Mi verschickt wird
        {calc, WggT} ->
            NewMis = vsutil:bestimme_mis(WggT, length(GGTClients)),
            send_mis(NewMis, GGTClients, LogFile),
            GGTStarter = get_random_ggts(GGTClients, LogFile),
            foreach(fun([_, Client, _]) -> Client ! {calc, start} end, GGTStarter),
            ready_state_loop(Params,
                             {Korrigieren, SmallestKnownNumber, NewMis, StarterCount},
                             GGTClients,
                             LogFile);
        {briefmi, {Clientname, CMi, CZeit}} ->
            %% ggT-Prozess informiert über neues `Mi` um `Time`
            logging(LogFile,
                    format("~s: ~s hat Mi=~p um ~s gemeldet.~n",
                           [?stime, Clientname, CMi, now2string(CZeit)])),
            ready_state_loop(Params, State, GGTClients, LogFile);
        {getinit, From} ->
            Mi = lists_nth(rand:uniform(length(Mis)), Mis),
            From ! {sendy, Mi},
            logging(LogFile, format("~s: Initialen Wert ~B gesendet.~n", [?stime, Mi])),
            ready_state_loop(Params, State, GGTClients, LogFile);
        %% 7.
        {From, briefterm, {Clientname, CMi, CZeit}} ->
            if Korrigieren and (SmallestKnownNumber < CMi) ->
                   From ! {sendy, SmallestKnownNumber};
               true ->
                   ok
            end,
            logging(LogFile,
                    format("~s: ~s mit ~p hat Terminierung mit Mi=~p um ~s gemeldet.~n",
                           [?stime, Clientname, From, CMi, now2string(CZeit)])),
            ready_state_loop(Params, State, GGTClients, LogFile);
        Any ->
            manual_interface(Any, Params, State, GGTClients, LogFile),
            ready_state_loop(Params, State, GGTClients, LogFile)
    end.

get_random_ggts(GGTClients, _LogFile) ->
    Count = round(length(GGTClients) * 0.2),
    GGTs = get_n_ggts(Count, GGTClients),
    case length(GGTs) of
        0 ->
            [lists_nth(rand:uniform(length(GGTClients)), GGTClients),
             lists_nth(rand:uniform(length(GGTClients)), GGTClients)
             | GGTs];
        1 ->
            [lists_nth(rand:uniform(length(GGTClients)), GGTClients) | GGTs];
        _ ->
            GGTs
    end.

get_n_ggts(N, [GGT | Tail]) ->
    get_n_ggts(N - 1, Tail, [GGT]).

get_n_ggts(0, _GGTClients, Out) ->
    Out;
get_n_ggts(N, [GGT | Tail], Out) ->
    get_n_ggts(N - 1, Tail, [GGT | Out]).

send_mis([], [], LogFile) ->
    logging(LogFile, format("~s: erfolgreich alle Mis versendet.~n", [?stime])),
    ok;
send_mis([Mi | Mis], [[Clientname, Client, _] | GGTClients], LogFile) ->
    Client ! {setpm, Mi},
    logging(LogFile, format("~s: Mi=~p, an ~s geschickt.~n", [?stime, Mi, Clientname])),
    send_mis(Mis, GGTClients, LogFile).

%% @doc 8. Beendigungsphase
exit_state_loop(_Params = {_, _, _, NameService, KoordinatorName},
                _State,
                GGTClients,
                LogFile) ->
    logging(LogFile, format("koordinator ist in exit_state_loop~n", [])),
    %% Koordinator informiert ggT-Porzesse über die Beendigung
    kill_ggt_handler(GGTClients, LogFile),
    %% Koordinator meldet sich ab
    nameservice_unbind(NameService, KoordinatorName, LogFile),
    ok.

%% @doc 9. Commands des Manuellen Interfaces des Koordinators, welche immer verfügbar sind.
-spec manual_interface(atom(), Params, State, GGTClients, LogFile) -> atom()
    when Params :: params(),
         State :: state(),
         GGTClients :: [{atom(), {atom(), atom()}, {atom() | none, atom() | none}}],
         LogFile :: string().
manual_interface(Command,
                 Params,
                 State = {Korrigieren, _SmallestKnownNumber, _Mis, _StarterCount},
                 GGTClients,
                 LogFile) ->
    case Command of
        %% 9.a.
        reset ->
            kill_ggt_handler(GGTClients, LogFile),
            initial_state_loop(Params, State, GGTClients, LogFile);
        ggt ->
            lists_nth(rand:uniform(length(GGTClients)), GGTClients);
        %% 9.b.
        prompt ->
            foreach(fun([Clientname, Client, _]) ->
                       Client ! {self(), tellmi},
                       receive
                           {mi, Mi} ->
                               logging(LogFile, format("~s hat Mi = ~p.~n", [Clientname, Mi]))
                       after 3000 ->
                           logging(LogFile,
                                   format("prompt: keine Antwort von ~s erhalten.~n", [Clientname]))
                       end
                    end,
                    GGTClients);
        %% 9.c.
        nudge ->
            foreach(fun([Clientname, Client, _]) ->
                       Client ! {self(), pingGGT},
                       receive
                           {pongGGT, GGTname} ->
                               logging(LogFile,
                                       format("GGTname=~s ist noch am Leben.~n", [GGTname]))
                       after 3000 ->
                           logging(LogFile,
                                   format("nudge: keine Antwort von ~s erhalten.~n", [Clientname]))
                       end
                    end,
                    %% after 30 ->
                    %%     logging(LogFile, format("~s ist nicht mehr am Leben.~n", [Client]))
                    GGTClients);
        %% 9.f.
        kill ->
            exit_state_loop(Params, State, GGTClients, LogFile),
            exit(normal);
        Any ->
            logging(LogFile,
                    format("~s: konnte mit Nachricht=~p nichts anfangen.~n", [?stime, Any]))
    end.

kill_ggt_handler(GGTClients, LogFile) ->
    foreach(fun([_, Client, _]) ->
               Client ! kill,
               logging(LogFile, format("~s: kill an ~p geschickt.~n", [?stime, Client]))
            end,
            GGTClients),
    ok.

toggle_koordinator_handler(true, LogFile) ->
    logging(LogFile, format("toggle-Befehl erhalten. Neuer Wert des Flags: false~n", [])),
    false;
toggle_koordinator_handler(false, LogFile) ->
    logging(LogFile, format("toggle-Befehl erhalten. Neuer Wert des Flags: true~n", [])),
    true.

toggle_ggt_handler(GGTClients, LogFile) ->
    foreach(fun([Clientname, Client, _]) ->
               Client ! {self(), toggle},
               logging(LogFile, format("toggle-Befehl an GGTname=~s gesendet.~n", [Clientname]))
            end,
            GGTClients).

%% UTIL

-spec foreach(function(), list()) -> ok.
foreach(_Fun, []) ->
    ok;
foreach(Fun, [Elem | Tail]) ->
    Fun(Elem),
    foreach(Fun, Tail).

%% dict

%% @doc inserts Entry into Dict.
%% returns alreadyExists when the Entry already exists in Dict.
dict_insert(Entry, []) ->
    {ok, [Entry]};
dict_insert(Entry, Dict) ->
    case lists_is_member(Entry, Dict) of
        true ->
            alreadyExists;
        false ->
            {ok, [Entry | Dict]}
    end.

%% -spec dict_get(atom(), [tuple()]) -> tuple() | notFound.
%% dict_get(_Key, []) ->
%%     notFound;
%% dict_get(Key, [[Key, Value] | _]) ->
%%     Value;
%% dict_get(Key, List) ->
%%     dict_get(Key, List).

%% dict_set(Key, NewValue, Dict) ->
%%     dict_set_inner(Key, NewValue, Dict).

%% dict_set_inner(_Key, _NewValue, []) ->
%%     notFound;
%% dict_set_inner(Key, NewValue, [[Key | _] | Tail]) ->
%%     [[Key, NewValue] | Tail];
%% dict_set_inner(Key, NewValue, [Elem = [_OtherKey | _] | Tail]) ->
%% {ok, [Elem | dict_set_inner(Key, NewValue, Tail)]}.

%% Lists

%% @doc checks if Elem is in List
lists_is_member(_Elem, []) ->
    false;
lists_is_member(Elem, [Elem | _Tail]) ->
    true;
lists_is_member(Elem, [_ | Tail]) ->
    lists_is_member(Elem, Tail).

-spec lists_nth(N, List) -> Elem
    when N :: pos_integer(),
         List :: [T, ...],
         Elem :: T,
         T :: term().
lists_nth(1, [H | _]) ->
    H;
lists_nth(N, [_ | T]) when N > 1 ->
    lists_nth(N - 1, T).

%% nameservice api

nameservice_rebind(NameService, Service, LogFile) ->
    NameService ! {self(), {rebind, Service, node()}},
    logging(LogFile,
            format("~s: rebind an nameservice geschickt.~n", [now2string(erlang:timestamp())])),
    receive
        ok_overwrite ->
            ok_overwrite;
        ok_new ->
            ok_new
    end.

nameservice_lookup(NameService, Service, LogFile) ->
    NameService ! {self(), {lookup, Service}},
    logging(LogFile,
            format("~s: used lookup with ~s.~n", [now2string(erlang:timestamp()), Service])),
    receive
        not_found ->
            not_found;
        {pin, {Name, Node}} ->
            {Name, Node}
    end.

nameservice_unbind(NameService, Service, LogFile) ->
    NameService ! {self(), {unbind, Service}},
    Return =
        receive
            ok ->
                logging(LogFile,
                        format("~s erfolgreich beim Namensdienst entbunden.~n", [Service])),
                ok
        end,
    unregister(Service),
    logging(LogFile, format("~s erfolgreich entregistriert.~n", [Service])),
    Return.

%% nameservice_twocast(NameService, MiNew, LogFile) ->
%%     NameService ! {self(), {twocast, tell, MiNew}},
%%     logging(LogFile, format("twocast mit neuem Mi: ~p ausgeführt.~n", [MiNew])),
%%     ok.

%% nameservice_reset(NameService, LogFile) ->
%%     NameService ! {self(), reset},
%%     receive
%%         ok ->
%%             logging(LogFile,
%%                     format("~s: Nameservice reset.~n",
%%                            [vsutil:now2string(
%%                                 erlang:timestamp())])),
%%             ok
%%     end.
