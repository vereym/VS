-module(koordinator).

-export([start/0]).

-import(vsutil, [get_config_value/2, now2string/1, now2stringD/1]).
-import(util, [logging/2]).
-import(io_lib, [format/2, format/1]).

start() ->
    {ok, HostName} = inet:gethostname(),
    LogFile = format("koordinator@~s.log", [HostName]),

    %% 1. config auslesen
    {ok, KoordinatorConfig} = file:consult("koordinator.cfg"),

    {ok, Arbeitszeit} = get_config_value(arbeitszeit, KoordinatorConfig),
    {ok, TermZeit} = get_config_value(termzeit, KoordinatorConfig),
    {ok, GGTProzessanzahl} = get_config_value(ggtprozessanzahl, KoordinatorConfig),
    {ok, NameServiceNode} = get_config_value(nameservicenode, KoordinatorConfig),
    {ok, KoordinatorName} = get_config_value(koordinatorname, KoordinatorConfig),
    {ok, Korrigieren} = get_config_value(korrigieren, KoordinatorConfig),

    %% 2. bei lokalen Erlang-Node und Namensdienst regestrieren
    register(KoordinatorName, self()),
    NameService =
        case net_adm:ping(NameServiceNode) of
            pang ->
                logging(LogFile, format("nameservice konnte nicht gefunden werden~n", [])),
                ok;
            pong ->
                NameServicePID = global:whereis_name(nameservice),
                NameServicePID ! {self(), {bind, KoordinatorName, node()}},
                receive
                    ok ->
                        NameServicePID
                end
        end,

    spawn(fun() ->
             initial_state_loop([Arbeitszeit,
                                 TermZeit,
                                 GGTProzessanzahl,
                                 NameService,
                                 KoordinatorName],
                                [Korrigieren],
                                [],
                                LogFile)
          end),
    ok.

%% @doc Bildet den bereit-Zustand ab.
initial_state_loop(Params =
                       [_Arbeitszeit = {AZMin, AZMax}, TermZeit, GGTProzessanzahl, NameService],
                   State = [Korrigieren],
                   GGTClients,
                   LogFile) ->
    receive
        {From, getsteeringval} ->
            %% AZMin, AZMax = simulierte Verzögerungszeit zur Berechnung in Sekunden
            %% TermZeit = Wartezeit in Sekunden, bis eine Wahl für eine Terminierung initiiert wird
            %% GGTProzessanzahl = Anzahl zu startender ggT-Prozesse
            From ! Msg = {steeringval, {AZMin, AZMax}, TermZeit, GGTProzessanzahl},
            logging(LogFile, format("~p an ~p geschickt~n", [Msg, From])),
            initial_state_loop(Params, State, GGTClients, LogFile);
        {hello, Clientname} ->
            Client = nameservice_lookup(NameService, Clientname, LogFile),
            {N1, N2} = get_free_neighbours(GGTClients, LogFile),
            Client ! {setneighbors, N1, N2},
            %% client in GGTClients updaten
            NewGGTClients = dict_set(Clientname, {N1, N2}, GGTClients),
            initial_state_loop(Params, State, NewGGTClients, LogFile);
        toggle ->
            New_Korrigieren = toggle_koordinator_handler(Korrigieren),
            initial_state_loop(Params, [New_Korrigieren], GGTClients, LogFile);
        toggle_ggt ->
            toggle_ggt_handler(GGTClients),
            initial_state_loop(Params, State, GGTClients, LogFile);
        Any ->
            manual_interface(Any, Params, State, GGTClients, LogFile),
            initial_state_loop(Params, State, GGTClients, LogFile)
    end,
    ok.

get_free_neighbours(GGTClients, LogFile) ->
    %% TODO muss irgendwie verwalten, welchen ggTs ich schon Neighbour geschickt hab
    ok.

ready_state_loop(Params, State = [Korrigieren], GGTClients, LogFile) ->
    receive
        %% startet die ggT-Berechnung indem initiales Mi verschickt wird
        {calc, WggT} ->
            %% TODO
            %% Mi holen
            %% an alle ggTs schicken
            %%
            %% zufällig 20% aller ggT-Prozesse (mind 2) auswählen
            %% an diese {calc,start} schicken
            vsutil:bestimme_mis(WggT, length(GGTClients)),
            calc_handler(),
            todo;
        {briefmi, {Clientname, CMi, CZeit}} ->
            %% ggT-Prozess informiert über neues `Mi` um `Time`
            ok,
            ready_state_loop(Params, State, GGTClients, LogFile);
        {getinit, From} ->
            ready_state_loop(Params, State, GGTClients, LogFile);
        %% TODO kann das hier passieren oder auch nur im initial Zustandt?
        toggle ->
            New_Korrigieren = toggle_koordinator_handler(Korrigieren),
            ready_state_loop(Params, [New_Korrigieren], GGTClients, LogFile);
        %% TODO kann das hier passieren oder auch nur im initial Zustandt?
        toggle_ggt ->
            toggle_ggt_handler(GGTClients),
            ready_state_loop(Params, State, GGTClients, LogFile);
        {From, briefterm, {Clientname, CMi, CZeit}} ->
            %% TODO unter umständen Fehler loggen bzw Korrigieren.
            ready_state_loop(Params, State, GGTClients, LogFile)
    end.

exit_state_loop(_Params = [_, _, _, NameService, KoordinatorName],
                _State,
                GGTClients,
                LogFile) ->
    kill_ggt_handler(GGTClients, LogFile),
    nameservice_unbind(NameService, KoordinatorName, LogFile),
    ok.

%% @doc Commands des Manuellen Interfaces des Koordinators, welche immer verfügbar sind.
manual_interface(Command,
                 Params =
                     [_Arbeitszeit, _TermZeit, _GGTProzessanzahl, _NameService, _KoordinatorName],
                 State = [Korrigieren],
                 GGTClients,
                 LogFile) ->
    case Command of
        reset ->
            kill_ggt_handler(GGTClients, LogFile),
            initial_state_loop(Params, State, GGTClients, LogFile);
        step ->
            %% TODO kann das nur im initial Zustand passieren?
            %% TODO ggTs MI schicken -> ring an ggT Prozessen aufbauen
            %% build_ggt_circle(GGTClients, LogFile)
            %% in bereit-Zustand wechseln
            ready_state_loop(Params, State, GGTClients, LogFile);
        prompt ->
            %% TODO einfach Client funktioniert nicht muss vorher pid aus Client holen
            foreach(fun(Client) ->
                       Client ! {self(), tellmi},
                       receive
                           {mi, Mi} -> logging(LogFile, format("~s hat Mi = ~p.~n", [Client, Mi]))
                       end
                    end,
                    GGTClients);
        nudge ->
            foreach(fun(Client) ->
                       Client ! {self(), pingGGT},
                       receive
                           {pongGGT, GGTname} ->
                               logging(LogFile,
                                       format("~s mit namen ~s ist noch am Leben.~n",
                                              [Client, GGTname]))
                       end
                    end,
                    %% after 30 ->
                    %%     logging(LogFile, format("~s ist nicht mehr am Leben.~n", [Client]))
                    GGTClients);
        kill ->
            exit_state_loop(Params, State, LogFile, GGTClients)
    end.

kill_ggt_handler(GGTClients, LogFile) ->
    foreach(fun(Client) ->
               Client ! kill,
               logging(LogFile, format("kill an ~p geschickt.~n", [Client]))
            end,
            GGTClients),
    ok.

calc_handler() ->
    %% TODO
    receive
        {getinit, PID} ->
            todo;
        _ ->
            todo
    end.

toggle_koordinator_handler(yes_correct) ->
    no_correct;
toggle_koordinator_handler(no_correct) ->
    yes_correct.

toggle_ggt_handler(GGTClients) ->
    todo.

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

-spec dict_get([tuple()], atom()) -> tuple() | notFound.
dict_get([], Key) ->
    notFound;
dict_get([[Key, Value] | _], Key) ->
    Value;
dict_get(List, Key) ->
    dict_get(List, Key).

dict_set(Key, NewValue, Dict) ->
    dict_set_inner(Key, NewValue, Dict).

dict_set_inner(Key, NewValue, []) ->
    notFound;
dict_set_inner(Key, NewValue, [[Key | _] | Tail]) ->
    [[Key, NewValue] | Tail];
dict_set_inner(Key, NewValue, [Elem = [OtherKey | _] | Tail]) ->
    {ok, [Elem | dict_set_inner(Key, NewValue, Tail)]}.

%% Lists

%% @doc checks if Elem is in List
lists_is_member(Elem, []) ->
    false;
lists_is_member(Elem, [Elem | Tail]) ->
    true;
lists_is_member(Elem, [_ | Tail]) ->
    lists_is_member(Elem, Tail).

%% nameservice api

nameservice_rebind(NameService, Service, LogFile) ->
    NameService ! {self(), {rebind, Service, node()}},
    receive
        ok_overwrite ->
            ok_overwrite;
        ok_new ->
            ok_new
    end.

nameservice_lookup(NameService, Service, LogFile) ->
    NameService ! {self(), {lookup, Service}},
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

nameservice_twocast(NameService, MiNew, LogFile) ->
    NameService ! {self(), {twocast, tell, MiNew}},
    logging(LogFile, format("twocast mit neuem Mi: ~p ausgeführt.~n", [MiNew])),
    ok.

nameservice_reset(NameService, LogFile) ->
    NameService ! {self(), reset},
    receive
        ok ->
            logging(LogFile,
                    format("~s: Nameservice reset.~n",
                           [vsutil:now2string(
                                erlang:timestamp())])),
            ok
    end.
