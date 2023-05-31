-module(koordinator).

-export([start/0]).

-import(vsutil, [get_config_value/2, now2string/1]).
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
                {nameservice, NameServiceNode}
        end,
    %% NameService ! {self(), {bind, KoordinatorName, node()}},
    %% receive
    %%     ok ->
    %%         ok;
    %%     in_use ->
    %%         logging(LogFile, format("koordinator nicht beim nameservice registriert werden.~n", []))
    %% end,
    nameservice_rebind(NameService, KoordinatorName, LogFile),

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
                       [_Arbeitszeit = {AZMin, AZMax},
                        TermZeit,
                        GGTProzessanzahl,
                        NameService,
                        _KoordinatorName],
                   State = [Korrigieren],
                   GGTClients,
                   LogFile) ->
    receive
        step ->
            %% TODO kann das nur im initial Zustand passieren?
            %% TODO  build_ggt_circle(GGTClients, LogFile)
            ready_state_loop(Params, State, GGTClients, LogFile);
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
            {ok, NewGGTClients} = dict_insert([Clientname, Client, {N1, N2}], GGTClients),
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

ready_state_loop(Params,
                 State = [Korrigieren, SmallestKnownNumber],
                 GGTClients,
                 LogFile) ->
    receive
        %% startet die ggT-Berechnung indem initiales Mi verschickt wird
        {calc, WggT} ->
            Mis = vsutil:bestimme_mis(WggT, length(GGTClients)),
            send_mis(Mis, GGTClients, LogFile),
            GGTStarter = get_random_ggts(GGTClients, LogFile),
            foreach(fun([_, Client, _]) -> Client ! {calc, start} end, GGTStarter),
            calc_handler(),
            ready_state_loop(Params, State, GGTClients, LogFile);
        {briefmi, {Clientname, CMi, CZeit}} ->
            %% ggT-Prozess informiert über neues `Mi` um `Time`
            logging(LogFile,
                    format("~s: ~s hat Mi=~p um ~s gemeldet.~n",
                           [now2string(erlang:timestamp()), Clientname, CMi, now2string(CZeit)])),
            ready_state_loop(Params, State, GGTClients, LogFile);
        {getinit, From} ->
            From ! {sendy, SmallestKnownNumber},
            ready_state_loop(Params, State, GGTClients, LogFile);
        %% TODO kann das hier passieren oder auch nur im initial Zustand?
        toggle ->
            New_Korrigieren = toggle_koordinator_handler(Korrigieren),
            ready_state_loop(Params, [New_Korrigieren], GGTClients, LogFile);
        %% TODO kann das hier passieren oder auch nur im initial Zustand?
        toggle_ggt ->
            toggle_ggt_handler(GGTClients),
            ready_state_loop(Params, State, GGTClients, LogFile);
        {From, briefterm, {Clientname, CMi, CZeit}} ->
            if Korrigieren and SmallestKnownNumber < CMi ->
                   From ! {sendy, SmallestKnownNumber}
            end,
            logging(LogFile,
                    format("~s: ~s mit ~p hat Mi=~p um ~s gemeldet.~n",
                           [now2string(erlang:timestamp()),
                            Clientname,
                            From,
                            CMi,
                            now2string(CZeit)])),
            ready_state_loop(Params, State, GGTClients, LogFile)
    end.

get_random_ggts(_, _) ->
    todo.

send_mis([], [], LogFile) ->
    logging(LogFile,
            format("~s: erfolgreich alle Mis versendet.~n", [now2string(erlang:timestamp())])),
    ok;
send_mis([Mi | Mis], [[Clientname, Client, _] | GGTClients], LogFile) ->
    Client ! {setpm, Mi},
    logging(LogFile,
            format("~s: Mi=~p, an ~s geschickt.~n",
                   [now2string(erlang:timestamp()), Mi, Clientname])),
    send_mis(Mis, GGTClients, LogFile).

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
                 State,
                 GGTClients,
                 LogFile) ->
    case Command of
        reset ->
            kill_ggt_handler(GGTClients, LogFile),
            initial_state_loop(Params, State, GGTClients, LogFile);
        ggt ->
            lists_nth(rand:uniform(length(GGTClients)), GGTClients);
        prompt ->
            foreach(fun([_, Client, _]) ->
                       Client ! {self(), tellmi},
                       receive
                           {mi, Mi} -> logging(LogFile, format("~s hat Mi = ~p.~n", [Client, Mi]))
                       end
                    end,
                    GGTClients);
        nudge ->
            foreach(fun([_, Client, _]) ->
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
    foreach(fun([_, Client, _]) ->
               Client ! kill,
               logging(LogFile,
                       format("~s: kill an ~p geschickt.~n",
                              [now2string(erlang:timestamp()), Client]))
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
    foreach(fun([_, Client, _]) -> Client ! {self(), toggle} end, GGTClients).

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

-spec dict_get(atom(), [tuple()]) -> tuple() | notFound.
dict_get(_Key, []) ->
    notFound;
dict_get(Key, [[Key, Value] | _]) ->
    Value;
dict_get(Key, List) ->
    dict_get(Key, List).

dict_set(Key, NewValue, Dict) ->
    dict_set_inner(Key, NewValue, Dict).

dict_set_inner(_Key, _NewValue, []) ->
    notFound;
dict_set_inner(Key, NewValue, [[Key | _] | Tail]) ->
    [[Key, NewValue] | Tail];
dict_set_inner(Key, NewValue, [Elem = [_OtherKey | _] | Tail]) ->
    {ok, [Elem | dict_set_inner(Key, NewValue, Tail)]}.

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
    logging(LogFile, format("~s: rebind an nameservice geschickt.~n", [now2string(erlang:timestamp())])),
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
