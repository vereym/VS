-module(ko_send).

-export([msg/1]).

-import(vsutil, [get_config_value/2]).

msg(Command) ->
    {ok, KoordinatorConfig} = file:consult("koordinator.cfg"),

    {ok, NameServiceNode} = get_config_value(nameservicenode, KoordinatorConfig),
    {ok, KoordinatorName} = get_config_value(koordinatorname, KoordinatorConfig),
 
    Koordinator =
        case net_adm:ping(NameServiceNode) of
            pang ->
                io:format("nameservice konnte nicht gefunden werden~n", []),
                ok;
            pong ->
                NameService = {nameservice, NameServiceNode},
                nameservice_lookup(NameService, KoordinatorName)
        end,

    case Command of
		help ->
            io:format(unicode:characters_to_list("Avaliable Commands:~n"
		        "    help:        Liste aller commands,~n"
		        "    vals:        Steuerungswerte,~n"
		        "    ggt:         Zufalls ggT,~n"
		        "    {calc,Wggt}: Berechnung des ggT mit einem Wunschggt starten,~n"
		        "    step:        Beendet Anmeldephase,~n"
		        "    nudge:       eine Art ping,~n"
		        "    prompt:      aktuelle Mi's anzeigen,~n"
		        "    toggle:      Korrektur-Flag ändern,~n"
		        "    toggle_ggt:  Korrektur-Flags der GGTs ändern,~n"
		        "    reset:       Koordinator in initial Zustandversetzen und Berechnung abbrechen,~n"
		        "    kill:        alle Prozesse (bis auf Namensdienst) beenden.~n"));
        vals ->
            Koordinator ! {self(), getsteeringval},
            receive
                Any -> io:fwrite(Any)
            end;
        ggt ->
            Koordinator ! ggt;
        {calc, Wggt} ->
            Koordinator ! {calc, Wggt},
            receive
                Any -> io:fwrite(Any)
            end;
        step ->
            Koordinator ! step,
            receive
                Any -> io:fwrite(Any)
            end;
        nudge ->
            Koordinator ! nudge,
            receive
                Any -> io:fwrite(Any)
            end;
        prompt ->
            Koordinator ! prompt,
            receive
                Any -> io:fwrite(Any)
            end;
        toggle ->
            Koordinator ! toggle,
            receive
                Any -> io:fwrite(Any)
            end;
        toggle_ggt ->
            Koordinator ! toggle_ggt,
            receive
                Any -> io:fwrite(Any)
            end;
        reset ->
            Koordinator ! reset,
            receive
                Any -> io:fwrite(Any)
            end;
        kill ->
            Koordinator ! kill,
            receive
                Any -> io:fwrite(Any)
            end;
        _ ->
            io:fwrite("unknown command, try help instead.")
    end.

nameservice_lookup(NameService, Service) ->
    NameService ! {self(), {lookup, Service}},
    receive
        not_found ->
            not_found;
        {pin, {Name, Node}} ->
            {Name, Node}
    end.

