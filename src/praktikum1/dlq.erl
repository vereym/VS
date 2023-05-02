-module(dlq).

-export([initDLQ/2]).
-export([delDLQ/1]).
-export([expectedNr/1]).
-export([push2DLQ/3]).
-export([listDLQ/1]).
-export([deliverMSG/4]).

-import(util, [logging/2]).
-import(vsutil, [now2string/1]).

%% @doc initialisiert die DLQ.
initDLQ(DLQLimit, LogFile) ->
    % 2
    logging(LogFile, io_lib:format("DLQ mit ~B erstellt.~n", DLQLimit)),
    % 1 und 3
    {DLQLimit, []}.

delDLQ(_Queue) ->
    ok.

%% @doc Ermittelt die als nächstes erwartete Nachrichennummer der DLQ.
expectedNr(Queue) ->
    % 1
    [NNr | _Tail] = reverse(Queue),
    % 2
    NNr + 1.

%% @doc Verschiebt eine Nachricht in die DLQ und ergänzt diese um einen Zeitstempel.
push2DLQ([NNr, TextMsg, TSclientout, TShbqin], Queue = {Capacity, Messages}, LogFile) ->
    % 1, 2
    TSdlqin = erlang:timestamp(),
    NewMsg = [NNr, TextMsg ++ now2string(TSdlqin), TSclientout, TShbqin, TSdlqin],

    % 7
    logging(LogFile, io_lib:format("~p wurde in die DLQ eingefügt.", NewMsg)),

    %               3                 4
    NewQueue =
        case length(Messages) =< lengthDLQ(Queue) of
            true ->
                % 6
                add2dlq(NewMsg, Queue);
            false ->
                % 5
                [_H | T] = reverse(Messages),
                % 6
                add2dlq(NewMsg, {Capacity, T})
        end,
    % 8
    NewQueue.

%% @doc Fuegt ein Element der DLQ hinzu und gewaehrleistet,
%% dass es immer in der richtigen Rheinfolge ist.
add2dlq(Elem, {Capacity, Messages}) ->
    %% TODO sicherstellen, dass queue geordnet ist
    {Capacity, [Elem | Messages]}.

%% @doc Versendet eine Nachricht an einen Leser.
%% Ist die angefragte Nachrichtennummer nicht in der DLQ enthalten,
%% so wird die Nachricht mit der nächstgrößeren Nummer versendet.
%% Sollte diese ebenfalls nicht existieren, wird stattdessen eine
%% spezielle Fehlernachricht ([-1, nokA, 0, 0, 0, 0]) versendet.
%%
%% Das passiert gdw. eine Nachricht angefragt wird,
%% aber es garkeine Nachrichten für den Leser mehr gab.
%%
%% Neben dem ursprünglichen Nachrichteninhalt werden außerdem
%% ein Ausgangszeitstempel und eine Mitteilung,
%% ob es noch weitere Nachrichten für den Leser gibt (Terminated = true,
%% wenn es keine Nachrichten mehr gibt) mit in die versendete Nachricht eingefügt.
%%
%% Anschließend wird die tatsächlich versendete Nachrichtennummer zurückgegeben.
deliverMSG(MSGNr, ClientPID, Queue, LogFile) ->
    ExpectedNNr = expectedNr(Queue),
    case getMessageByNr(MSGNr, Queue, LogFile) of
        nok when MSGNr + 1 < ExpectedNNr ->
            deliverMSG(MSGNr + 1, ClientPID, Queue, LogFile);
        nok ->
            logging(LogFile, io_lib:format("Nachricht=~B konnte nicht gefunden werden.", MSGNr)),
            NewMessage = [-1, nokA, 0, 0, 0, 0],
            ClientPID ! {reply, NewMessage, true},
            %% TODO sollen wir hier nochmal loggen, dass wir die Fehlernachricht rausgeschickt haben?
            %% logging(LogFile, io_lib:format("~p wurde an ~B gesendet.", NewMessage, ClientPID)),
            -1;
        Message ->
            Terminated =
                if MSGNr == ExpectedNNr - 1 ->
                       true;
                   true ->
                       false
                end,
            TSdlqout = erlang:timestamp(),
            NewMessage = Message ++ TSdlqout,
            ClientPID ! {reply, NewMessage, Terminated},
            logging(LogFile, io_lib:format("~p wurde an ~B gesendet.", NewMessage, ClientPID)),
            MSGNr
    end.

%% @doc Gibt eine Liste aller Nachrichtennummern in der DLQ zurück.
listDLQ({_Capacity, List}) -> % 1, 2
    listDLQintern(List).

%% 3
listDLQintern([]) ->
    [];
%%               5               4
listDLQintern([[CurrentNNr | _T] | Tail]) ->
    %    6                7
    [CurrentNNr | listDLQintern(Tail)]. % 8

%% @doc Gibt die Kapazitaet der DLQ zurueck.
lengthDLQ({Capacity, _Queue}) ->
    Capacity.

%% -------------------------------------
%% Util
%% -------------------------------------

%% @doc Gibt bei Erfolg die zugehörige Nachricht zu einer angegebenen Nachrichtennummer zurück.
getMessageByNr(NNr, {_Capacity, List}, LogFile) ->
    %                    1
    getMessageByNr(NNr, List, LogFile);
%%                  2
getMessageByNr(NNr, [], LogFile) ->
    logging(LogFile, io_lib:format("NNr=~B wurde nicht in DLQ gefunden.", NNr)),
    nok;
%%                             3, 5
getMessageByNr(NNr, Message = [NNr | _Tail], _LogFile) ->
    % 7
    Message;
%%                           3, 5
getMessageByNr(NNr, [_CurrentNr | Tail], LogFile) ->
    %                    6
    getMessageByNr(NNr, Tail, LogFile).

%% @doc reverses a list.
reverse(List) ->
    reverse(List, []).

reverse([Elem | Tail], Accu) ->
    reverse(Tail, [Elem | Accu]).

%% man kann einfach eine Listcomprehention benutzen
%% map(F, []) -> [];
%% map(F, [H|T]) -> [F(H)|map(F, T)].
