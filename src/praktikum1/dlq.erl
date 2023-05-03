-module(dlq).

-export([initDLQ/2]).
-export([delDLQ/1]).
-export([expectedNr/1]).
-export([push2DLQ/3]).
-export([listDLQ/1]).
-export([deliverMSG/4]).
-export([lengthDLQ/1]).

-import(util, [logging/2]).
-import(vsutil, [now2string/1]).

%% @doc initialisiert die DLQ.
initDLQ(DLQLimit, LogFile) ->
    % 2
    logging(
        LogFile,
        io_lib:format("~s: DLQ mit DLQLimit ~B erstellt.~n", [
            DLQLimit, now2string(erlang:timestamp())
        ])
    ),
    % 1 und 3
    {DLQLimit, []}.

delDLQ(_Queue) ->
    ok.

%% @doc Ermittelt die als nächstes erwartete Nachrichennummer der DLQ.
expectedNr({_Capacity, Queue}) ->
    % 1
    case Queue of
        [] ->
            1;
        % 2
        [NNr | _Tail] ->
            NNr + 1
    end.

%% @doc Verschiebt eine Nachricht in die DLQ und ergänzt diese um einen Zeitstempel.
push2DLQ([NNr, TextMsg, TSclientout, TShbqin], Queue = {Capacity, Messages}, LogFile) ->
    % 1, 2
    TSdlqin = erlang:timestamp(),
    NewMsg = [NNr, TextMsg ++ now2string(TSdlqin), TSclientout, TShbqin, TSdlqin],

    % 7
    logging(LogFile, io_lib:format("DLQ: ~p wurde in die DLQ eingefügt.~n", [NewMsg])),

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
    {Capacity, add2dlq_intern(Elem, Messages)}.

add2dlq_intern(Elem, []) ->
    Elem;
add2dlq_intern(
    Elem = [ElemNNr | _ElemTail],
    Messages = [Msg = [MsgNNr | _MsgTail] | MessagesTail]
) ->
    case ElemNNr < MsgNNr of
        true ->
            [Elem | Messages];
        false ->
            [Msg | add2dlq_intern(Elem, MessagesTail)]
    end.

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
            NewMessage = [-1, nokA, 0, 0, 0, 0],
            logging(
                LogFile,
                io_lib:format(
                    "DLQ: Nachricht=~B konnte nicht gefunden werden. ~p wurde an "
                    "~B gesendet.",
                    [MSGNr, NewMessage, ClientPID]
                )
            ),
            ClientPID ! {reply, NewMessage, true},
            -1;
        Message ->
            Terminated =
                if
                    MSGNr == ExpectedNNr - 1 ->
                        true;
                    true ->
                        false
                end,
            TSdlqout = erlang:timestamp(),
            NewMessage = Message ++ TSdlqout,
            ClientPID ! {reply, NewMessage, Terminated},
            logging(
                LogFile, io_lib:format("DLQ: ~p wurde an ~B gesendet.~n", [NewMessage, ClientPID])
            ),
            MSGNr
    end.

%% @doc Gibt eine Liste aller Nachrichtennummern in der DLQ zurück.

% 1, 2
listDLQ({_Capacity, List}) ->
    listDLQintern(List).

%% 3
listDLQintern([]) ->
    [];
%%               5               4
listDLQintern([[CurrentNNr | _T] | Tail]) ->
    %    6                7

    % 8
    [CurrentNNr | listDLQintern(Tail)].

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
    logging(LogFile, io_lib:format("NNr=~B wurde nicht in DLQ gefunden.~n", [NNr])),
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

reverse([], Accu) ->
    Accu;
reverse([Elem | Tail], Accu) ->
    reverse(Tail, [Elem | Accu]).

%% man kann einfach eine Listcomprehention benutzen
%% map(F, []) -> [];
%% map(F, [H|T]) -> [F(H)|map(F, T)].
