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
    logging(LogFile,
            io_lib:format("~s: DLQ mit DLQLimit ~B erstellt.~n",
                          [now2string(erlang:timestamp()), DLQLimit])),
    % 1 und 3
    {DLQLimit, []}.

%% @doc Löscht die DLQ.
delDLQ(_Queue) ->
    % 1, 2
    ok.

%% @doc Ermittelt die als nächstes erwartete Nachrichennummer der DLQ.
%% @end
%%                     1
expectedNr({_Capacity, []}) ->
    % 2
    1;
expectedNr({_Capacity, Queue}) ->
    % 3
    [[NNr | _MessageTail] | _QueueTail] = reverse(Queue),
    % 4
    NNr + 1.

%% @doc Verschiebt eine Nachricht in die DLQ und ergänzt diese um einen Zeitstempel.
push2DLQ(Message, Queue = {Capacity, Messages}, LogFile) ->
    % 1, 2
    TSdlqin = erlang:timestamp(),
    NewMsg =
        case length(Message) > 2 of
            true ->
                [NNr, Msg, TSclientout, TShbqin] = Message,
                %                      2                                  1
                [NNr, Msg ++ now2string(TSdlqin), TSclientout, TShbqin, TSdlqin];
            false ->
                [NNr, Msg] = Message,
                %                      2           1
                [NNr, Msg ++ now2string(TSdlqin), TSdlqin]
        end,

    NewQueue =
        %       3                    4
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

    % 7
    logging(LogFile, io_lib:format("DLQ: ~p wurde in die DLQ eingefügt.~n", [NewMsg])),

    % 8
    NewQueue.

%% @doc Fuegt ein Element der DLQ hinzu und gewaehrleistet,
%% dass es immer in der richtigen Rheinfolge ist.
add2dlq(Elem, {Capacity, Messages}) ->
    {Capacity, add2dlq_intern(Elem, Messages)}.

add2dlq_intern(Elem, []) ->
    [Elem];
add2dlq_intern(Elem = [ElemNNr | _ElemTail],
               Messages = [Msg = [MsgNNr | _MsgTail] | MessagesTail]) ->
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
deliverMSG(MSGNr, ClientPID, Queue = {_Capacity, Messages}, LogFile) ->
    % 1
    ExpectedNNr = expectedNr(Queue),
    %                         2
    case getMessageByNr(MSGNr, Messages, LogFile) of
        % 3        4       5
        nok when MSGNr + 1 < ExpectedNNr ->
            deliverMSG(MSGNr + 1, ClientPID, Queue, LogFile);
        nok ->
            % 14
            NewMessage = [-1, nokA, 0, 0, 0, 0],
            % 13
            logging(LogFile,
                    io_lib:format("DLQ: Nachricht=~B konnte nicht gefunden werden. ~p wurde an "
                                  "~p gesendet.",
                                  [MSGNr, NewMessage, ClientPID])),
            %          16                   15
            ClientPID ! {reply, NewMessage, true},
            % 17
            -1;
        Message ->
            Terminated =
                % 6
                if MSGNr == ExpectedNNr - 1 ->
                       true; % 7
                   true ->
                       false % 8
                end,
            TSdlqout = erlang:timestamp(),
            [NNr, Msg, TSclientout, TShbqin, TSdlqin] = Message,
            % 9
            NewMessage = [NNr, Msg ++ now2string(TSdlqout), TSclientout, TShbqin, TSdlqin, TSdlqout],
            % 11
            ClientPID ! {reply, NewMessage, Terminated},
            % 10
            logging(LogFile,
                    io_lib:format("DLQ: ~p wurde an ~p gesendet.~n", [NewMessage, ClientPID])),
            % 12
            MSGNr
    end.

%% @doc Gibt eine Liste aller Nachrichtennummern in der DLQ zurück.
%% @end
%%                  1
listDLQ({_Capacity, List}) ->
    listDLQintern(List).

%%           2, 3
listDLQintern([]) ->
    [];
%%               5               4
listDLQintern([[CurrentNNr | _T] | Tail]) ->
    %    6                7
    [CurrentNNr | listDLQintern(Tail)]. % 8

%% @doc Gibt die momentane Länge der DLQ zurueck.
lengthDLQ({_Capacity, Queue}) ->
    length(Queue). % 1,2

%% -------------------------------------
%% Util
%% -------------------------------------

%% @doc Gibt bei Erfolg die zugehörige Nachricht zu einer angegebenen Nachrichtennummer zurück.
%% @end
%%                 1, 2
getMessageByNr(NNr, [], LogFile) ->
    logging(LogFile, io_lib:format("NNr=~B wurde nicht in DLQ gefunden.~n", [NNr])),
    nok;
%%                    3          5
getMessageByNr(NNr, [Message = [NNr | _Tail]|_QueueTail], _LogFile) ->
    % 7
    Message;
%%                      3, 5
getMessageByNr(NNr, [_CurrentMsg | Tail], LogFile) ->
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
