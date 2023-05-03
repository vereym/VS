-module(cmem).

-export([initCMEM/2, delCMEM/1, updateClient/4, getClientNNr/2, listCMEM/1, lengthCMEM/1]).

-import(util, [logging/2]).
-import(vsutil, [now2string/1, now2stringD/1, diffTS/2]).
-import(io_lib, [format/2]).

% Initialisiert das CMEM.
initCMEM(RemTime, LogFile) ->
    % 2
    logging(
        LogFile,
        format("~s: CMEM erfolgreich initialisiert~n", [
            now2string(erlang:timestamp())
        ])
    ),
    % 1 & 3
    {RemTime, []}.

% Löscht das CMEM.
delCMEM(_CMEM) ->
    % 1 & 2
    ok.

% Aktualisiert einen Eintrag im CMEM mit der übergebenen Nachrichtennummer.
% Gibt es für den angegebenen Client noch keinen Eintrag, wird dieser erstellt,
% wobei ihm die Nachrichtennummer 1 zugewiesen wird.
updateClient(CMEM, ClientID, NNr, LogFile) ->
    % 1
    LastInteraction = erlang:timestamp(),
    % 2
    NewCMEM = forgetClients(CMEM),
    %             3
    {RemTime, ClientList} = NewCMEM,
    %     8                 4
    NewClientList = updateClientList(ClientList, ClientID, LastInteraction, NNr),
    % 9
    logging(
        LogFile,
        format("~s: CMEM erfolgreich aktualisiert~n", [
            now2string(erlang:timestamp())
        ])
    ),
    % 10
    {RemTime, NewClientList}.

% Hilfsfunktion für updateClient/4.
%                5
updateClientList([], ClientID, LastInteraction, NNr) ->
    % 7
    [{ClientID, LastInteraction, NNr}];
%                5
updateClientList([{ClientID, _, _} | T], ClientID, LastInteraction, NNr) ->
    % 6
    [{ClientID, LastInteraction, NNr} | T];
updateClientList([H | T], ClientID, LastInteraction, NNr) ->
    [H | updateClientList(T, ClientID, LastInteraction, NNr)].

% Entfernt Einträge von allen Clients, die sich zu lang nicht beim Server gemeldet
% haben (länger als RemTime), aus dem CMEM.
%                  3        2
forgetClients({RemTime, ClientList}) ->
    % 1
    Now = erlang:timestamp(),
    %     8
    NewClientList = forgetClients(ClientList, Now, RemTime),
    % 9
    {RemTime, NewClientList}.

% Hilfsfunktion für forgetClients/1.
%             4
forgetClients([], _Now, _RemTime) ->
    [];
%             4                 5
forgetClients([H = {_ID, LastInteraction, _NNr} | T], Now, RemTime) ->
    TimeDifference = tsToSeconds(diffTS(Now, LastInteraction)),
    % 6
    if
        TimeDifference > RemTime ->
            % 7
            forgetClients(T, Now, RemTime);
        true ->
            [H | forgetClients(T, Now, RemTime)]
    end.

% Hilfsfunktion für forgetClients/3.
% Wandelt einen Zeitstempel in eine Zeit in Sekunden um.
tsToSeconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.

% Abfrage, welche Nachrichtennummer der Client als nächstes erhalten darf.
%                            1
getClientNNr({_RemTime, ClientList}, ClientID) ->
    % 2
    getClientNNrInternal(ClientList, ClientID).

% Hilfsfunktion für getClientNNr/2.
%                    3
getClientNNrInternal([], _ClientID) ->
    % 6 & 7
    1;
%                    3
getClientNNrInternal([{ClientID, _LastInteraction, NNr} | _T], ClientID) ->
    % 4, 5 & 7
    NNr + 1;
getClientNNrInternal([_H | T], ClientID) ->
    % 4
    getClientNNrInternal(T, ClientID).

% Gibt eine Liste aller ClientIDs und den dazugehörigen Nachrichtennummern im CMEM zurück.
%                        1
listCMEM({_RemTime, ClientList}) ->
    % 2 & 4
    listCMEMinternal(ClientList).

% Hilfsfunktion für listCMEM/1.
listCMEMinternal([]) ->
    [];
listCMEMinternal([{ClientID, _LastInteraction, NNr} | T]) ->
    % 3
    [{ClientID, NNr} | listCMEMinternal(T)].

% Gibt die Anzahl der Elemente in CMEM zurück.
%                          1
lengthCMEM({_RemTime, ClientList}) ->
    % 2 & 3
    length(ClientList).
