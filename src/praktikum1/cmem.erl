-module(cmem).

-export([initCMEM/2, delCMEM/1, updateClient/4, getClientNNr/2, listCMEM/1, lengthCMEM/1]).

-import(util, [logging/2]).
-import(vsutil, [now2string/1, now2stringD/1, diffTS/2]).
-import(io_lib, [format/2]).

initCMEM(RemTime, LogFile) ->
    logging(
        LogFile,
        format("~s: CMEM erfolgreich initialisiert~n", [
            now2string(erlang:timestamp())
        ])
    ),
    {RemTime, []}.

delCMEM(_CMEM) ->
    ok.

updateClient(CMEM, ClientID, NNr, LogFile) ->
    LastInteraction = erlang:timestamp(),
    NewCMEM = forgetClients(CMEM),
    {RemTime, ClientList} = NewCMEM,
    NewClientList = updateClientList(ClientList, ClientID, LastInteraction, NNr),
    logging(
        LogFile,
        format("~s: CMEM erfolgreich aktualisiert~n", [
            now2string(erlang:timestamp())
        ])
    ),
    {RemTime, NewClientList}.

updateClientList([], ClientID, LastInteraction, NNr) ->
    [{ClientID, LastInteraction, NNr}];
updateClientList([{ClientID, _, _} | T], ClientID, LastInteraction, NNr) ->
    [{ClientID, LastInteraction, NNr} | T];
updateClientList([H | T], ClientID, LastInteraction, NNr) ->
    [H | updateClientList(T, ClientID, LastInteraction, NNr)].

forgetClients({RemTime, ClientList}) ->
    Now = erlang:timestamp(),
    NewClientList = forgetClients(ClientList, Now, RemTime),
    {RemTime, NewClientList}.

forgetClients([], _Now, _RemTime) ->
    [];
forgetClients([H = {_ID, LastInteraction, _NNr} | T], Now, RemTime) ->
    TimeDifference = tsToSeconds(diffTS(Now, LastInteraction)),
    if
        TimeDifference > RemTime ->
            forgetClients(T, Now, RemTime);
        true ->
            [H | forgetClients(T, Now, RemTime)]
    end.

tsToSeconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.

getClientNNr({_RemTime, ClientList}, ClientID) ->
    getClientNNrInternal(ClientList, ClientID).

getClientNNrInternal([], _ClientID) ->
    1;
getClientNNrInternal([{ClientID, _LastInteraction, NNr} | _T], ClientID) ->
    NNr;
getClientNNrInternal([_H | T], ClientID) ->
    getClientNNrInternal(T, ClientID).

listCMEM({_RemTime, ClientList}) ->
    listCMEMinternal(ClientList).

listCMEMinternal([]) ->
    [];
listCMEMinternal([{ClientID, _LastInteraction, NNr} | T]) ->
    [{ClientID, NNr} | listCMEMinternal(T)].

lengthCMEM({_RemTime, ClientList}) ->
    length(ClientList).
