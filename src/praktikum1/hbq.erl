-module(hbq).

-export([initHBQ/2]).

-import(dlq, [initDLQ/2]).

initHBQ(_DLQLimit, _HBQName) ->
    % Datei = "DLQLog.log",
    % DLQ = initDLQ(DLQLimit, Datei),
    HBQ = [],
    spawn(fun() -> loop(HBQ) end).

loop(HBQ) ->
    receive
        {request, pushHBQ, _List} -> ok
    end,
    io:write("Still alive"),
    loop(HBQ).