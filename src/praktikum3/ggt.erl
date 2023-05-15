-module(ggt).
-export([start/0]).

start() ->
    pass.

loop() ->
    receive
        {setneighbors, LeftN, RightN} ->
            loop();
        {setpm, MiNeu} ->
            loop();
        {sendy, Y} ->
            loop();
        {calc, start} ->
            loop();
        {From, {vote, Initiator, MiIn}} ->
            loop();
        {voteYes, Name} ->
            loop();
        {From, toggle} ->
            loop();
        {From, tellmi} ->
            loop();
        {From, pingGGT} ->
            loop();
        kill ->
            loop()
    end.