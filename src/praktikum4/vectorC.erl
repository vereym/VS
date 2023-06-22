-module(vectorC).

-export([initVT/0]).
-export([myVTid/1]).
-export([myVTvc/1]).
-export([myCount/1]).
-export([foCount/2]).
-export([isVT/1]).
-export([syncVT/2]).
-export([tickVT/1]).
-export([compVT/2]).
-export([aftereqVTJ/2]).
-export_type([vectorTimestamp/0]).

-include_lib("eunit/include/eunit.hrl").

-import(vsutil, [get_config_value/2, now2string/1]).
-import(util, [logging/2]).
-import(io_lib, [format/2]).
-import(io, [format/1]).

-define(DELAY, 3000).

initVT() ->
    {ok, HostName} = inet:gethostname(),
    LogFile = format("VectorC@~s.log", [HostName]),

    %% 4.1 Auslesen von servername und servernode aus der Konfigureationsdatei
    TowerClockConfig =
        case file:consult("towerClock.cfg") of
            {ok, File} ->
                File;
            {error, Reason} ->
                logging(LogFile, format("~nProblem mit towerClock.cfg: ~s~n", [Reason])),
                exit(bad_config)
        end,
    {ok, ServerName} = get_config_value(servername, TowerClockConfig),
    {ok, ServerNode} = get_config_value(servernode, TowerClockConfig),

    %% 4.2 Kontaktaufbau zur towerClock
    VID =
        case net_adm:ping(ServerNode) of
            pang ->
                ErrReason = format("towerClockNode konnte nicht gefunden werden~n", []),
                logging(LogFile, ErrReason),
                %% {error, ErrReason},
                exit(list_to_atom(ErrReason));
            pong ->
                {ServerName, ServerNode} ! {getVecID, self()},
                receive
                    {vt, VTID} ->
                        VTID
                after ?DELAY ->
                    TimeOutErr = format("keine Antwort von towerClock erhalten~n"),
                    logging(LogFile, TimeOutErr),
                    {error, TimeOutErr}
                end
        end,

    %% 4.3, 4.4, 4.5
    {VID, createVectorZero(VID)}.

%% @doc ermittelt die eindeutige ID der Kommunikationseinheit
myVTid({ID, _List}) ->
    %% 5.1
    ID.

%% @doc ermittelt den Vektor eines Vektorzeitstempels
myVTvc({_ID, List}) ->
    %% 6.1
    List.

%% @doc ermittelt den Zaehlerwert zur eigenen ID aus einem Vektorzeitstempel
myCount({ID, Vec}) ->
    %%   ^ 7.1
    %% 7.2, 7.3
    lists_nth(ID, Vec).

myCount_test() ->
    TestList = [1, 2, 3, 4, 5, 6, 7],
    ?assertEqual(1, myCount({1, TestList})),
    ?assertEqual(7, myCount({7, TestList})),
    ok.

foCount(J, {_, Vec}) ->
    %%   ^ 8.1
    %% 8.2, 8.3
    lists_nth(J, Vec).

-type vectorTimestamp() :: {pos_integer(), [pos_integer(), ...]}.

%% @doc ueberprueft das Format eines Vektorzeitstempels
-spec isVT(VT) -> true | false when VT :: vectorTimestamp().
isVT({ID, Vec = [Elem | _]}) when is_integer(ID), length(Vec) =:= ID, is_integer(Elem) ->
    %% ^ 9.1, 9.2                      ^ 9.3             ^ 9.4          ^ 9.5
    %% 9.6
    true;
isVT(_Any) ->
    false.

syncVT({ID, Vec1}, {_, Vec2}) ->
    %%         ^ 10.1
    %% 10.2
    {Vector1Ext, Vector2Ext} = extendVector(Vec1, Vec2),
    %% 10.3, 10.4
    VectorNeu = zip_vt(Vector1Ext, Vector2Ext),
    %% 10.5, 10.6
    {ID, VectorNeu}.

zip_vt(Vec, Vec2) ->
    lists_reverse(zip_vt_internal(Vec, Vec2, [])).

zip_vt_internal([], [], Acc) ->
    Acc;
zip_vt_internal([H | Tail], [H2 | Tail2], Acc) when H >= H2 ->
    zip_vt_internal(Tail, Tail2, [H | Acc]);
zip_vt_internal([H | Tail], [H2 | Tail2], Acc) when H < H2 ->
    zip_vt_internal(Tail, Tail2, [H2 | Acc]).

zip_vt_test() ->
    TestList = [1, 2, 3, 4, 5, 10, 7],
    TestList2 = [1, 9, 3, 9, 5, 6, 7],
    Outcome = [1, 9, 3, 9, 5, 10, 7],
    ?assertEqual(Outcome, zip_vt(TestList, TestList2)),
    ok.

syncVT_test() ->
    TestVecC = {7, [1, 2, 3, 4, 5, 10, 7]},
    TestVecC2 = {10, [1, 9, 3, 9, 5, 6, 7, 0, 2, 21]},
    Outcome = {7, [1, 9, 3, 9, 5, 10, 7, 0, 2, 21]},
    ?assertEqual(Outcome, syncVT(TestVecC, TestVecC2)),
    ok.

%% (14.)
extendVector(Vec, Vec2) ->
    %% 14.1
    Diff = length(Vec) - length(Vec2),
    %% 14.2
    if
        Diff > 0 ->
            %% 14.4
            {Vec, Vec2 ++ createVectorZero(Diff)};
        Diff < 0 ->
            %% 14.4
            {Vec ++ createVectorZero(abs(Diff)), Vec2};
        Diff =:= 0 ->
            %% 14.4
            {Vec, Vec2}
    end.

%% 14.3
-spec createVectorZero(Num) -> [0, ...] when Num :: pos_integer().
createVectorZero(1) ->
    [0];
createVectorZero(Counter) when Counter > 1 ->
    [0 | createVectorZero(Counter - 1)].

createVectorZero_test() ->
    ?assertEqual([0], createVectorZero(1)),
    ?assertEqual([0, 0], createVectorZero(2)),
    ok.

extendVector_test() ->
    TestList = [1, 2, 3, 4],
    TestList2 = [1, 2, 3, 4, 5, 6, 7],

    {Out, _} = extendVector(TestList, TestList2),
    ?assertEqual(length(TestList2), length(Out)),
    ?assertEqual({[1, 2, 3, 4, 0, 0, 0], TestList2}, extendVector(TestList, TestList2)),
    ?assertEqual({TestList, TestList}, extendVector(TestList, TestList)),
    ok.

tickVT({ID, Vec}) ->
    %%  ^ 11.1
    %% 11.2, 11.3, 11.4, 11.5
    {ID, tickVT_internal(ID, Vec)}.

tickVT_internal(1, [Elem | Tail]) ->
    [Elem + 1 | Tail];
tickVT_internal(ID, [H | T]) ->
    [H | tickVT_internal(ID - 1, T)].

tickVT_internal_test() ->
    TestList = [1, 2, 3, 4, 5, 6, 7],
    ModifiedList = [1, 2, 3, 4, 5, 6, 8],
    ModifiedList2 = [1, 2, 3, 5, 5, 6, 7],
    ?assertEqual(ModifiedList, tickVT_internal(7, TestList)),
    ?assertEqual(ModifiedList2, tickVT_internal(4, TestList)),
    ok.

%% (12.)
compVT({_, Vec}, {_, Vec2}) ->
    %%       ^ 12.1
    %% 12.2
    {Vektor1Ext, Vektor2Ext} = extendVector(Vec, Vec2),
    %% 12.3
    compareVector(Vektor1Ext, Vektor2Ext).

%% @doc (13.) Vergleicht zwei Vektorzeitstempel im Sinne des kausalen Multicasts.
%% @param VT eigener Vektorzeitstempel
%% @param VTR Vektorzeitstempel einer erhaltenen Nachricht
aftereqVTJ(_VT = {JD, _}, _VTR = {JD, _}) ->
    {aftereq, 0};
aftereqVTJ(_VT = {_, Vektor1}, _VTR = {JD, Vektor2}) ->
    %%                                 ^ 13.1
    %% 13.2
    {DistVT, VecRest} = lists_take_nth(JD, Vektor1),
    {DistVTR, VecRestR} = lists_take_nth(JD, Vektor2),
    %% 13.3
    {Vektor1Ext, Vektor2Ext} = extendVector(VecRest, VecRestR),
    %% 13.4
    %% FIXME hier werden zwei leere Vektoren verglichen, aber was soll passieren, wenn wir
    %%       zwei leere vektoren erhalten?
    case compareVector(Vektor1Ext, Vektor2Ext) of
        %% 13.5
        beforeVT ->
            false;
        concurrentVT ->
            false;
        %% 13.6
        _ ->
            {aftereqVTJ, DistVT - DistVTR}
    end.

aftereqVTJ_test() ->
    TestVecC = {1, [0, 0]},
    TestVecC2 = {2, [0, 1]},

    ?assertEqual(false, aftereqVTJ({1, [1, 1]}, {2, [8, 1]})),
    ?assertEqual({aftereqVTJ, 0}, aftereqVTJ(TestVecC2, TestVecC)),
    ok.

%% Hilfsfunktionen %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 15.
-spec compareVector(Vec, Vec2) -> beforeVT | equalVT | afterVT | concurrentVT when
    Vec :: [integer(), ...],
    Vec2 :: [integer(), ...].
compareVector([Elem | Tail], [Elem2 | Tail2]) ->
    %% 15.1
    case compareElem(Elem, Elem2) of
        beforeVT ->
            compareVectorLast(Tail, Tail2, beforeVT);
        equalVT ->
            compareVectorLast(Tail, Tail2, equalVT);
        afterVT ->
            compareVectorLast(Tail, Tail2, afterVT)
    end.

%% Last :: beforeVT|equalVT|afterVT
compareVectorLast([], [], Last) ->
    Last;
compareVectorLast([Elem | Tail], [Elem2 | Tail2], Last) ->
    NewComp = compareElem(Elem, Elem2),
    case (NewComp == Last) or (NewComp == equalVT) of
        true ->
            compareVectorLast(Tail, Tail2, Last);
        %% 15.2
        false ->
            concurrentVT
    end.

compareElem(Elem, Elem2) ->
    if
        Elem < Elem2 ->
            beforeVT;
        Elem =:= Elem2 ->
            equalVT;
        Elem > Elem2 ->
            afterVT
    end.

compareElem_test() ->
    ?assertEqual(beforeVT, compareElem(1, 4)),
    ?assertEqual(equalVT, compareElem(1, 1)),
    ?assertEqual(afterVT, compareElem(9, 1)),
    ok.

compareVector_test() ->
    TestList = [1, 2, 3, 4, 5, 6, 7],
    TestList2 = [1, 2, 3, 4, 5, 6, 7],
    ?assertEqual(equalVT, compareVector(TestList, TestList2)),
    ok.

%% util %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generelle Hilfsfunktionen (nicht im Entwurf dokumentiert)

-spec lists_nth(N, List) -> Elem when
    N :: pos_integer(),
    List :: list(),
    Elem :: term() | [].
lists_nth(_Num, []) ->
    [];
lists_nth(1, [H | _]) ->
    H;
lists_nth(N, [_ | T]) when N > 1 ->
    lists_nth(N - 1, T).

lists_nth_test() ->
    TestList = [1, 2, 3, 4, 5, 6, 7],
    ?assertEqual(3, lists_nth(3, TestList)),
    ?assertEqual(1, lists_nth(1, [1])),
    ok.

%% @doc reverses a list.
lists_reverse(List) ->
    lists_reverse(List, []).

lists_reverse([], Accu) ->
    Accu;
lists_reverse([Elem | Tail], Accu) ->
    lists_reverse(Tail, [Elem | Accu]).

lists_reverse_test() ->
    TestList = [1, 2, 3, 4, 5, 6, 7],
    Outcome = [7, 6, 5, 4, 3, 2, 1],
    ?assertEqual(Outcome, lists_reverse(TestList)),
    ?assertEqual([], lists_reverse([])),
    ok.

%% @doc takes the nth element in a list and returns the element and
%%      a new list without the element.
-spec lists_take_nth(Position, List) -> {Elem, NewList} when
    Position :: pos_integer(),
    List :: list(),
    NewList :: list(),
    Elem :: term() | [].
lists_take_nth(Num, []) ->
    {Num, []};
lists_take_nth(1, [Elem | Tail]) ->
    {Elem, Tail};
lists_take_nth(Position, [E | T]) ->
    {Elem, Tail} = lists_take_nth(Position - 1, T),
    {Elem, [E | Tail]}.

lists_take_test() ->
    ?assertEqual({3, [1, 2, 4, 5, 6, 7]}, lists_take_nth(3, [1, 2, 3, 4, 5, 6, 7])),
    ?assertEqual({1, []}, lists_take_nth(1, [1])),
    ?assertEqual({1, []}, lists_take_nth(1, [])),
    ok.
