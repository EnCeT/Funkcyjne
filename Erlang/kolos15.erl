-module(kolos15).
-export([start/1, start/2, process/0, process/1]).

start(N) ->
    Last = spawn(zad3, process, []),
    First = start(N - 1, Last),

    Last ! First,
    First ! 0.

start(0, _) ->
    ok;
start(N, Next) ->
    Curr = spawn(zad3, process, [Next]),

    case start(N - 1, Curr) of
        ok ->
            Curr;
        PID ->
            PID
    end.

process() ->
    receive
        Next ->
            process(Next)
    end.

process(Next) ->
    receive
        Number ->
            Next ! Number + 1,
            io:format("~p otrzymał ~p.~n", [self(), Number]),
            process(Next)
    end.