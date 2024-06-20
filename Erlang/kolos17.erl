-module (rok16_3).
-export ([start/1,worker/0]).

start(N) ->
    start(N, []).

start(0, [X|Pids]) ->
    X ! {[1], Pids},
    Pids;

start(N, Pids) ->
    Pid = spawn(rok16_3, worker, []),
    start(N - 1, [Pid | Pids]).
 
worker() ->
    receive
        {[Y|VAL], [X|Pids]} -> 
            io:format("worker VALUE: ~p pid:~p~n", [[Y|VAL], self()]),
            X ! {[(Y + 1)] ++ [Y] ++ VAL, Pids ++ [self()]}
    end,
    worker().