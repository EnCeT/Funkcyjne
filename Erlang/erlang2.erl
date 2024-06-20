-module(erlang2).
-export([proces/0,start/0,process/1,iterStarter/1,start2/1]).

proces() -> 
    receive
        {Sender, Message} ->
            X = Message + 1,
            io:format("Proces otrzymał atom ~p i wysłał ~p ~n", [Message, X]),
            Sender ! {self(), X}
    end.

start() -> 
    B = spawn(erlang2, proces, []),
    A = spawn(erlang2, proces, []),
    B ! {A, 1}.

%    
process(B) ->
    receive
        { Message} ->
            io:format("Proces otrzymał atom ~p~n", [Message +1]),
            B ! { Message + 1}      
    end,
    process(B).

start2(N)->
    register(proces1, iterStarter(N)),
    proces1 ! 1.

iterStarter(1) ->
    spawn(erlang2, process, [process1]);
iterStarter(N) ->
    spawn(erlang2, process, [iterStarter(N-1)]).


serwer (Result) -> 
    recive
        {"Dodaj", Message} ->
            io:format("Proces dodał ~p~n",[Message]),
            X = Message + Result,
            serwer(X);
        {"Pomnóż", Message} ->
            io:format("Proces pomnożył ~p~n",[Message]),
            X = Message * Result,
            serwer(X);
        {"Wynik", Sender} ->
            io:format("Proces wysłał wynik ~p~n",[Result]),
            Sender ! {self(), Result},
            serwer(Result);
        {"Koniec", Sender} ->
            io:format("Proces zakończył działanie ~p~n",[Result]),
            Sender ! {self(), Result}   
    end.

dodaj (X, Y, N) ->
    X ! {"Dodaj", Y},
    dodaj (X, Y, N-1);


start3(N) ->
    register spawn(erlang2, serwer, [0]),
    
