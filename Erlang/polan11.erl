-module(polan11).
% -import(Module).
-export([fib/1,srednia/1,sum/1,posNeg/1,split/1,rownaniekwadratowe/3,sublist/2]).

% Zadanie 1
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

% Zadanie 2
sum([]) -> 0;
sum([H|T]) -> H + sum(T).

% length([]) -> 0;
% length([_|T]) -> 1 + length(T).

srednia([]) -> 0;
srednia(L) -> sum(L) / length(L).

% Zadanie 3
posNeg([]) -> {0,0};
posNeg([H|T]) -> 
    {P,N} = posNeg(T),
    if H >= 0 -> {P+1,N};
       H < 0 -> {P,N+1};
       true -> {P,N}
    end.

% Zadanie 4
split([]) -> {0,0};
split([H|T]) ->
    {P,N} = split(T),
    if  (is_integer(H) == true) -> {P+1,N};
        (is_integer(H) == false) -> {P,N+1};
       true -> {P,N}
    end.

% Zadanie 5
rownaniekwadratowe(A,B,C) ->
    D = B*B - 4*A*C,
    if  D > 0 -> {(-B + math:sqrt(D))/(2*A),(-B - math:sqrt(D))/(2*A)};
        D == 0 -> {-B/(2*A)};
        D < 0 -> 'brakRozwiazania'
    end.

% Zadanie 6
generate_sublists([], _, Acc) ->
    lists:reverse(Acc);
generate_sublists([H | T], K, Acc) when length([H | T]) >= K ->
    Sublist = lists:sublist([H | T], K),
    generate_sublists(T, K, [Sublist | Acc]);
generate_sublists(_, _, Acc) ->
    lists:reverse(Acc).

sublists(L, K) -> generate_sublists(L, K, []).
