% C:\Program Files\Erlang OTP\bin\erl.exe
-module(main).
-import(io,[fwrite/1]).
-export([start/0,f/1,elem/2,elem2/2,silnia/1,elemr/2,usun/2,usunDup/1]).

start() ->
   fwrite("Hello, world!\n").

f(N) -> 2*N.

silnia(0) -> 1;
silnia(N) -> N * silnia(N - 1).
% [X|XS] = [1,2,3,4,5,6]
% X = 1
% XS = [2,3,4,5,6]

elem(_,[]) -> false;
elem(N,[X|XS]) -> 
  if N == X ->
      true;
  true -> 
      elem(N,XS)
  end.
       
elem2(_,[]) -> false;
elem2(N,[X,_]) when N == X -> true;
elem2(N,[_|XS]) -> elem2(N,XS).

elemr(_,[]) -> false;
elemr(A,[X|_]) when A == X -> true;
elemr(A,[X|XS]) when is_list(X) -> elemr(A,XS);
elemr(A,[_|XS]) -> elemr(A,XS).

usun(_,[]) -> [];
usun(E,[H|T]) when E == H -> usun (E,T);
usun(E,[H|T]) when is_list(H) -> [usun(E,H)|usun(E,T)];
usun(E,[H|T]) -> [H|usun(E,T)].
% usuwa też listę z listy

usunDup([]) -> [];
usunDup([H|T]) -> [H|usunDup(usun(H,T))].