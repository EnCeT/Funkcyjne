% Zadanie 3. Napisa´c moduł tworz ˛acy w nast˛epuj ˛acy sposób M-arne drzewo komunikuj ˛acych
% si˛e procesów o wysoko´sci N. Liczby M, N i X s ˛a naturalne, wi˛eksze od zera.
% • Pierwszy proces (stanowi ˛acy korzen drzewa na gł˛eboko ´ ´sci 0), wywołany z parametrem X, tworzy M procesów i przekazuje im jako parametr liczby M X, ..., M X + M − 1.
% Nast˛epnie czeka na komunikaty od wszystkich potomków, konkatenuje je i dokłada X
% jako głow˛e oraz wypisuje tak utworzon ˛a list˛e na ekran.
% • Ka˙zdy nast˛epny proces:
% – je˙zeli jest na gł˛eboko´sci mniejszej ni˙z N, to tworzy M procesów podobnie jak proces
% pierwszy, czeka na komunikaty, konkatenuje je dokładaj ˛ac X jako głow˛e, a list˛e
% wypisuje na ekran i odsyła do rodzica,
% – je˙zeli jest na gł˛eboko´sci N, to wypisuje swój parametr X na ekran i odsyła do rodzica
% jako jednoelementow ˛a list˛e.
% • Funkcja start(M, N, X) tworzy pierwszy proces w taki sposób, by przekaza´c mu parametr X i doprowadzi´c do zbudowania drzewa w opisany powy˙zej sposób.
% • Procesy mog ˛a otrzymywa´c jako parametry nie tylko liczby X; nie mog ˛a jednak otrzymywa´c ˙zadnych list.
% Przykładowo, wywołanie start(2, 2, 1) powinno spowodowa´c wypisanie nast˛epuj ˛acych list:
% [4], [5], [6], [7], [2,4,5], [3,6,7], [1,2,4,5,3,6,7]. Kolejno´s´c list mo˙ze by´c inna, zale˙znie od
% kolejno´sci dostarczania komunikatów i dost˛epu do procesora.
-module(Zadanie3).
-export([]).

process (M, N, 0) ->
    
    
start (M, N, X) ->
    Pid = spawn(Zadanie3, process, [M, N, X]),
    Pid ! {self(), X},
    receive
        {Pid, List} ->
            io:format("~p~n", [List])
    end.