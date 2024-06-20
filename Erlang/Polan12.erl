% Napisać moduł zadanie1, który uruchomi N procesów, z których
% każdy wypisze ”Proces ” oraz swój numer, i każdy zrobi to 5 razy
% (czyli uogólnienie programu ze slajdów, w którym procesy były
% dwa). Liczbę N przekazujemy jako argument funkcji start/1.
% Zad.1
-module (polan12).
-export ([start/1,wypisz/2, recvMessage/0, send/1, start2/0, client/2, server/0, start3/1,server2/0,client2/2,start4/1]).

wypisz(A, 1) -> 
    io:format("~p~n", [A]);
wypisz(A, N) ->
    io:format("~p~n", [A]), wypisz(A, N-1).

start(1) -> 
    io:format("
    PID: Proces ~p ~p~n", [1, spawn(polan12, wypisz, [ 1 , 5])]);
start(N) -> 
    io:format("
    PID: Proces ~p ~p~n", [N, spawn(polan12, wypisz, [N , 5])]), start(N-1).

% Napisać moduł zadanie2, który uruchomi 2 procesy, A i B. Proces
% A ma wysłać atom czesc do procesu B, a proces B potwierdzić
% jego odbiór. Przykładowe wyjście:
% Start procesu B, PID = <0.94.0>.
% Start procesu A, PID = <0.95.0>.
% Proces A wyslal atom czesc do B.
% Proces B otrzymał atom czesc.
% Zad.2
recvMessage() ->
    receive
        {Sender, Message} ->
            io:format("Proces ~p otrzymał atom ~p~n", [Sender, Message])
    end.

send(B) -> 
    B ! {self(), czesc},
    io:format("Proces ~p wyslal atom ~p do B~n", [self(), czesc]).
start2() ->
    B = spawn(polan12, recvMessage, []),
    A = spawn(polan12, send, [B]),
    io:format("Start procesu B, PID = ~p~n", [B]),
    io:format("Start procesu A, PID = ~p~n", [A]).

    % Napisać moduł zadanie3, który uruchomi 2 procesy, klient i
    % serwer. Klient ma otrzymać jako argumenty listę liczb całkowitych
    % oraz PID serwera i wysłać listę do serwera element po elemencie.
    % Serwer każdą otrzymaną liczbę ma podnieść do kwadratu i
    % wypisać. Po przesłaniu ostatniego elementu listy klient powinien
    % przesłać atom koniec, po otrzymaniu którego serwer ma zakończyć
    % działanie. Przykładowe wyjście:
    % Start serwera, PID = <0.87.0>.
    % Start klienta, PID = <0.88.0>, z listą [1,2,3].
    % 1
    % 4
    % 9
    % Serwer konczy dzialanie.
% Zad.3
client ([X], B) ->
    B ! {self(), X},
    io:format("~p~n", ["Koniec dzialania klienta"]);
client([X|XS], B) ->
    B ! {self(), X},
    io:format("~p~n", [X]),
    client(XS, B).

recvMsg()  ->
    receive
        {Sender, Message} ->
            X = Message * Message,
            io:format("Od ~p otrzymał liczbe = ~p i podniosl do kwadratu = ~p~n", [Sender,Message ,X]),
            recvMsg()
    end.

server() -> io:format("Start serwera, PID = ~p~n", [self()]),
    recvMsg().

start3(L) -> 
    Serwer = spawn(polan12, server, []),
    Klient = spawn(polan12, client, [L, Serwer]),
    io:format("Start klienta, PID = ~p, z lista ~p~n", [Klient, Serwer]).
    
    % adanie 4
    % Napisać moduł zadanie4 analogiczny do modułu zadanie3, ale
    % obliczenia powinny zostać przeprowadzone po stronie serwera (czyli
    % dostaje liczbę, oblicza kwadrat, odsyła wynik, i to klient wypisuje
    % wynik). Zamiast osobnego atomu koniec, serwer powinien sam się
    % wyłączyć po 3 sekundach, w których nie otrzymał komunikatu.
    % Przykładowe wyjście:
    % Start serwera, PID = <0.95.0>.
    % Start klienta, PID = <0.96.0>, z listą [1,2,3].
    % Klient wyslal 1.
    % Klient dostal 1.
    % Klient wyslal 2.
    % Klient dostal 4.
    % Klient wyslal 3.
    % Klient dostal 9.
    % Serwer konczy dzialanie.
    % (ostatnia linijka po 3 sekundach)
% Zad.4
client2 ([X], B) ->
    B ! {self(), X},
    io:format("~p~n", ["Koniec dzialania klienta"]),
    receive
        {Message} ->
            X = Message * Message,
            io:format(" ~p do kwadratu = ~p~n", [Message ,X])
    end;
client2([X|XS], B) ->
    B ! {self(), X},
    io:format("~p~n", [X]),
    receive
        {Message} ->
            X = Message * Message,
            io:format(" ~p do kwadratu = ~p~n", [Message ,X])
    end,
    client(XS, B).


server2() -> io:format("Start serwera, PID = ~p~n", [self()]),
    receive
    {B, Message} ->
        X = Message * Message,
        io:format("Od ~p otrzymał liczbe = ~p i podniosl do kwadratu.~p~n", [B,Message ,X]),
        B ! {X},
        server2()
    after 
        3000 ->
            io:format("Serwer konczy dzialanie.~n")
    end.

start4(L) -> 
    Serwer = spawn(polan12, server, []),
    Klient = spawn(polan12, client, [L, Serwer]),
    io:format("Start klienta, PID = ~p, z lista ~p~n", [Klient, Serwer]).

    % Napisać moduł zadanie5, który posiada osobną funkcję startującą
    % serwer i osobną funkcję startującą klienta. Należy też wykorzystać
    % do ich komunikacji register. Serwer ma w nieskończonej pętli
    % czekać na komunikaty, w zależności od tego, czy otrzyma liczbę,
    % czy listę, ma wypisać stosowną wiadomość. Po otrzymaniu atomu
    % koniec ma zakończyć działanie. W szczególności powinniśmy móc
    % wystartować serwer jednym wywołaniem, a potem móc wywoływać
    % wiele razy tworzenie klienta, za każdym razem z innym
    % argumentem i serwer ma odpowiadać na komunikaty przesyłane w
    % argumencie aż do otrzymania koniec.
% Zad.5
% 