--------------------
Compilieren der Dateien:
--------------------
Zu dem Paket gehören die Dateien
client_func.erl; clientS.erl; cmem.erl; dlq.erl; hbq.erl; client.cfg;
server_func.erl; serverS.erl; util.erl; vsutil.erl; server.cfg;

sowie:
Readme.txt;

shell: start (w)erl -make
% oder Erlang-Node:
1> make:all().
% oder
1> c(<Dateiname>).
% oder
1> c(<Dateiname>,[debug_info]).
% oder 


--------------------
Starten der Nodes:
--------------------
(w)erl -(s)name <ServerNodeName> -setcookie zummsel
(w)erl -(s)name <hbqNode-Name> -setcookie zummsel
(w)erl -(s)name <ClientNodeName> -setcookie zummsel

--------------------
Starten der HBQ:
--------------------
Wird mittels
   spawn(HBQnode,fun() -> hbq:initHBQ(DLQSize,HBQname) end)
gestartet. Achtung: die PID dieses spawn ist nicht die PID der HBQ!
Im vorgegebenen System wird die HBQ vom Server gestartet. 
Die Node der HBQ muss jedoch manuell gestartet werden.

Manuell startbar auf der Node der HBQ mit
1> HBQPID = hbq:initHBQ(DLQLimit,HBQName).

--------------------
Starten des Servers:
--------------------
1> serverS:start( ).
% oder in der Command-Shell z.B.
shell: start (w)erl -sname server -eval serverS:start().


% in der server.cfg:
% {latency, 60}. Zeit in Sekunden, die der Server bei Leerlauf wartet, bevor er sich beendet
% {clientlifetime,5}. Zeitspanne, in der sich an den Client erinnert wird
% {servername, wk}. Name des Servers als Atom
% {hbqname, hbq}. Name der HBQ als Atom
% {hbqnode, '<hbqNode-Name>@<NodeName>'}. Name der Node der HBQ als Atom
% {dlqlimit, 13}. Größe der DLQ

Starten des Clients:
--------------------
1> clientS:start( ).
% oder in der Command-Shell z.B.
shell: start (w)erl -sname clientA -eval clientS:start( ).

% 'server@lab33.cpt.haw-hamburg.de': Name der Server Node, erhält man zB über node()
% ' wegen dem - bei haw-hamburg, da dies sonst als minus interpretiert wird.
% in der client.cfg:
% {clients, 2}.  Anzahl der Clients, die gestartet werden sollen
% {lifetime, 42}. Laufzeit der Clients in Sekunden
% {sendeintervall, {3,9}}. Zeitabstand der einzelnen Nachrichten in Sekunden. Beim Start wird ein Zufallswert aus dem Intervall gewählt
% {servername, wk}. Name des Servers
% {servernode, '<ServerName>@<NodeName>'}. Node des Servers

Runterfahren:
-------------
2> Ctrl/Strg Shift G
-->q

Rücksetzen Variablen:
-------------
1> f(<VariablenName>).
2> f().

Anzeigen aller Variablen:
-------------
1> b().

Informationen zu Prozessen bzw. Modulen:
-------------
2> observer:start().
2> process_info(PID).
2> <Module>:module_info(). 
