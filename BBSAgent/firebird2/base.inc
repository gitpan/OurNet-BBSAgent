Firebird2 Base

setv statustail ���d

=login
wait ���U�п�J
send $[username]\n$[password]\n\n\nqqqqqq
goto main

=main
send \e[D\e[D\e[D\e[D\e[D\e[D
wait �D���
till $[statustail]