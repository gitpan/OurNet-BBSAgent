Maple2/SOB Base

setv pager      ����

=login
wait ���U�G
send $[username]\n
send $[password]\n
send n\nq\x20q\x20q\x20q\x20q
goto main

=main
send \e[D\e[D\e[D\e[D\e[D\e[D
wait �D�\���
till $[pager]

=logoff
call main
send g\ny\n\n\n\n
exit