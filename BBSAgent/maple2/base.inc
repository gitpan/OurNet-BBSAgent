Maple2/SOB Base

setv pager      扣機

=login
wait 註冊：
send $[username]\n
send $[password]\n
send n\nq\x20q\x20q\x20q\x20q
goto main

=main
send \e[D\e[D\e[D\e[D\e[D\e[D
wait 主功能表
till $[pager]

=logoff
call main
send g\ny\n\n\n\n
exit
