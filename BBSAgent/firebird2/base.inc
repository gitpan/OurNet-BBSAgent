Firebird2 Base

setv statustail 停留

=login
wait 註冊請輸入
send $[username]\n$[password]\n\n\nqqqqqq
goto main

=main
send \e[D\e[D\e[D\e[D\e[D\e[D
wait 主選單
till $[statustail]
