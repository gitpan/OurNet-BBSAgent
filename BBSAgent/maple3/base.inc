Maple3/Melix Base

=login
wait 代號：
send $[username]\n
doif $[password]
    wait 密碼：
    send $[password]\nn\n
endo
send \n\n\n\n

=main
send qqqqqqee
wait 主功能表
till 呼叫器

=logoff
send eeeeeeeeeeee\nn\ny\ny\n\n\n
