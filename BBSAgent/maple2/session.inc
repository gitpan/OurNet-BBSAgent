Maple2/SOB Session

load maple2.base

=messsage
wait \e[1;33;46m��
till \e[37;45m\x20
till \x20\e[0m
exit

=message_reply
send \x12
wait �^�h�G
send $[message]\n
wait [Y]
send \n
wait \e[m
exit

=message_send
call talk_users
send /
wait �m�W�G
send $[receiver]\n
wait $[receiver]
send s
wait Call-In�G
send $[message]\n
wait [Y]
send \n
wait \e[m
exit

=talk
call main
send t\n
wait ��ѻ���
till Call��

=talk_users
call talk
send u\n
wait �𶢲��
till (TAB/f)

=talk_users_refresh
send \x15
wait �𶢲��
till (TAB/f)
exit

=chat_enter
call talk
send c\no\n\n
wait \x00\x00\x00\x00\x00

=chat_room
send /j $[room]\n
wait [$[room]]
exit

=chat_say
send $[message]\n
wait $[message]
exit

=chat_listen
wait ;1H\x00\x00\x00\x00\x00
till :\x20
  or *\x20
till \e[
exit

=chat_leave
send /bye\n
wait [C]
send c\n
goto main

=chat_message_send
send /m $[caller] $[message]\n
wait $[message]
exit