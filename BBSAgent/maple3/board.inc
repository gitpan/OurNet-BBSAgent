Maple3/Melix Board

load maple3.base

=board
call main
send b\n
send s
wait �W��
send $[board]\n\x20\x20
wait �O�D�G
wait ��  ��  ��  �D

=board_list_last
call board $[board]
send 1\n\x24h\x20 # 1, $, h, space
wait �O�D�G
wait \x0d>
till /
exit

=board_article_fetch_first
call board $[board]
send $[article]\n\n
wait \e[2J
till \e[0;34;46m
  or \e[34;46m
till \e[31;47m (
wait \e[m
exit

=board_article_fetch_next
send \n
till \e[0;34;46m
  or \e[34;46m
till \e[31;47m (
wait \e[m
exit

=board_article_fetch_last
send q
till �O�D�G
exit

# ñ�W�� post �e�N��
=article_post_raw
call board $[board]
send \x10$[title]\n0\n$[body]
# clean header
send \x13\x19\x19\x19\x19
# save
send \x18s\n\x20
exit