Firebird2 Board

load firebird2.base

setv headl \x0d?
setv headr :\x20(.+?)(?:,\x20�H��:\x20.+)?$
setv headi �o�H�H,��\x20\x20�D,�o�H��
setv separator \e\[\\d;1H

=board
call main
send s\n
wait �۰ʷj�M):
send $[board]\nr\n\x20
wait �O�D:
till $[statustail]

=board_list_last
call board $[board]
send 1\n\x24
wait \x0d>
till �ɶ�
exit

=board_article_fetch_first
call board $[board]
send $[article]\n\n
wait \e[J
till \e[1;44;31m[�\Ū�峹]
  or \e[1;44;32m�U���٦���
till ����
wait m/\^X\x20?��\x20?p\x20\e\[m/
  or �x\x20\x20\x20\x20\x20\e[m
exit

=board_article_fetch_next
send \x20
till \e[1;44;31m[�\Ū�峹]
  or \e[1;44;32m�U���٦���
till ����
wait m/\^X\x20?��\x20?p\x20\e\[m/
  or �x\x20\x20\x20\x20\x20\e[m
exit

=board_article_fetch_last
send q
wait \e[1;44;33m�ɶ�
exit

=article_post_raw
call board $[board]
send \x10$[title]\n\n
send $[body]
send \x18s\n
send \x20
exit