Maple2/SOB Board

load maple2.base

setv cursor	��
setv morehead	m/\e\[0?m\e\[(?:[\d;]+)m\x20\x20�s��\x20P./
setv moretail	m/\[q\]\e\[30m����\x20\x20\x20\e\[0?m/
setv postsign	0 # ñ�W�ɦb post ���

# ���]�b�D���άݪO��, ���i�H select board
=board
send s
wait �W��
send $[board]\n\x20\x20
wait �O�D�G
till �峹��Ū

=board_list_last
call board $[board]
send 1\n\x24h\x20
wait �O�D�G
wait \x0d$[cursor]
till /
exit

=board_article_fetch_first
call board $[board]
send $[article]\n\n
till $[morehead]
wait (
till )
wait $[moretail]
exit

=board_article_fetch_next
send \x20
till $[morehead]
wait (
till )
wait $[moretail]
exit

=board_article_fetch_last
send qq
wait �峹��Ū
exit

=board_list_author
call board $[board]
send a$[author]\n
wait $[cursor]
till �峹��Ū
exit

=article_post_raw
call board $[board]
send \x10$[title]\n
doif $[postsign]
else
  send 0\n
endo
send $[body]
# clean tailer (x-origin)
# send \x14\x19\x19\x19 # delete by fascist autrijus
send \x18s\n
doif $[postsign]
  send 0\n
endo
send \x20
doif $[author]
    call article_edit_author $[author]
endo # ?
exit

=article_edit_author
send \x24E\x19\x19\x19\x19
# 1 empty 2 from 3 origin 4 --
send \x14\x19\x19
send \x18\n
wait �峹��Ū
exit

=article_edit_raw
send E$[content]\x18
wait �ɮ׳B�z
send s\n
doif $[postsign]
  send 0\n
endo
send \x20
doif $[author]
    call article_edit_author $[author]
endo
exit