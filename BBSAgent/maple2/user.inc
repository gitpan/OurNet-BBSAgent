# $File: //depot/OurNet-BBSAgent/BBSAgent/maple2/user.inc $ $Author: autrijus $
# $Revision: #2 $ $Change: 2217 $ $DateTime: 2001/10/27 06:03:10 $
# Contributed by Smartboy <kcwu@ck.tp.edu.tw>

Maple2/SOB User

load maple2.base

=configure
call main
send c\n
wait ��L�\��
till Call��

=mood_get
call configure
send m\n
wait �ثe�߱�\x20\e[1;31m\x20
till \x20\e[
wait �ĴX���G
send \n
exit

=mood_set
call configure
send m\n
wait �ĴX���G
send 0\n$[mood]\n
exit

=mail
call main
send m\n
wait �q�l�l��
till Call��

=mail_send
call mail
send s\n
wait �N���G
send $[receiver]\n
wait �D�D�G
send $[subject]\n
wait �s��峹
send $[content]\x18
wait �ɮ׳B�z
send s\n\n
wait ���Z
doif $[savelocal]
    send \n
else
    send n\n
endo
send \n