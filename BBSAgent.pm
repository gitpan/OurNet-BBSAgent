package OurNet::BBSAgent;
require 5.005;

$OurNet::BBSAgent::VERSION = '1.51';

use strict;
use vars qw/$AUTOLOAD/;
use Net::Telnet;

=head1 NAME

OurNet::BBSAgent - Scriptable telnet-based virtual users

=head1 SYNOPSIS

    # To run it, make sure you have a 'cvic.bbs' file in the same
    # directory. Its contents is listed just below this section.

    use OurNet::BBSAgent;

    my $cvic = new OurNet::BBSAgent('cvic.bbs', undef, 'testlog');

    # $cvic->{debug} = 1; # Turn on for debugging

    $cvic->login($ARGV[0] || 'guest', $ARGV[1]);
    print "now at $cvic->{state}";
    $cvic->Hook('balloon', \&callback);
    $cvic->Loop;

    sub callback {
        ($caller, $message) = @_;
        print "Received: $message\n";
        exit if $message eq '!quit';
        $cvic->balloon_reply("$caller, I've got your message!");
    }

=head1 DESCRIPTION

OurNet::BBSAgent provides an object-oriented interface to TCP/IP-based
interactive services (e.g. BBS, IRC, ICQ and Telnet), by simulating as
a "virtual user" with action defined by a script language. The developer
could then use the same methods to access different services, to easily
implement interactive robots, spiders, or other cross-service agents.

=head2 Site Description File

This module has its own scripting language, which looks like this in
a site description file:

    CVIC BBS
    cvic.org:23

    =login
    wait 註冊
      or 使用者


    send $[username]\n
    doif $[password]
        wait 密碼
        send $[password]\n\n
    endo
    send \n\n\n
    goto main

    =main
    send eeeeeeee
    wait 主功能表
    till Call機

    =balloon
    wait \e[1;33;46m★
    till \e[37;45m\x20
    till \x20\e[0m
    exit

    =balloon_reply
    send \x12
    wait 回去：
    send $[message]\n
    wait [Y]
    send \n
    wait \e[m
    exit

The first two lines describes the service's title, its IP address and
port number. Any number of 'procedures' then begins with C<=procname>,
which could be called like C<$object->procname([arguments])> in the
program. Each procedure is made by any number of following directives:

=over

=item load FILENAME

This directive must be used before any procedures. It loads another
BBS definition file under the same directory (or current directory).

=item wait STRING
=item till STRING
=item   or STRING

Tells the agent to wait until STRING is sent by remote host. Might time
out after C<$self->{timeout}> seconds. Any trailing C<or> directives
specifies an alternative string to match.

If STRING is of format C<m/.*/[imsx]*>, it will be treated as a regular
expression. This is an B<experimental> feature and the interface is
subject to change.

Additionally, C<till> puts anything between the last C<wait> or C<till>
and STRING into the return list.

=item send STRING

Sends STRING to remote host.

=item doif CONDITION
=item else CONDITION
=item endo

The usual flow control directives. Nested C<doif...endo>s is supported.

=item goto PROCEDURE
=item call PROCEDURE

Executes another procedure in the site description file. C<goto> never
returns, while C<call> always will. Also, a C<call> will not occur if
the destination was the last executed procedure that does not end with
C<exit>.

=item exit

Marks the termination of a procedure; also means this procedure is not
a 'state' - that is, multiple C<call>s to it will all be executed.

=item setv VAR STRING

Sets a global, non-overridable variable (see below).

=item idle TIME

Sleep that much seconds.

=item back

=head2 Variable Handling

Whenever a variable in the form of $[name] is encountered as a part of
a directive, it is looked up in the global 'setv' hash C<$self->{var}>
first, then at the procedure-scoped variable hash, and is shift()'ed
from the argument list if none are found.

For example:

    setv foo World!

    =login
    send $[bar]      # sends the first argument
    send $[foo]      # sends 'World!'
    send $[password] # sends the second argument
    send $[username] # sends the first argument again

A notable exception is digits-only subscripts (e.g. $[1]), which contains
the matched string in the previous 'wait' or 'till' directive. If there
are multiple strings via 'or' directives, the subscript correspond to the
matched alternative. For example:

    =match
    wait foo
      or m/baz+/
    doif $[1] # 'foo' matched
        send $[1] # sends 'foo'
    else
        send $[2] # sends 'bazzzzz...'
    endo

=head2 Event Hooks

In addition to call the procedures one-by-one, you can 'hook' those
that begins with 'wait' (or 'call' and 'wait') so whenever the strings
they expected are received, the responsible procedure is immediately
called. You can also supply a call-back function to handle its results.

For example, the code in L</SYNOPSIS> above 'hooks' a callback function
to procedure 'balloon', then enters a event loop by calling C<Loop>,
which never terminates except when the agent receives '!quit' via the
balloon procedure.

The internal hook table could be accessed by $obj->{hook}. It is
implemented via a hash of hash of hash of lists -- Kids, don't try
this at home!

=cut

# ---------------
# Variable Fields
# ---------------
use fields qw/bbsname bbsaddr bbsport bbsfile lastmatch loadstack
              debug timeout state proc var netobj hook loop errmsg/;

# --------------------------------------------
# Subroutine new($bbsfile, $timeout, $logfile)
# --------------------------------------------
sub new {
    my $class = shift;
    my $self  = ($] > 5.00562) ? fields::new($class)
                               : do { no strict 'refs'; bless [\%{"$class\::FIELDS"}], $class };

    $self->{bbsfile} = shift;
    $self->{timeout} = shift;

    die("Cannot find bbs definition file: $self->{bbsfile}")
        unless -e $self->{bbsfile};

    open(local *_FILE, $self->{bbsfile});

    chomp($self->{bbsname} = <_FILE>);
    chomp(my $addr = <_FILE>);

    if ($addr =~ /^(.*?)(:\d+)?$/) {
        $self->{bbsaddr} = $1;
        $self->{bbsport} = substr($2, 1) || 23;
    }
    else {
        die("Malformed location line: $addr");
    }

    close *_FILE;

    $self->loadfile($self->{bbsfile});
    $self->{netobj} = Net::Telnet->new(
    	Timeout => $self->{timeout},
		Errmode => sub { $self->{errmsg} = $_[0]; die($_[0]); },
	);
    $self->{netobj}->open('Host' => $self->{bbsaddr},
                          'Port' => $self->{bbsport});
    $self->{netobj}->output_record_separator('');
    $self->{netobj}->input_log($_[0]) if $_[0];

    $self->{state} = '';

    return $self;
}

sub _plain {
  my $str = $_[0];

  $str=~ s/([\x00-\x20])/sprintf("\\x%02x", ord($1))/eg;

  return $str;
}

sub loadfile {
    my ($self, $bbsfile) = @_;

    return if $self->{loadstack}{$bbsfile}++;

    open(local *_FILE, $bbsfile) or die "cannot find file: $bbsfile";
    <_FILE>; <_FILE>; # skip headers

    while (my $line = <_FILE>) {
        chomp $line;
        next if $line =~ /^#|^\s*$/;

        if ($line =~ /^=(\w+)$/) {
            $self->{state}    = $1;
            $self->{proc}{$1} = [];
        }
        elsif ($line =~ /^\s*(idle|load|doif|endo|goto|call|wait|send|else|till|setv|exit)\s*(.*)$/) {
            if (!$self->{state}) {
                # directives must belong to procedures...
                if ($1 eq 'setv') {
                    # ...but 'setv' is an exception.
                    my ($var, $val) = split(/\s/, $2, 2);
                    $val =~ s/\x5c\x5c/_!!!_/g;
                    $val =~ s/\\n/\015\012/g;
                    $val =~ s/\\e/\e/g;
                    $val =~ s/\\x([0-9a-fA-F][0-9a-fA-F])/chr(hex($1))/eg;
                    $val =~ s/_!!!_/\x5c/g;

                    $val =~ s{\$\[([^\]]+)\]}{
                         (exists $self->{var}{$1}? $self->{var}{$1} : '!notfound!')
                    };
                    $self->{var}{$var} = $val;
                }
                elsif ($1 eq 'load') {
                    # ...but 'load' is another exception.
                    $self->loadfile(
                        -e $2 ? $2 :
                        substr($bbsfile, 0, rindex($bbsfile, '/') + 1) . $2
                    );

                    $self->{state} = '';
                }
                else {
                    die('Not in a procedure');
                }
            }
            push @{$self->{proc}{$self->{state} || ''}}, $1, $2;
        }
        elsif ($line =~ /^\s*or\s*(.+)$/) {
            die('Not in a procedure') unless $self->{state};
            die('"or" directive not after a "wait" or "till"')
                unless $self->{proc}{$self->{state}}->[-2] eq 'wait'
                    or $self->{proc}{$self->{state}}->[-2] eq 'till';

            ${$self->{proc}{$self->{state}}}[-1] .= "\n$1";
        }
        else {
            warn("Error parsing '$line'");
        }
    }

}

# ---------------------------------------
# Subroutine Unhook($self, $procedure)
# ---------------------------------------
# Unhooks the procedure from event table.
# ---------------------------------------
sub Unhook {
    my $self = shift;
    my $sub  = shift;

    if (exists $self->{proc}{$sub}) {
        my ($state, %var);
        my @proc = @{$self->{proc}{$sub}};

        $state = $self->_chophook(\@proc, \%var, \@_);

        print "Unhook $sub\n" if $self->{debug};
        delete $self->{hook}{$state}{$sub};
    }
    else {
        die "Unhook: undefined procedure '$sub'";
    }
}

# -----------------------------------------------------------
# Subroutine Unhook($self, $procedure, [\&callback], [@args])
# -----------------------------------------------------------
# Adds a procedure from event table, with optional callback
# functions and procedure parameters.
# -----------------------------------------------------------
sub Hook {
    my $self = shift;
    my ($sub, $callback) = splice(@_, 0, 2);

    if (exists $self->{proc}{$sub}) {
        my ($state, $wait, %var) = '';
        my @proc = @{$self->{proc}{$sub}};

        ($state, $wait) = $self->_chophook(\@proc, \%var, [@_]);

        print "Hook $sub: State=$state, Wait=$wait\n" if $self->{debug};

        $self->{hook}{$state}{$sub} = [$sub, $wait, $callback, @_];
    }
    else {
        die "Hook: Undefined procedure '$sub'";
    }
}

# -------------------------------------------------------------
# Subroutine Loop($self, [$timeout])
# -------------------------------------------------------------
# Loops for $timeout seconds, or indefinitely if not specified.
# -------------------------------------------------------------
sub Loop {
    my $self = shift;

    do {
        $self->Expect(undef, defined $_[0] ? $_[0] : -1);
    } until (defined $_[0]);
}

# --------------------------------------------------------------
# Subroutine Expect($self, [$string], [$timeout])
# --------------------------------------------------------------
# Implements the 'wait' and 'till' directive depends on context.
# Note multiple strings could be specified in one $string by
# using \n as delimiter.
# --------------------------------------------------------------
sub Expect {
    my $self    = shift;
    my $param   = shift;
    my $timeout = shift || $self->{timeout};

    if ($self->{netobj}->timeout() ne $timeout) {
        $self->{netobj}->timeout($timeout);
        print "Timeout change to $timeout\n" if $self->{debug};
    }

    my (@keys, $retval, $retkey, $key, $val, %wait);

    while (($key, $val) = each %{$self->{hook}{$self->{state}}}) {
        push @keys, $val->[1] unless exists $wait{$val->[1]};
        $wait{$val->[1]} = $val;
    }

    if (defined $self->{state}) {
        while (($key, $val) = each %{$self->{hook}{''}}) {
            push @keys, $val->[1] unless exists $wait{$val->[1]};
            $wait{$val->[1]} = $val;
        }
    }

    if (defined $param) {
        foreach my $key (split('\n', $param)) {
            push @keys, $key unless exists $wait{$key};
            $wait{$key} = undef;
        }
    }

    # Let's see the counts...
    return unless @keys;

    print "Waiting: [", _plain(join(",", @keys)), "]\n" if $self->{debug};

	($retval, $retkey) = ($self->{netobj}->waitfor(map {
		m|^m/.*/[imsx]*$| ? ('Match' => $_) : ('String' => $_)
	} @keys));

    if ($retkey) {
        # which one matched?
        $self->{lastmatch} = [];

        foreach my $idx (0 .. $#keys) {
            $self->{lastmatch}[$idx+1] =
                ($keys[$idx] =~ m|^m/.*/[imsx]*$|
                    ? (eval{"\$retkey =~ $keys[$idx]"})
                    : $retkey eq $keys[$idx]) ? $retkey : undef;
        }
    }

    return if $self->{errmsg};

    if ($wait{$retkey}) {
        # Hook call.
        $AUTOLOAD = $wait{$retkey}->[0];

        if (ref($wait{$retkey}->[2]) eq 'CODE') {
            &{$wait{$retkey}->[2]}(
                # &{$wait{$retkey}->[0]}
                $self->AUTOLOAD(\'1', @{$wait{$retkey}}[3..$#{$wait{$retkey}}])
            );
        }
        else {
            $self->AUTOLOAD(\'1', @{$wait{$retkey}}[3..$#{$wait{$retkey}}])
        }
    }
    else {
        # Direct call.
        return (defined $retval ? $retval : '') if defined wantarray;
    }
}

# Chops the first one or two lines from a procedure to determine
# if it could be used as a hook, among other things.
sub _chophook {
    my $self = shift;
    my ($procref, $varref, $paramref) = @_;
    my ($state, $wait);
    my $op = shift(@{$procref});

    if ($op eq 'call') {
        $state = shift(@{$procref});
        $state =~ s/\$\[(.+?)\]/$varref->{$1} ||
                               ($varref->{$1} = shift(@{$paramref}))/eg;

        # Chophook won't cut the wait op under scalar context.
        return $state if (defined wantarray xor wantarray);

        $op    = shift(@{$procref});
    }

    if ($op eq 'wait') {
        $wait = shift(@{$procref});
        $wait =~ s/\$\[(.+?)\]/$varref->{$1} ||
                              ($varref->{$1} = shift(@{$paramref}))/eg;

        # Don't bother any more under void context.
        return unless wantarray;

        $wait =~ s/\x5c\x5c/_!!!_/g;
        $wait =~ s/\\n/\015\012/g;
        $wait =~ s/\\e/\e/g;
        $wait =~ s/\\x([0-9a-fA-F][0-9a-fA-F])/chr(hex($1))/eg;
        $wait =~ s/_!!!_/\x5c/g;
    }
    else {
        die "Chophook: Procedure does not start with 'wait'";
    }

    return ($state, $wait);
}

# Implementation of named procedures.
sub AUTOLOAD {
    my $self   = shift;
    my $flag   = ${shift()} if ref($_[0]);
    my $params = join(',', @_) if @_;
    my $sub    = $AUTOLOAD;

    local $^W = 0; # no warnings here

    $sub =~ s/^.*:://;

    if (exists $self->{proc}{$sub}) {
        my @proc = @{$self->{proc}{$sub}};
        my @cond = 1;
        my (@result, %var);

        print "Entering $sub ($params)\n" if $self->{debug};

        $self->_chophook(\@proc, \%var, \@_) if $flag;

        while (my $op = shift(@proc)) {
            my $param = shift(@proc);

            if ($op eq 'endo') {
                pop @cond; next;
            } 
            elsif ($op eq 'else') {
                $cond[-1] = !($cond[-1]); next;
            }
            else {
                next unless ($cond[-1]);
            }

            $param =~ s/\x5c\x5c/_!!!_/g;
            $param =~ s/\\n/\015\012/g;
            $param =~ s/\\e/\e/g;
            $param =~ s/\\x([0-9a-fA-F][0-9a-fA-F])/chr(hex($1))/eg;
            $param =~ s/_!!!_/\x5c/g;

            $param =~ s{\$\[([\-\d]+)\]}{
                $self->{lastmatch}[$1]
            }eg unless $op eq 'call';

            $param =~ s{\$\[([^\]]+)\]}{
                $var{$1} || ($var{$1} = (exists $self->{var}{$1}
                    ? $self->{var}{$1} : shift))
            }eg unless $op eq 'call';

            print "*** $op ", _plain($param), "\n" if $self->{debug};

            if ($op eq 'doif') {
                push(@cond, $param);
            }
            elsif ($op eq 'call') {
                # for kkcity
                $param =~ s{\$\[([^\]]+)\]}{
                    $var{$1} || ($var{$1} = (exists $self->{var}{$1}
                        ? $self->{var}{$1} : shift))
                }eg;

                my @params = split(',', $param);
                ($param, $params[0]) = split(/\s/, $params[0], 2);

                s{\$\[(.+?)\]}{
                    $var{$1} || ($var{$1} = (exists $self->{var}{$1}
                        ? $self->{var}{$1} : shift))
                }eg foreach @params;

                $self->$param(@params)
                    unless $self->{state} eq "$param ".join(',',@params);

                print "Return from $param (",join(',',@params),")\n"
                    if $self->{debug};
            }
            elsif ($op eq 'goto') {
                $self->$param() unless $self->{state} eq $param;
                return wantarray ? @result : $result[0];
            }
            elsif ($op eq 'wait') {
                defined $self->Expect($param) or return;
            }
            elsif ($op eq 'till') {
                my $lastidx = $#result;
                push @result, $self->Expect($param);
                return if $lastidx == $#result;
            }
            elsif ($op eq 'send') {
                $self->{netobj}->send($param);
                return if $self->{errmsg};
            }
            elsif ($op eq 'exit') {
                $result[0] = '' unless defined $result[0];
                return wantarray ? @result : $result[0];
            }
            elsif ($op eq 'setv') {
                my ($var, $val) = split(/\s/, $param, 2);
                $self->{var}{$var} = $val;
            }
            elsif ($op eq 'idle') {
                sleep $param;
            }
            else {
                die "No such operator: $op";
            }
        }

        $self->{state} = "$sub $params";

        print "Set State: $self->{state}\n" if $self->{debug};
        return wantarray ? @result : $result[0];
    }
    else {
        die "Undefined procedure '$sub' called";
    }
}

1;

__END__

=head1 AUTHORS

Autrijus Tang E<lt>autrijus@autrijus.org>

=head1 COPYRIGHT

Copyright 2001 by Autrijus Tang E<lt>autrijus@autrijus.org>.

All rights reserved.  You can redistribute and/or modify
this module under the same terms as Perl itself.

=cut
