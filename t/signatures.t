use strict;
use warnings;

use Test::More;
use Test::Exception;
use Parse::Method::Signatures;

my @sigs = (
    ['()',                      'empty signature'],
    ['($x)',                    'single required positional'],
    ['($x:)',                   'invocant only'],
    ['($x, $y)',                'two required positionals'],
    ['($x where { $_->isa("Moose") })',
                                'with constraint'],
    ['($x where { $_->isa("Moose") } where { $_->does("Gimble") })',
                                'multiple constraints'],
    ['(Str $name)',             'typed positional'],
    ['(Int $x, Str $y)',        'multiple typed positionals'],
    ['(Animal|Human $affe)',    'type constraint alternative'],
    ['(Some::Class $x)',        'type constraint with colon'],
    ['(Tuple[Int,Str] $x)',     'parameterized types', 'TODO'],
    ['(Str|Tuple[Int,Str] $x)', 'parameterized with alternative', 'TODO'],
    ['(:$x)',                   'optional named'],
    ['(:$x!)',                  'required named'],
    ['($x, $y , :$z)',          'positional and named'],
    ['($x, $y?, :$z)',          'optional positional and named'],
    ['(:$a, :$b, :$c)',         'multiple named'],
    ['($a , $b!, :$c!, :$d!)',  'positional and multiple required named'],
    ['($a?, $b?, :$c , :$d?)',  'optional positional and named'],
    ['($self:  $moo)',          'invocant and positional'],
    ['(:apan($affe) )',         'long named'], # called as $obj->foo(apan => $value)
    ['(:apan($affe)!)',         'required long named'],
    ['( $x = 42)',              'positional with default'],
    ['(:$x = 42)',              'named with default'],
    ['( $x = "foo")',           'simple string default'],
    ['( $x = "foo, bar")',      'string default with comma'],
    ["(\$x = 'foo, bar')",      'single quoted default with comma'],
    ['( $x = q"foo")',          'default with q"" quoting'],
    ['( $x = q{foo})',          'default with q{} quoting', 'TODO'],
    ['( $x = q(foo))',          'default with q() quoting', 'TODO'],
    ['( $x = q,foo,)',          'default with q,, quoting', 'TODO'],
    ['($x, $y = $x)',           'default based on other paramter', 'TODO'],
    ['(Str :$who, Int :$age where { $_ > 0 })',
                                'complex with constraint'],
    ['(Str $name, Bool :$excited = 0)',
                                'complex with default'],
    [q#(SomeClass $thing where { $_->can('stuff') }:
Str  $bar  = "apan"
Int :$baz! = 42 where { $_ % 2 == 0 } where { $_ > 10 })#,
                                'complex invocant, defaults and constraints'],
    ['(@x)',                    'positional array', 'TODO'],
    ['($x, @y)',                'positinal scalar and array', 'TODO'],
    ['(%x)',                    'positinal hash', 'TOOD'],
    ['($x, %y)',                'positinal scalar and hash', 'TOOD'],
    ['([$x, $y])',              'simple array ref unpacking', 'TODO'],
    ['([@x])',                  'array ref unpacking into array', 'TODO'],
    ['([$x, $y, @rest])',       'array ref unpacking into scalars and arrays', 'TODO'],
    ['($x, [$y, $z, @rest])',   'array ref unpacking combined with normal positionals', 'TODO'],
    ['([$y, $z, @rest], $x)',   'array ref unpacking combined with normal positionals', 'TODO'],
    ['([$y, $z, @rest], :$x)',  'array ref unpacking combined with named', 'TODO'],
    ['(:foo([$x, $y, @rest]))', 'named array ref unpacking', 'TODO'],
    ['({%x})',                  'hash ref unpacking into hash', 'TODO'],
    ['({:$x, :$y, %rest})',     'hash ref unpacking into scalars and hash', 'TODO'],
    ['($x, {:$y, :$z, %rest})', 'hash ref unpacking combined with normal positionals', 'TODO'],
    ['({:$y, :$z, %rest}, $x)', 'hash ref unpacking combined with normal positionals', 'TODO'],
    ['({:$x, $y, %rest}, :$z)', 'hash ref unpacking combined with named', 'TODO'],
    ['(:foo({:$x, :$y, %r}))',  'named hash ref unpacking', 'TODO'],
);

my @invalid = (
    ['($x?:)',                  'optional invocant', 'TODO'],
    ['($x?, $y)',               'required positional after optional one', 'TODO'],
    ['(Int| $x)',               'invalid type alternation'],
    ['(|Int $x)',               'invalid type alternation'],
    ['(@x, @y)',                'multiple arrays'],
    ['(%x, %y)',                'multiple hashes'],
    ['(:@x)',                   'named array'],
    ['(:%x)',                   'named hash'],
    ['(:[@x])',                 'named array ref unpacking without label'],
    ['([:$x, :$y])',            'unpacking array ref to something not positional'],
    ['(:{%x})',                 'named hash ref unpacking without label'],
    ['({$x, $y})',              'unpacking hash ref to something not named'],
);

plan tests => scalar @sigs + scalar @invalid;

test_sigs(sub {
    my ($sig, $msg) = @_;
    lives_ok { Parse::Method::Signatures->signature($sig) } $msg;
}, @sigs);

test_sigs(sub {
    my ($sig, $msg) = @_;
    dies_ok { Parse::Method::Signatures->signature($sig) } $msg;
}, @invalid);

sub test_sigs {
    my ($test, @sigs) = @_;

    for my $row (@sigs) {
        my ($sig, $msg, $todo) = @{ $row };
        TODO: {
            local $TODO = $todo if $todo;
            $test->($sig, $msg);
        }
    }
}
