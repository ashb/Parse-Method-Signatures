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
    ['(Str :$who, Int :$age where { $_ > 0 })',
                                'complex with constraint'],
    ['(Str $name, Bool :$excited = 0)',
                                'complex with default'],
    [q#(SomeClass $thing where { $_->can('stuff') }:
Str  $bar  = "apan"
Int :$baz! = 42 where { $_ % 2 == 0 } where { $_ > 10 })#,
                                'complex invocant, defaults and constraints'],
);

plan tests => scalar @sigs;

for my $row (@sigs) {
    my ($sig, $msg, $todo) = @{ $row };
    TODO: {
        todo_skip $todo, 1 if $todo;

        lives_ok {
            Parse::Method::Signatures->signature($sig);
        } $msg;
    }
}
