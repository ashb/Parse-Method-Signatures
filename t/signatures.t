use strict;
use warnings;

use Test::More;
use Test::Exception;
use Parse::Method::Signatures;

my @sigs = (
    ['(Str $name)'],
    ['(Str :$who, Int :$age where { $_ > 0 })'],
    ['(Str $name, Bool :$excited = 0)'],
    ['(Animal|Human $affe)'],
    ['(:$a, :$b, :$c)'],
    ['( $a,  $b, :$c)'],
    ['($a , $b!, :$c!, :$d!)'],
    ['($a?, $b?, :$c , :$d?)'],
    ['($self:  $moo)'],
    ['(:     $affe ) # called as $obj->foo(affe => $value)'],
    ['(:apan($affe)) # called as $obj->foo(apan => $value)'],
    [q#(SomeClass $thing where { $_->can('stuff') }:
Str  $bar  = "apan"
Int :$baz! = 42 where { $_ % 2 == 0 } where { $_ > 10 })#],
);

plan tests => scalar @sigs;

for my $row (@sigs) {
    my ($sig, $msg) = @{ $row };
    lives_ok {
        Parse::Method::Signatures->signature($sig);
    } ($msg || 'parsed');
}
