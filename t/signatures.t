use strict;
use warnings;

use Test::More tests => 13;
use Test::Exception;

use_ok('SlimSignature') or BAIL_OUT('Cannot continue');



lives_ok { SlimSignature->signature('(Str $name)'); }
         'parsed';

lives_ok { SlimSignature->signature('(Str :$who, Int :$age where { $_ > 0 })'); }
         'parsed';

lives_ok { SlimSignature->signature('(Str $name, Bool :$excited = 0)'); }
         'parsed';

lives_ok { SlimSignature->signature('(Animal|Human $affe)'); }
         'parsed';

lives_ok { SlimSignature->signature('(:$a, :$b, :$c)'); }
         'parsed';

lives_ok { SlimSignature->signature('( $a,  $b, :$c)'); }
         'parsed';

lives_ok { SlimSignature->signature('($a , $b!, :$c!, :$d!)'); }
         'parsed';

lives_ok { SlimSignature->signature('($a?, $b?, :$c , :$d?)'); }
         'parsed';

lives_ok { SlimSignature->signature('($self:  $moo)'); }
         'parsed';

lives_ok { SlimSignature->signature('(:     $affe ) # called as $obj->foo(affe => $value)'); }
         'parsed';

{
local $TODO = 'What the hell is this';
lives_ok { SlimSignature->signature('(:apan($affe)) # called as $obj->foo(apan => $value)'); }
         'parsed';
}
lives_ok { SlimSignature->signature(q#(SomeClass $thing where { $_->can('stuff') }:
Str  $bar  = "apan"
Int :$baz! = 42 where { $_ % 2 == 0 } where { $_ > 10 })#)}
         'parsed';


