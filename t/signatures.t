use strict;
use warnings;

use Test::More tests => 13;
use Test::Exception;

use_ok('SlimSignature') or BAIL_OUT('Cannot continue');



lives_ok { SlimSignature->parse_signature('(Str $name)'); }
         'parsed';

lives_ok { SlimSignature->parse_signature('(Str :$who, Int :$age where { $_ > 0 })'); }
         'parsed';

lives_ok { SlimSignature->parse_signature('(Str $name, Bool :$excited = 0)'); }
         'parsed';

lives_ok { SlimSignature->parse_signature('(Animal|Human $affe)'); }
         'parsed';

lives_ok { SlimSignature->parse_signature('(:$a, :$b, :$c)'); }
         'parsed';

lives_ok { SlimSignature->parse_signature('( $a,  $b, :$c)'); }
         'parsed';

lives_ok { SlimSignature->parse_signature('($a , $b!, :$c!, :$d!)'); }
         'parsed';

lives_ok { SlimSignature->parse_signature('($a?, $b?, :$c , :$d?)'); }
         'parsed';

lives_ok { SlimSignature->parse_signature('($self:  $moo)'); }
         'parsed';

lives_ok { SlimSignature->parse_signature('(:     $affe ) # called as $obj->foo(affe => $value)'); }
         'parsed';

lives_ok { SlimSignature->parse_signature('(:apan($affe)) # called as $obj->foo(apan => $value)'); }
         'parsed';

lives_ok { SlimSignature->parse_signature(q#(SomeClass $thing where { $_->can('stuff') }:
Str  $bar  = "apan"
Int :$baz! = 42 where { $_ % 2 == 0 } where { $_ > 10 })#)}
         'parsed';


