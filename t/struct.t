use strict;
use warnings;

use Test::More tests => 13;
use Test::Differences;

use_ok('SlimSignature') or BAIL_OUT('Cannot continue');

eq_or_diff(
  SlimSignature->signature('(Str $name)'),
  { params => [
      { tc => 'Str',
        var => '$name',
      }
    ]
  }
);

eq_or_diff(
  SlimSignature->signature('(Str :$who, Int :$age where { $_ > 0 })'),
  { params => [
      { tc => 'Str',
        named => 1,
        var => '$who'
      },
      { tc => 'Int',
        named => 1,
        var => '$age',
        where => [
          '{ $_ > 0 }'
        ]
      },
    ]
  }
);

eq_or_diff( 
  SlimSignature->signature('(Str $name, Bool :$excited = 0)'),
  { params => [
      { tc => 'Str',
        var => '$name',
      },
      { tc => 'Bool',
        var => '$excited',
        named => 1,
        default => '0'
      },
    ]
  }
);

eq_or_diff(
  SlimSignature->signature('(Animal|Human $affe)'),
  { params => [
      { tc => 'Animal|Human',
        var => '$affe'
      },
    ]
  }
);

eq_or_diff(
  SlimSignature->signature('(:$a, :$b, :$c)'),
  { params => [
      { var => '$a',
        named => 1
      },
      { var => '$b',
        named => 1
      },
      { var => '$c',
        named => 1
      },
    ]
  }
);

eq_or_diff( 
  SlimSignature->signature('( $a,  $b, :$c)'),
  { params => [
      { var => '$a' },
      { var => '$b' },
      { var => '$c',
        named => 1
      },
    ]
  }
);

eq_or_diff( 
  SlimSignature->signature('($a , $b!, :$c!, :$d!)'),
  { params => [
      { var => '$a' },
      { var => '$b',
        required => 1
      },
      { var => '$c',
        named => 1,
        required => 1
      },
      { var => '$d',
        named => 1,
        required => 1
      },
    ]
  }
);

eq_or_diff( 
  SlimSignature->signature('($a?, $b?, :$c , :$d?)'),
  { params => [
      { var => '$a',
        optional => 1
      },
      { var => '$b',
        optional => 1
      },
      { var => '$c',
        named => 1,
      },
      { var => '$d',
        named => 1,
        optional => 1
      },
    ]
  }
);

eq_or_diff(
  SlimSignature->signature('($self:  $moo)'),
  { params => [
      { var => '$moo' }
    ],
    invocant => {
      var => '$self'
    }
  }
);

# TODO: Should this have a empty invocant struct?
eq_or_diff(
  SlimSignature->signature('(:     $affe ) # called as $obj->foo(affe => $value)'),
  { params => [
      { var => '$affe',
        named => 1
      }
    ]
  }
);

SKIP: {
  eval {
  local $TODO = 'What the hell is this';
  eq_or_diff(
    SlimSignature->signature('(:apan($affe)) # called as $obj->foo(apan => $value)'),
    {
    }
  );
  };
  skip('this doesn\'t parse yet', 1) if ($@);
  
}

eq_or_diff( SlimSignature->signature(q#(SomeClass $thing where { $_->can('stuff') }:
Str  $bar  = "apan"
Int :$baz! = 42 where { $_ % 2 == 0 } where { $_ > 10 })#),
  { params => [
      { tc => 'Str',
        var => '$bar',
        default => '"apan"'
      },
      { tc => 'Int',
        var => '$baz',
        named => 1,
        required => 1,
        where => [
          '{ $_ % 2 == 0 }',
          '{ $_ > 10 }'
        ],
        default => '42'
      }
    ],
    invocant => {
      tc => 'SomeClass',
      var => '$thing',
      where => [
        '{ $_->can(\'stuff\') }'
      ]
    }
  }
);


