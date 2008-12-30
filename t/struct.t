use strict;
use warnings;

use Test::More skip_all => 'port to signature objects'; #tests => 17;
use Test::Differences;

use_ok('Parse::Method::Signatures') or BAIL_OUT('Cannot continue');

eq_or_diff(
  scalar Parse::Method::Signatures->signature('(Str $name)'),
  { params => [
      { tc => 'Str',
        var => '$name',
      }
    ]
  },
);

eq_or_diff(
  scalar Parse::Method::Signatures->signature('(Str :$who, Int :$age where { $_ > 0 })'),
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
  },
);

eq_or_diff( 
  scalar Parse::Method::Signatures->signature('(Str $name, Bool :$excited = 0)'),
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
  },
);

eq_or_diff(
  scalar Parse::Method::Signatures->signature('(Animal|Human $affe)'),
  { params => [
      { tc => 'Animal|Human',
        var => '$affe'
      },
    ]
  },
);

eq_or_diff(
  scalar Parse::Method::Signatures->signature('(:$a, :$b, :$c)'),
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
  },
);

eq_or_diff( 
  scalar Parse::Method::Signatures->signature('( $a,  $b, :$c)'),
  { params => [
      { var => '$a' },
      { var => '$b' },
      { var => '$c',
        named => 1
      },
    ]
  },
);

eq_or_diff( 
  scalar Parse::Method::Signatures->signature('($a , $b!, :$c!, :$d!)'),
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
  },
);

eq_or_diff( 
  scalar Parse::Method::Signatures->signature('($a?, $b?, :$c , :$d?)'),
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
  },
);

eq_or_diff(
  scalar Parse::Method::Signatures->signature('($self:  $moo)'),
  { params => [
      { var => '$moo' }
    ],
    invocant => {
      var => '$self'
    }
  },
);

# TODO: Should this have a empty invocant struct?
eq_or_diff(
  scalar Parse::Method::Signatures->signature('(:     $affe ) # called as $obj->foo(affe => $value)'),
  { params => [
      { var => '$affe',
        named => 1
      }
    ]
  }, 
);

eq_or_diff(
  scalar Parse::Method::Signatures->signature('(:apan($affe)) # called as $obj->foo(apan => $value)'),
  { params => [
      { label => 'apan',
        var => '$affe',
        named => 1
      }
    ]
  },
);

eq_or_diff(
  scalar Parse::Method::Signatures->signature(q#(SomeClass $thing where { $_->can('stuff') }:
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
  },
);


eq_or_diff(
  [ Parse::Method::Signatures->signature('(Str $name)') ],
  [ { params => [
      { tc => 'Str',
        var => '$name',
      }
    ]
  }, ''],
);

eq_or_diff(
  [ Parse::Method::Signatures->signature('(Str $name) further data }') ],
  [ { params => [
      { tc => 'Str',
        var => '$name',
      }
    ]
  }, 'further data }'],
);


eq_or_diff(
  [ Parse::Method::Signatures->param(
      input => 'previous data(Str $name) further data }',
      offset => 14) ],
  [ { tc => 'Str',
      var => '$name',
    },
    ') further data }'],
);

eq_or_diff(
  [ Parse::Method::Signatures->signature( "(\$param1 # Foo bar\n \$param2) postfix") ],
  [ { params => [
      { var => '$param1' },
      { var => '$param2' },
    ] },
    'postfix'
  ]
);
