use strict;
use warnings;

use Test::More 'no_plan';
use Test::Exception;
use Test::Differences;

use_ok("Parse::Method::Signatures") or BAIL_OUT("$@");

is( Parse::Method::Signatures->new("ArrayRef")->_ident(), "ArrayRef");
is( Parse::Method::Signatures->new("where::Foo")->_ident(), "where::Foo");

{ local $TODO = "sort out lextable";
is( Parse::Method::Signatures->new("where Foo")->_ident(), undef);
}

throws_ok {
  Parse::Method::Signatures->new("Foo[Bar")->tc()
} qr/^\QRunaway '[]' in type constraint near '[Bar' at\E/;

throws_ok {
  Parse::Method::Signatures->new("Foo[Bar:]")->tc()
} qr/^\QError parsing type constraint near 'Bar:' in 'Bar:' at\E/;

is( Parse::Method::Signatures->new("ArrayRef")->tc(), "ArrayRef");
is( Parse::Method::Signatures->new("ArrayRef[Str => Str]")->tc(), "ArrayRef[Str => Str]");
is( Parse::Method::Signatures->new("ArrayRef[Str]")->tc(), "ArrayRef[Str]");
is( Parse::Method::Signatures->new("ArrayRef[0 => Foo]")->tc(), "ArrayRef[0 => Foo]");
is( Parse::Method::Signatures->new("ArrayRef[qq/0/]")->tc(), "ArrayRef[qq/0/]");

lives_ok { Parse::Method::Signatures->new('$x')->param() };

throws_ok {
  Parse::Method::Signatures->new('$x[0]')->param()
  } qr/Error parsing parameter near '\$x' in '\$x\[0\]' at /;
