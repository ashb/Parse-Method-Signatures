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

is( Parse::Method::Signatures->new("ArrayRef")->tc(), "ArrayRef");
is( Parse::Method::Signatures->new("ArrayRef[Str => Str]")->tc(), "ArrayRef[Str => Str]");
is( Parse::Method::Signatures->new("ArrayRef[Str]")->tc(), "ArrayRef[Str]");
is( Parse::Method::Signatures->new("ArrayRef[0 => Foo]")->tc(), "ArrayRef[0 => Foo]");
is( Parse::Method::Signatures->new("ArrayRef[qq/0/]")->tc(), "ArrayRef[qq/0/]");
