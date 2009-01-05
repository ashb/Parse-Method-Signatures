use inc::Module::Install 0.77;

use strict;
use warnings;

name 'Parse-Method-Signatures';
all_from 'lib/Parse/Method/Signatures.pm';

requires 'List::MoreUtils';
requires 'Moose';
requires 'MooseX::Traits';
requires 'MooseX::Types';
requires 'MooseX::Types::Moose';
requires 'namespace::clean';
requires 'Text::Balanced' => 2.000000;

test_requires 'Test::More';
test_requires 'Test::Differences';
test_requires 'Test::Exception';

postamble(<<"EOM");
testcover: pm_to_blib
\tcover -delete
\tHARNESS_PERL_SWITCHES=-MDevel::Cover \$(MAKE) test
\tcover
EOM

Meta->resources('repository' => 'http://github.com/ashb/parse-method-signatures/tree/master');

auto_install;
WriteAll;
