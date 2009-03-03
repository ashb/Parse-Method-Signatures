use inc::Module::Install 0.79;

use strict;
use warnings;

name 'Parse-Method-Signatures';
all_from 'lib/Parse/Method/Signatures.pm';

requires 'List::MoreUtils';
requires 'Moose';
requires 'MooseX::Traits';
requires 'MooseX::Types';
requires 'MooseX::Types::Moose';
requires 'MooseX::Types::Structured';
requires 'namespace::clean';
requires 'PPI' => '1.203';

test_requires 'aliased';
test_requires 'Test::More';
test_requires 'Test::Exception';
test_requires 'Test::Differences';
test_requires 'MooseX::Types::Structured';

postamble(<<"EOM");
testcover: pm_to_blib
\tcover -delete
\tHARNESS_PERL_SWITCHES=-MDevel::Cover \$(MAKE) test
\tcover
EOM

resources(
    'repository' => 'git://github.com/ashb/parse-method-signatures.git',
    'homepage'   => 'http://github.com/ashb/parse-method-signatures/tree/master',
);

#auto_install;
WriteAll;
