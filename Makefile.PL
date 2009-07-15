use inc::Module::Install 0.91;

use strict;
use warnings;

name 'Parse-Method-Signatures';
all_from 'lib/Parse/Method/Signatures.pm';

requires 'List::MoreUtils';
requires 'Moose';
requires 'MooseX::Traits' => '0.06';
requires 'MooseX::Types' => '0.10';
requires 'MooseX::Types::Moose';
requires 'MooseX::Types::Structured';
requires 'namespace::clean' => '0.10';
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

auto_provides;
auto_install();
WriteAll;
