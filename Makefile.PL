use inc::Module::Install 0.77;

use strict;
use warnings;

name 'Parse-Method-Signatures';
all_from 'lib/Parse/Method/Signatures.pm';

requires 'aliased';
requires 'List::MoreUtils';
requires 'Moose';
requires 'MooseX::Types';
requires 'MooseX::Types::Moose';
requires 'namespace::clean';
requires 'Text::Balanced' => 2.000000;
requires 'MooseX::StrictConstructor'; # TODO: Remove me

test_requires 'Test::More';
test_requires 'Test::Differences';
test_requires 'Test::Exception';

auto_install;
WriteAll;
