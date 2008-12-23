use inc::Module::Install 0.77;

use strict;
use warnings;

name 'SlimSignature';
all_from 'lib/SlimSignature.pm';

requires 'Text::Balanced' => '2.0.0';

test_requires 'Test::More';
test_requires 'Test::Exception';

auto_install;
WriteAll;
