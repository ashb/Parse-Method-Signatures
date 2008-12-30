use strict;
use warnings;

package Parse::Method::Signatures::Types;

use Moose::Util::TypeConstraints;
use MooseX::Types::Moose qw/Str/;
use namespace::clean;

use MooseX::Types -declare => [qw/
    VariableName
    TypeConstraint
/];

subtype VariableName,
    as Str,
    where { /^[\$@%][a-z_][a-z_\d]*$/i },
    message { 'not a valid variable name' };

subtype TypeConstraint,
    as 'Moose::Meta::TypeConstraint';

coerce TypeConstraint,
    from Str,
    via { find_or_parse_type_constraint($_) };

1;
