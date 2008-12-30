package Parse::Method::Signatures::Param;

use Moose;
use MooseX::StrictConstructor; # FIXME: remove later
use MooseX::Types::Moose qw/ArrayRef Str/;
use Parse::Method::Signatures::Types qw/VariableName TypeConstraint/;
use namespace::clean -except => 'meta';

has variable_name => (
    is  => 'ro',
    isa => VariableName,
);

has type_constraint => (
    is        => 'ro',
    isa       => TypeConstraint,
    predicate => 'has_type_constraint',
);

has constraints => (
    is        => 'ro',
    isa       => ArrayRef[Str],
    predicate => 'has_constraints',
);

# TODO: add TC once default_value is unified
has default_value => (
    is        => 'ro',
    predicate => 'has_default_value',
);

__PACKAGE__->meta->make_immutable;

1;
