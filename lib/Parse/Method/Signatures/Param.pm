package Parse::Method::Signatures::Param;

use Moose;
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

has default_value => (
    is        => 'ro',
    isa       => Str,
    predicate => 'has_default_value',
);

has required => (
    is       => 'ro',
    isa      => 'Bool',
    required => 1
);

sub _stringify_type_constraint {
    my ($self) = @_;
    return q{} unless $self->has_type_constraint;
    return $self->type_constraint . q{ };
}

sub _stringify_variable_name {
    my ($self) = @_;
    my $ret = $self->variable_name;
    $ret .= '?' unless $self->required;
    return $ret;
}

sub _stringify_default_value {
    my ($self) = @_;
    return q{} unless $self->has_default_value;
    return q{ = } . $self->default_value;
}

sub _stringify_constraints {
    my ($self) = @_;
    return q{} unless $self->has_constraints;
    return q{ where } . join(q{ where }, @{ $self->constraints });
}

sub to_string {
    my ($self) = @_;
    my $ret = q{};

    $ret .= $self->_stringify_type_constraint;
    $ret .= $self->_stringify_variable_name();
    $ret .= $self->_stringify_default_value;
    $ret .= $self->_stringify_constraints;

    return $ret;
}

__PACKAGE__->meta->make_immutable;

1;
