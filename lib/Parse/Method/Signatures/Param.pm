package Parse::Method::Signatures::Param;

use Moose;
use MooseX::Types::Moose qw/Bool Str ArrayRef/;

use namespace::clean -except => 'meta';

with 'MooseX::Traits';

has required => (
    is       => 'ro',
    isa      => Bool,
    required => 1
);

has sigil => (
    is       => 'ro',
    isa      => 'Str',
    required => 1,
);

has type_constraints => (
    is         => 'ro',
    isa        => ArrayRef [Str],
    predicate  => 'has_type_constraints',
    auto_deref => 1,
);

has default_value => (
    is        => 'ro',
    isa       => Str,
    predicate => 'has_default_value',
);

has constraints => (
    is         => 'ro',
    isa        => ArrayRef [Str],
    predicate  => 'has_constraints',
    auto_deref => 1,
);

has '+_trait_namespace' => (
    default => 'Parse::Method::Signatures::Param',
);

sub _stringify_type_constraints {
    my ($self) = @_;
    return $self->has_type_constraints
        ? join(q{|}, $self->type_constraints) . q{ }
        : q{};
}

sub _stringify_default_value {
    my ($self) = @_;
    return $self->has_default_value
        ? q{ = } . $self->default_value
        : q{};
}

sub _stringify_constraints {
    my ($self) = @_;
    return q{} unless $self->has_constraints;
    return q{ where } . join(q{ where }, $self->constraints);
}

sub to_string {
    my ($self) = @_;
    my $ret = q{};

    $ret .= $self->_stringify_type_constraints;
    $ret .= $self->_stringify_variable_name;
    $ret .= $self->_stringify_required;
    $ret .= $self->_stringify_default_value;
    $ret .= $self->_stringify_constraints;

    return $ret;
}

__PACKAGE__->meta->make_immutable;

1;
