package Parse::Method::Signatures::Param;

use Moose;
use MooseX::Types::Structured qw/Tuple/;
use MooseX::Types::Moose qw/Bool Str ArrayRef HashRef/;

use namespace::clean -except => 'meta';

with 'MooseX::Traits';

has required => (
    is       => 'ro',
    isa      => Bool,
    required => 1
);

has sigil => (
    is       => 'ro',
    isa      => Str,
    required => 1,
);

has type_constraints => (
    is         => 'ro',
    isa        => 'Parse::Method::Signatures::TypeConstraint',
    predicate  => 'has_type_constraints',
    handles    => {
        meta_type_constraint => 'tc'
    },
);

has default_value => (
    is        => 'ro',
    isa       => Str,
    predicate => 'has_default_value',
);

has constraints => (
    is         => 'ro',
    isa        => ArrayRef[Str],
    predicate  => 'has_constraints',
    auto_deref => 1,
);

has param_traits => (
    is         => 'ro',
    isa        => ArrayRef[Tuple[Str, Str]],
    predicate  => 'has_traits',
    auto_deref => 1
);

has '+_trait_namespace' => (
    default => 'Parse::Method::Signatures::Param',
);

sub _stringify_type_constraints {
    my ($self) = @_;
    return $self->has_type_constraints
        ? $self->type_constraints->str . q{ }
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

sub _stringify_traits {
    my ($self) = @_;
    return q{} unless $self->has_traits;
    return q{ } . join q{ }, map { @{ $_ } } $self->param_traits;
}

sub to_string {
    my ($self) = @_;
    my $ret = q{};

    $ret .= $self->_stringify_type_constraints;
    $ret .= $self->_stringify_variable_name;
    $ret .= $self->_stringify_required;
    $ret .= $self->_stringify_default_value;
    $ret .= $self->_stringify_constraints;
    $ret .= $self->_stringify_traits;

    return $ret;
}

__PACKAGE__->meta->make_immutable;

1;
