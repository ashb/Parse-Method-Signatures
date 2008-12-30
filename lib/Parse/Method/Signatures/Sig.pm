package Parse::Method::Signatures::Sig;

use Moose;
use MooseX::Types::Moose qw/ArrayRef HashRef/;
use aliased 'Parse::Method::Signatures::Param';
use aliased 'Parse::Method::Signatures::Param::Named';
use List::MoreUtils qw/part/;

use namespace::clean -except => 'meta';

has invocant => (
    is => 'ro',
    isa => Param,
);

has positional_params => (
    is        => 'ro',
    isa       => ArrayRef[Param],
    predicate => 'has_positional_params',
);

has named_params => (
    is        => 'ro',
    isa       => ArrayRef[Param],
    predicate => 'has_named_params',
);

has _named_map => (
    is         => 'ro',
    isa        => HashRef[Param],
    lazy_build => 1,
);

override BUILDARGS => sub {
    my $args = super();

    if (my $params = delete $args->{params}) {
        my ($positional, $named) = part { $_->isa(Named) } @{ $params };
        $_ ||= [] for $positional, $named;
        @{ $args }{qw/positional_params named_params/} = ($positional, $named);
    }

    return $args;
};

sub _build__named_map {
    my ($self) = @_;
    return {} unless $self->has_named_params;
    return { map { $_->label => $_ } @{ $self->named_params } };
}

sub named_param {
    my ($self, $name) = @_;
    return $self->_named_map->{$name};
}

__PACKAGE__->meta->make_immutable;

1;
