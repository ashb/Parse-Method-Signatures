package Parse::Method::Signatures::TypeConstraint;

use Moose;
use MooseX::Types::Moose qw/Str HashRef CodeRef/;
use Parse::Method::Signatures::Types qw/TypeConstraint/;

use namespace::clean -except => 'meta';

has str => (
  is       => 'ro',
  isa      => Str,
  required => 1
);

# I'm very lazy - lets just get this working/storing things for now
has data => (
  is       => 'ro',
  isa      => Str|HashRef,
  required => 1,
);

has tc => (
    is => 'ro',
    isa => TypeConstraint,
    lazy => 1,
    builder => '_build_tc',
);

has tc_callback => (
    is       => 'ro',
    isa      => CodeRef,
    default  => sub { \&find_registered_constraint },
);

sub _build_tc {
    my ($self) = @_;
    return $self->visit($self->data);
}

sub visit {
    my ($self, $data) = @_;
    unless (ref $data) {
        return $self->invoke_callback($data);
    }
    elsif (exists $data->{-or}) {
        my @types = map { $self->visit($_) } @{ $data->{-or} };
        return @types if scalar @types == 1;
        return Moose::Meta::TypeConstraint::Union->new(type_constraints => \@types);
    }
    elsif (exists $data->{-type}) {
        my @params = map { $self->visit($_) } @{ $data->{-params} };
        my $type = $self->invoke_callback($data->{-type});
        return $type->parameterize(@params);
    }
    elsif (exists $data->{-str}) {
        return $data->{-str};
    }
    else {
        confess 'failed to visit tc';
    }
}

sub invoke_callback {
    my $self = shift;
    $self->tc_callback->($self, @_);
}

sub find_registered_constraint {
    my ($self, $name) = @_;
    my $registry = Moose::Util::TypeConstraints->get_type_constraint_registry;
    return $registry->find_type_constraint($name) || $name;
}

__PACKAGE__->meta->make_immutable;

1;
