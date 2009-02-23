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
    return $self->_walk_data($self->data);
}

sub _walk_data {
    my ($self, $data) = @_;

    my $res = $self->_leaf($data)
           || $self->_union_node($data)
           || $self->_params_node($data)
           || $self->_str_node($data)
      or confess 'failed to visit tc';
    return $res->();
}

sub _leaf {
    my ($self, $data) = @_;
    return if ref($data);

    sub { $self->_invoke_callback($data) };
}

sub _union_node {
    my ($self, $data) = @_;
    return unless exists $data->{-or};

    my @types = map { $self->_walk_data($_) } @{ $data->{-or} };
    sub {
      scalar @types == 1 ? @types
        : Moose::Meta::TypeConstraint::Union->new(type_constraints => \@types)
    };
}

sub _params_node {
    my ($self, $data) = @_;
    return unless exists $data->{-type};

    my @params = map { $self->_walk_data($_) } @{ $data->{-params} };
    my $type = $self->_invoke_callback($data->{-type});
    sub { $type->parameterize(@params) }
}


sub _str_node {
    my ($self, $data) = @_;
    return unless exists $data->{-str};

    sub { $data->{-str} };
}

sub _invoke_callback {
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

__END__

=head1 NAME

Parse::Method::Signatures::TypeConstraint - turn parse TC data into Moose TC object

=head1 ATTRIBUTES

=head2 str

String representation of the type constraint

=head2 data

A simple hash based representation of the type constraint. Exact details of
this structure yet to be documented. Read the code if you are interested.

=head2 tc

B<Lazy Build.>

The L<Moose::Meta::TypeConstraint> object for this type constraint, build when
requested. L</tc_callback> will be called for each individual component type in
turn.

=head2 tc_callback

B<Type:> CodeRef

Callback used to turn type names into type objects. See
L<Parse::Method::Signatures/type_constraint_callback> for more details and an
example.

=head1 METHODS

=head2 find_registered_constraint

Simply asks the L<Moose::Meta::TypeConstraint::Registry> for a type with the
given name.

=head1 AUTHORS

Florian Ragwitz <rafl@debian.org>.

Ash Berlin <ash@cpan.org>.

=head1 LICENSE

Licensed under the same terms as Perl itself.

