package Parse::Method::Signatures::TypeConstraint;

use Moose;
use MooseX::Types::Moose qw/Str HashRef/;

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

__PACKAGE__->meta->make_immutable;

1;
