package Parse::Method::Signatures::TypeConstraint;

use Moose;

use namespace::clean -except => 'meta';

has str => (
  is       => 'ro',
  isa      => 'Str',
  required => 1
);

# I'm very lazy - lets just get this working/storing things for now
has data => (
  is       => 'ro',
  isa      => 'Str|HashRef',
  required => 1,
);

1;
