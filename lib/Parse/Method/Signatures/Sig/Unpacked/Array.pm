package Parse::Method::Signatures::Sig::Unpacked::Array;

use Moose;
use MooseX::Types::Moose qw/Str Maybe/;

use namespace::clean -except => 'meta';

extends 'Parse::Method::Signatures::Sig';
with 'Parse::Method::Signatures::Param';

has '+named_params' => ( default => sub { [] } );

has label => (
    is        => 'ro',
    isa       => Maybe[Str],
    predicate => 'has_label'
);

sub to_string {
  my ($self) = @_;

  my $str = '[' . join(', ', map { $_->to_string } @{$self->positional_params}) . ']';

  return $self->has_label ? ':'.$self->label."($str)" : $str;
                        
};

1;
