package Parse::Method::Signatures::Sig::Unpacked::Array;

use Moose;
use MooseX::Types::Moose qw/Str Maybe/;

use namespace::clean -except => 'meta';

extends 'Parse::Method::Signatures::Sig';
with 'Parse::Method::Signatures::Param';

has '+required_named_params' => ( default => sub { [] } );
has '+named_params' => ( default => sub { [] } );

has label => (
    is        => 'ro',
    isa       => Maybe[Str],
    predicate => 'has_label'
);

override BUILDARGS => sub {
  my $args = super();

  $args->{required_positional_params} = scalar @{$args->{positional_params}}
    unless exists ($args->{required_positional_params});

  return $args;
};

sub to_string {
  my ($self) = @_;

  my $str = '[' . join(', ', map { $_->to_string } @{$self->positional_params}) . ']';

  return $self->has_label ? ':'.$self->label."($str)" : $str;
                        
};

1;
