package Parse::Method::Signatures::Sig::Unpacked::Hash;

use Moose;
use MooseX::Types::Moose qw/Str Maybe/;

use namespace::clean -except => 'meta';

extends 'Parse::Method::Signatures::Sig';
with 'Parse::Method::Signatures::Param';

has '+required_positional_params' => ( default => 0 );
has '+positional_params' => ( default => sub { [] } );

has label => (
    is        => 'ro',
    isa       => Maybe[Str],
    predicate => 'has_label'
);

override BUILDARGS => sub {
  my $args = super();

  return $args if $args->{required_named_params};

  $args->{required_named_params} = [];
  foreach (@{$args->{named_params}}) {
    push @{$args->{required_named_params}}, $_->label
      if $_->meta->has_attribute('label') && $_->required;
  }

  return $args;
};

sub to_string {
  my ($self) = @_;

  my $str = '{' . join(', ', map { $_->to_string } @{$self->named_params}) . '}';

  return $self->has_label ? ':'.$self->label."($str)" : $str;
                        
};

1;
