package SlimSignature;

use Moose;
use Text::Balanced qw(extract_codeblock);

has 'tokens' => (
  is => 'ro',
  isa => 'ArrayRef',
  init_arg => undef,
  default => sub { [] },
);

has 'input' => (
  is => 'ro',
  isa => 'Str',
  required => 1
);

has '_input' => (
  is => 'ro',
  isa => 'ScalarRef',
  init_arg => undef,
  lazy_build => 1
);

sub _build__input {
    my $var = $_[0]->input;
    return \$var;
}

sub parse_signature {
  my ($self, $data) = @_;

  $self = $self->new(input => $data);

  $self->token();
  
}

sub parse_param {
  my ($self, $data) = @_;
  $self = $self->new unless blessed($self);
  $data = \$data unless ref $data;
}

sub assert_token {
  my ($self, $type) = @_;

  if ($self->token->{type} eq $type) {
    $self->consume_token;
    return 1;
  }
  
  die "$type required!";
}

sub token {
  my ($self) = @_;

  return $self->tokens->[0]
    if @{$self->tokens};

  my $token = $self->next_token($self->_input);
  push @{$self->tokens}, $token;
  return $token;
}

sub consume_token {
  my ($self) = @_;

  die "No token to consume"
    unless @{$self->tokens};
    
  return shift @{$self->token};
}

sub next_token {
  my ($self, $data) = @_;

  my $re = qr/ \s* (?:
    ([(){},:=|]) |
    (
      [A-Za-z][a-zA-Z0-0_-]+
      (?:::[A-Za-z][a-zA-Z0-0_-]+)
    ) |
    (where) |
    (\$[_A-Za-z][a-zA-Z0-9_])
  ) \s* /sx;

  # symbols in $1
  # class-name ish in $2
  # 'where' in $3
  # $var in $4

  unless ( $$data =~ s/$re//) {
    die "Error paring signature at '" . substr($$data, 0, 10);
  }

  my ($sym, $cls,$where,$var) = ($1,$2,$3,$4);

  if ($sym) {
    return { type => $sym, literal => $sym };
  }

  die "Shouldn't get here!";
}

1;

