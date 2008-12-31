package Parse::Method::Signatures;

use Moose;
use MooseX::Types::Moose qw/ArrayRef HashRef ScalarRef Int Str/;
use Text::Balanced qw(
  extract_codeblock
  extract_variable
  extract_quotelike
);

use Parse::Method::Signatures::Sig::Unpacked::Array;
use Parse::Method::Signatures::Sig::Unpacked::Hash;

use namespace::clean -except => 'meta';

our $VERSION = 1.000000;

has 'tokens' => (
    is       => 'ro',
    isa      => ArrayRef[HashRef],
    init_arg => undef,
    default  => sub { [] },
);

has 'input' => (
    is       => 'ro',
    isa      => Str,
    required => 1
);

has 'offset' => (
    is      => 'rw',
    isa     => Int,
    default => 0,
);

has '_input' => (
    is         => 'ro',
    isa        => ScalarRef,
    init_arg   => undef,
    lazy_build => 1
);

has 'signature_class' => (
    is      => 'ro',
    isa     => 'Str',
    default => 'Parse::Method::Signatures::Sig',
);

has 'param_class_positional' => (
    is      => 'ro',
    isa     => 'Str',
    default => 'Parse::Method::Signatures::Param::Positional',
);

has 'param_class_named' => (
    is      => 'ro',
    isa     => 'Str',
    default => 'Parse::Method::Signatures::Param::Named',
);

has 'param_class_unpacked_array' => (
    is      => 'ro',
    isa     => 'Str',
    default => 'Parse::Method::Signatures::Sig::Unpacked::Array',
);

has 'param_class_unpacked_hash' => (
    is      => 'ro',
    isa     => 'Str',
    default => 'Parse::Method::Signatures::Sig::Unpacked::Hash',
);

sub BUILD {
    my ($self) = @_;

    Class::MOP::load_class($_)
        for map { $self->$_ } qw/
            signature_class
            param_class_positional
            param_class_named
        /;
}

sub _build__input {
    my $var = substr($_[0]->input, $_[0]->offset);
    return \$var;
}

sub create_param {
    my ($self, $args) = @_;

    my $param_class = $args->{positional_params}
        ? $self->param_class_unpacked_array
        : $args->{named_params}
        ? $self->param_class_unpacked_hash
        : delete $args->{named}
        ? $self->param_class_named
        : $self->param_class_positional;
    return $param_class->new($args);
}

# signature: O_PAREN
#            invocant
#            params
#            C_PAREN
#
# invocant: param ':'
#
# params: param COMMA params
#       | param
#       | /* NUL */
sub signature {
  my $self = shift;

  $self = $self->new(@_ == 1 ? (input => $_[0]) : @_);

  $self->assert_token('(');

  my $args = {
    required_positional_params => 0,
    required_named_params => [],
  };
  my $params = [];

  my $param = $self->param;

  if ($param && $self->token->{type} eq ':') {
    # That param was actualy the invocant
    $args->{invocant} = $param;
    die "Invocant cannot be optional"
      if !$param->required;
    die "Invocant cannot be named"
      if $param->isa($self->param_class_named);
    die "Invocant cannot have a default value"
      if $param->has_default_value;

    die "Invocant must be a simple scalar"
      if $param->isa('Parse::Method::Signatures::Sig') ||
         $param->sigil ne '$';

    $self->consume_token;
    $param = $self->param;

  }

  if ($param) {
    push @$params, $param;

    my $opt_pos_param = !$param->required;
    if ($param->required) {
      if ($param->isa($self->param_class_named)) {
        push @{ $args->{required_named_params} }, $param->label;
      } else {
        $args->{required_positional_params}++;
      }
    }

    while ($self->token->{type} eq ',') {
      $self->consume_token;

      $param = $self->param;
      die "parameter expected"
        if !$param;

      if (!$param->isa($self->param_class_named) && 
          $param->required && $opt_pos_param) {
        die "Invalid: Required positional param '"
          . $param->{variable_name} . "' found after optional one.\n";
      }

      push @$params, $param;
      $opt_pos_param = $opt_pos_param || !$param->required;
      if ($param->required) {
        if ($param->isa($self->param_class_named)) {
          push @{ $args->{required_named_params} }, $param->label;
        } else {
          $args->{required_positional_params}++;
        }
      }
    }
  }

  $self->assert_token(')');
  $args->{params} = $params;

  my $sig = $self->signature_class->new($args);

  return wantarray ? ($sig, $self->remaining_input) : $sig;
}


sub unpacked_array {
  my ($self) = @_;

  $self->assert_token('[');
  my $params = [];

  while ($self->token->{type} ne ']') {
    my $param = $self->param
      or $self->assert_token('var'); # not what we are asserting, but should give a useful error message

    die "Cannot have named parameters in an unpacked-array"
      if $param->isa($self->param_class_named);

    die "Cannot have optional parameters in an unpacked-array"
      if !$param->required;

    push @$params, $param;

    last if ($self->token->{type} eq ']');
    $self->assert_token(',');
  }
  $self->assert_token(']');

  return $params;
}

sub unpacked_hash {
  my ($self, $label) = @_;

  $self->assert_token('{');
  my $params = [];

  while ($self->token->{type} ne '}') {
    my $param = $self->param
      or $self->assert_token('var'); # not what we are asserting, but should give a useful error message

    $DB::single = 1;
    die "Cannot have positional parameters in an unpacked-hash: " . $param->to_string
      if !$param->isa($self->param_class_named) &&
          $param->isa($self->param_class_positional) && $param->variable_name =~ /^\$/;

    push @$params, $param;

    last if ($self->token->{type} eq '}');
    $self->assert_token(',');
  }
  $self->assert_token('}');

  return $params;
}

# param: classishTCName?
#        var
#        (OPTIONAL|REQUIRED)?
#        default?
#        where*
#
# where: WHERE <code block>
#
# var : COLON label '(' var_or_unpack ')' # label is classish, with only /a-z0-9_/i allowed
#     | COLON VAR
#     | var_or_unpack
#
# var_or_unpack : '[' param* ']' # should all be required + un-named
#               | '{' param* '}' # Should all be named
#               | VAR
#
# OPTIONAL: '?'
# REQUIRED: '!'
sub param {
  my $self = shift;
  my $class_meth;
  unless (blessed($self)) {
    $self = $self->new(@_ == 1 ? (input => $_[0]) : @_);
    $class_meth = 1;
  }

  my $param = {};
  my $consumed = 0;

  my $token = $self->token;
  if ($token->{type} eq 'class') {
    $param->{type_constraint} = $token->{literal};
    $self->consume_token;
    $token = $self->token;
    $consumed = 1;
  }

  if ($token->{type} eq ':') {
    $param->{named} = 1;
    $self->consume_token;
    $token = $self->token;
    $consumed = 1;

    # Probably a label
    if ($token->{type} eq 'class') {
      $param->{label} = $self->consume_token->{literal};

      die "label required, class or type constraint found"
        if $param->{label} =~ /[^a-zA-Z0-9_]/;

      $self->assert_token('(');
      $token = $self->token;
    }
  }

  # positionals are required by default, named params aren't
  $param->{required} = !$param->{named};

  #use Data::Dumper; warn Dumper($param, $token);
  if ($token->{type} eq '[') {
    die "Label required for named non-scalar param"
      if $param->{named} && !defined $param->{label};

    $param->{positional_params} = $self->unpacked_array;
  } elsif ($token->{type} eq '{') {
    die "Label required for named non-scalar param"
      if $param->{named} && !defined $param->{label};

    $param->{named_params} = $self->unpacked_hash;
  } elsif ($consumed || $token->{type} eq 'var') {
    $param->{variable_name} = $self->assert_token('var')->{literal};

    die "Label required for named non-scalar param"
      if $param->{variable_name} !~ /^\$/ && 
         $param->{named} && !defined $param->{label};
    
  } else {
    return;
  }

  if (defined $param->{label}) {
    $self->assert_token(')');
  }

  $token = $self->token;

  if ($token->{type} eq '?') {
    $param->{required} = 0;
    $self->consume_token;
    $token = $self->token;
  } elsif ($token->{type} eq '!') {
    $param->{required} = 1;
    $self->consume_token;
    $token = $self->token;
  }

  if ($token->{type} eq '=') {
    # default value
    $self->consume_token;

    $param->{default_value} = $self->value_ish();

    $token = $self->token;
  }

  while ($token->{type} eq 'WHERE') {
    $self->consume_token;

    $param->{constraints} ||= [];
    my ($code) = extract_codeblock(${$self->_input});

    # Text::Balanced *sets* $@. How horrible.
    die "$@" if $@;

    substr(${$self->_input}, 0, length($code), '');
    push @{$param->{constraints}}, $code;

    $token = $self->token;
  }

  #use Data::Dumper; $Data::Dumper::Indent = 1;warn Dumper($param);
  if ($class_meth) {
    return wantarray ? ($param, $self->remaining_input) : $param;
  } else {
    return $self->create_param($param);
  }
}

# Used by default production.
#
# value_ish: number_literal
#          | quote_like
#          | variable
#          | balanced
#          | closure

sub value_ish {
  my ($self) = @_;

  my $data = $self->_input;
  my $num = $self->_number_like;
  return $num if defined $num;

  my $default = $self->_quote_like || $self->_variable_like;
  return $default;
}

sub _number_like {
  my ($self) = @_;
  # This taken from Perl6::Signatures, which in turn took it from perlfaq4
  my $number_like = qr/^
                      ( ([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?# float
                      | -?(?:\d+(?:\.\d*)?|\.\d+)                      # decimal
                      | -?\d+\.?\d*                                    # real
                      | [+-]?\d+                                       # +ve or -ve integer
                      | -?\d+                                          # integer
                      | \d+                                            # whole number
                      | 0x[0-9a-fA-F]+                                 # hexadecimal
                      | 0b[01]+                                        # binary
                      # note that octals will be captured by the "whole number"
                      # production. Our consumer will have to eval this (we don't
                      # want to do it for them because of roundtripping. But maybe
                      # we need annotation nodes anyway?
                      )/x;

  my $data = $self->_input;

  my ($num) = $$data =~ /$number_like/;

  if (defined $num) {
    substr($$data, 0, length($num), '');
    return $num;
  }
  return undef;
}

sub _quote_like {
  my ($self) = @_;

  my $data = $self->_input;

  my @quote = extract_quotelike($$data);

  return if blessed $@ && $@->{error} =~ /^No quotelike operator found after prefix/;

  die "$@" if $@;
  return unless $quote[0];

  my $op = $quote[3] || $quote[4];

  my %whitelist = map { $_ => 1 } qw(q qq qw qr " ');
  die "rejected quotelike operator: $op" unless $whitelist{$op};

  substr($$data, 0, length $quote[0], '');

  return $quote[0];
}

sub _variable_like {
  my ($self) = @_;

  my $token = $self->token;
  if ($token->{type} eq 'var') {
    $self->consume_token;
    return $token->{literal};
  }
}

sub assert_token {
  my ($self, $type) = @_;

  if ($self->token->{type} eq $type) {
    return $self->consume_token;
  }

  Carp::confess "$type required, found  '" .$self->token->{literal} . "'!";
}

sub token {
  my ($self, $la) = @_;

  $la ||= 0;

  while (@{$self->tokens} <= $la) {
    my $token = $self->next_token($self->_input);

    $token ||= { type => 'NUL' };

    push @{$self->tokens}, $token;
  }
  return $self->tokens->[$la];
}

sub consume_token {
  my ($self) = @_;

  die "No token to consume"
    unless @{$self->tokens};

  return shift @{$self->tokens};
}

our %LEXTABLE = (
  where => 'WHERE'
);

sub next_token {
  my ($self, $data) = @_;

  my $re = qr/^ (\s* (?:
    ([(){}\[\],:=|!?]) |
    (
      [A-Za-z][a-zA-Z0-0_-]+
      (?:::[A-Za-z][a-zA-Z0-0_-]+)*
    ) |
    ([\$\%\@](?:[_A-Za-z][a-zA-Z0-9_]*)?) |
  ) (?:\s*\#.*?[\r\n])?\s*) /x;

  # symbols in $2
  # class-name ish in $3
  # $var in $4

  unless ( $$data =~ s/$re//) {
    die "Error parsing signature at '" . substr($$data, 0, 10);
  }

  my ($orig, $sym, $cls,$var) = ($1,$2,$3, $4);

  return { type => $sym, literal => $sym, orig => $orig }
    if defined $sym;

  if (defined $cls) {
    if ($LEXTABLE{$cls}) {
      return { type => $LEXTABLE{$cls}, literal => $cls, orig => $orig };
    }

    my $tc = $self->extract_tc($cls);
    return {
      type => 'class',
      literal => $tc,
      orig => $orig . substr($tc, length($cls))
    };
  }

  return { type => 'var', literal => $var, orig => $orig }
    if $var;


  die "Shouldn't get here!";
}

sub extract_tc {
  my ($self, $tc) = @_;
  my $data = $self->_input;

  my $level = 0;
  while ($$data =~ s/^([|\[\],])//x) {
    $tc .= $1;
    if ($1 eq '[') {
      $level++;
    } elsif ($1 eq ',') {
      die "Unexpected '$1' in type constraint after '$tc', $level, '$$data'\n"
        unless $level;
    } elsif ($1 eq ']' ) {
      die "Unexpected '$1' in type constraint after '$tc', $level, '$$data'\n"
        unless $level;
      $level--;
      next;
    }

    die "Error parsing type constraint after '$tc' (class-like expected)\n"
      unless $$data =~ s/^ (
        [A-Za-z][a-zA-Z0-0_-]+
        (?:::[A-Za-z][a-zA-Z0-0_-]+)*
        ) //x;
    $tc .= $1;
  }

  die "Unbalanced [] in type constraint: '$tc'\n"
    if $level;

  return $tc;
}

sub remaining_input {
  my ($self) = @_;

  return ${$self->_input} unless @{$self->tokens};

  my $input = '';

  $input .= $_->{orig} for @{$self->tokens};
  $input .= ${$self->_input};
  return $input;
}

__PACKAGE__->meta->make_immutable;

1;

__END__

=head1 NAME

Parse::Method::Signatures - Perl6 like method signature parser

=head1 DESCRIPTION

Inspired by L<Perl6::Signature> but streamlined to just support the subset
deemed useful for L<TryCatch> and L<MooseX::Method::Signatures>.

=head1 TODO

=over

=item * Work out return interface

=back

=head1 METHODS

There are only two public methods to this module, both of which should be
called as class methods.

=head2 signature

 my $sig = Parse::Method::Signatures->signature( '(Str $foo)' )

Attempts to parse the (bracketed) method signature. Returns a value or dies on
error.

=head2 param

  my $param = Parse::Method::Signatures->param( 'Str $foo where { length($_) < 10 }')

Attempts to parse the specification for a single parameter. Returns value or
dies on error.

=head1 CAVEATS

Like Perl6::Signature, the parsing of certain constructs is currently only a
'best effort' - specifically default values and where code blocks might not
successfully for certain complex cases. Patches/Failing tests welcome.

Additionally, default value specifications are not evaluated which means that
no such lexical or similar errors will not be produced by this module.
Constant folding will also not be performed.

=head1 AUTHOR

Ash Berlin C<< <ash@cpan.org> >>.

Thanks to rafl.

=head1 LICENSE

Licensed under the same terms as Perl itself.

