package Parse::Method::Signatures;

use Moose;
use MooseX::Types::Moose qw/ArrayRef HashRef ScalarRef CodeRef Int Str/;
use Text::Balanced qw(
  extract_codeblock
  extract_variable
  extract_quotelike
);

use Parse::Method::Signatures::ParamCollection;
use Parse::Method::Signatures::Types qw/PositionalParam NamedParam UnpackedParam/;
use Carp qw/croak/;

use namespace::clean -except => 'meta';

our $VERSION = '1.002000';
our %LEXTABLE;

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
    isa     => Str,
    default => 'Parse::Method::Signatures::Sig',
);

has 'param_class' => (
    is      => 'ro',
    isa     => Str,
    default => 'Parse::Method::Signatures::Param',
);

has 'type_constraint_class' => (
    is      => 'ro',
    isa     => Str,
    default => 'Parse::Method::Signatures::TypeConstraint',
);

has 'type_constraint_callback' => (
    is        => 'ro',
    isa       => CodeRef,
    predicate => 'has_type_constraint_callback',
);

sub BUILD {
    my ($self) = @_;

    Class::MOP::load_class($_)
        for map { $self->$_ } qw/
            signature_class
            param_class
            type_constraint_class
        /;
}

sub _build__input {
    my $var = substr($_[0]->input, $_[0]->offset);
    return \$var;
}

sub create_param {
    my ($self, $args) = @_;

    my @traits;
    push @traits, $args->{ variable_name } ? 'Bindable' : 'Placeholder'
        if !exists $args->{unpacking};
    push @traits, $args->{ named         } ? 'Named'    : 'Positional';
    push @traits, 'Unpacked::' . $args->{unpacking}
        if exists $args->{unpacking};

    return $self->param_class->new_with_traits(traits => \@traits, %{ $args });
}

override BUILDARGS => sub {
  my $class = shift;

  return { input => $_[0] } if @_ == 1 and !ref $_[0];

  return super();
};

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

  $self = $self->new(@_) unless blessed($self);

  $self->assert_token('(');

  my $args = {};
  my $params = [];

  my $param = $self->param;

  if ($param && $self->token->{type} eq ':') {
    # That param was actualy the invocant
    $args->{invocant} = $param;
    croak "Invocant cannot be named"
      if NamedParam->check($param);
    croak "Invocant cannot be optional"
      if !$param->required;
    croak "Invocant cannot have a default value"
      if $param->has_default_value;

    croak "Invocant must be a simple scalar"
      if UnpackedParam->check($param) || $param->sigil ne '$';

    $self->consume_token;
    $param = $self->param;

  }

  if ($param) {
    push @$params, $param;

    my $greedy = $param->sigil ne '$' ? $param : undef;
    my $opt_pos_param = !$param->required;

    while ($self->token->{type} eq ',') {
      $self->consume_token;

      $param = $self->param;
      croak "parameter expected"
        if !$param;

      my $is_named = NamedParam->check($param);
      if (!$is_named) {
        if ($param->required && $opt_pos_param) {
          croak "Invalid: Required positional param '"
            . $param->variable_name . "' found after optional one.\n";
        }
        if ($greedy) {
          croak "Invalid: Un-named parameter '" . $param->variable_name
            . "' after greedy '" 
            . $greedy->variable_name . "'\n";
        }
      }

      push @$params, $param;
      $opt_pos_param = $opt_pos_param || !$param->required;
      $greedy = $param->sigil ne '$' ? $param : undef;
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

    croak "Cannot have named parameters in an unpacked-array"
      if NamedParam->check($param);

    croak "Cannot have optional parameters in an unpacked-array"
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

    croak "Cannot have positional parameters in an unpacked-hash: " . $param->to_string
      if $param->sigil eq '$' && PositionalParam->check($param);

    push @$params, $param;

    last if ($self->token->{type} eq '}');
    $self->assert_token(',');
  }
  $self->assert_token('}');

  return $params;
}

# param: tc?
#        var
#        (OPTIONAL|REQUIRED)?
#        default?
#        where*
#        trait*
#
# where: WHERE <code block>
#
# trait: TRAIT class
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
    $self = $self->new(@_) unless blessed($self);
    $class_meth = 1;
  }

  my $param = {};

  $self->_param_possibly_labeled(
    $param, $self->_param_tc($param, 0),
    sub {
      my $consumed = shift;
      $self->_param_array($param)
      || $self->_param_hash($param)
      || $self->_param_variable($param, $consumed)
    }
  ) || return;

  $self->_param_required_or_optional($param);
  $self->_param_default_value($param);
  $self->_param_constraints($param);
  $self->_param_traits($param);

  #use Data::Dumper; $Data::Dumper::Indent = 1;warn Dumper($param);
  $param = $self->create_param($param);
  if ($class_meth) {
    return wantarray ? ($param, $self->remaining_input) : $param;
  } else {
    return $param
  }
}

sub _param_tc {
  my ($self, $param, $consumed) = @_;
  my @tc = $self->tc;
  return $consumed unless @tc;

  my $tc = $self->type_constraint_class->new(
    str  => $tc[1],
    data => $tc[0],
    $self->has_type_constraint_callback
      ? (tc_callback => $self->type_constraint_callback)
      : ()
  );
  $param->{type_constraints} = $tc;
  return 1;
}

sub _param_colon_label {
  my ($self, $param, $consumed) = @_;
  $self->token->{type} eq ':' or return $consumed;

  $param->{named} = 1;
  $self->consume_token;

  # Probably a label
  if ($self->token->{type} eq 'ident') {
    $param->{label} = $self->consume_token->{literal};

    $self->assert_token('(');
  }
  return 1;
}

sub _param_possibly_labeled {
  my ($self, $param, $consumed, $inner_parser) = @_;
  $consumed = $self->_param_colon_label($param, $consumed);
  $param->{required} = !$param->{named};

  return unless $inner_parser->($consumed);

  $self->assert_token(')') if defined($param->{label});
  1;
}

sub _param_required_or_optional {
  my ($self, $param) = @_;
  my $token = $self->token;
  if ($token->{type} =~ /[\?!]/) {
    $param->{required} = ($token->{type} eq '!');
    $self->consume_token;
  }
  return $param;
}

sub _param_default_value {
  my ($self, $param) = @_;
  return $param unless $self->token->{type} eq '=';
  $self->consume_token;

  $param->{default_value} = $self->value_ish();
  return $param;
}

sub _param_constraints {
  my ($self, $param) = @_;
  while ($self->token->{type} eq 'WHERE') {
    $self->consume_token;

    $param->{constraints} ||= [];
    my ($code) = extract_codeblock(${$self->_input});

    # Text::Balanced *sets* $@. How horrible.
    croak "$@" if $@;

    substr(${$self->_input}, 0, length($code), '');
    push @{$param->{constraints}}, $code;
  }
}

sub _param_traits {
  my ($self, $param) = @_;
  my $token = $self->token;
  while ($token->{type} eq 'TRAIT') {
    $self->consume_token;
    my $trait = $self->assert_token('ident')->{literal};

    $param->{param_traits} ||= [];
    push @{$param->{param_traits}}, [$token->{literal} => $trait];
    $token = $self->token;
  }
}

sub _param_array {
  my ($self, $param) = @_;
  return unless $self->token->{type} eq '[';

  croak "Label required for named non-scalar param"
    if $param->{named} && !defined $param->{label};

  $param->{params} = $self->unpacked_array;
  $param->{sigil} = '$';
  $param->{unpacking} = 'Array';
}

sub _param_hash {
  my ($self, $param) = @_;
  return unless $self->token->{type} eq '{';

  croak "Label required for named non-scalar param"
    if $param->{named} && !defined $param->{label};

  $param->{params} = $self->unpacked_hash;
  $param->{sigil} = '$';
  $param->{unpacking} = 'Hash';
}

sub _param_variable {
  my ($self, $param, $consumed) = @_;
  return unless $consumed || $self->token->{type} eq 'var';

  $param->{variable_name} = $self->assert_token('var')->{literal};
  $param->{sigil} = substr($param->{variable_name}, 0, 1);

  croak "Label required for named non-scalar param"
    if $param->{variable_name} !~ /^\$/ &&
       $param->{named} && !defined $param->{label};

  return 1;
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

  croak "$@" if $@;
  return unless $quote[0];

  my $op = $quote[3] || $quote[4];

  my %whitelist = map { $_ => 1 } qw(q qq qw qr " ');
  croak "rejected quotelike operator: $op" unless $whitelist{$op};

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

# tc: CLASS ('::' CLASS)*
#   | tc '[' tc (',' tc)* ']'
#   | tc '|' tc

sub tc {
  my ($self, $required) = @_;
  my ($tc, $tc_str);

  my $state = $self->_ident($required) or return;
  $state = $self->_tc_params($state);
  @{$self->_tc_alternation($state)};
}

sub _ident {
  my($self, $required) = @_;
  my $token = $self->token;

  my $tc_str = $token->{literal};
  my $full = $token->{orig};
  if ($token->{type} ne 'ident' && !exists $LEXTABLE{$token->{literal}}) {
    return unless ($required);

    $self->assert_token('ident');
  }
  $self->consume_token;

  while (( $token = $self->token)->{type} eq '::') {
    $tc_str .= '::';
    $full .= $token->{orig};
    $self->consume_token;

    $token = $self->token;
    if ($token->{type} ne 'ident' && !exists $LEXTABLE{$token->{literal}}) {
      $self->assert_token('ident');
    }
    $full .= $token->{orig};
    croak "Invalid spacing in type constraint after '$tc_str'\n"
    if ($full =~ /\s::|::\s/ms);
    $tc_str .= $self->consume_token->{literal};
  }
  return $tc_str;
}

sub _tc_params {
  my($self, $tc_str) = @_;
  my $token = $self->token;

  return([$tc_str, $tc_str]) unless $token->{type} eq '[';

  my $tc = $tc_str;
  $tc_str .= '[';
  $self->consume_token;

  my @params = ($self->tc(1));
  $tc_str .= pop @params;

  while ($self->token->{type} eq ',') {
    my $lit = $self->consume_token->{literal};

    my ($sub, $str) = $self->tc(1);

    # Turn previous arg into explicit str if followed by a fat comma
    if ($lit eq '=>' && !ref $params[-1]) {
      $params[-1] = { -str => $params[-1] };
    }
    push @params, $sub;

    $tc_str .= "," . $str;
  }

  $self->assert_token(']');
  $tc_str .= ']';

  $tc = { -type => $tc, -params => \@params };
  return [$tc, $tc_str];
}

sub _tc_alternation {
  my $self = shift;
  my ($tc, $tc_str) = @{$_[0]};
  return([$tc, $tc_str]) unless $self->token->{type} eq '|';

  my @tcs = ( $tc );

  while ($self->token->{type} eq '|') {
    $tc_str .= '|';
    $self->consume_token;
    my ($sub, $str) = $self->tc(1);
    push @tcs, $sub;
    $tc_str .= $str;
  }

  return [{ -or => \@tcs }, $tc_str];
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

  Carp::confess "No token to consume"
    unless @{$self->tokens};

  return shift @{$self->tokens};
}

%LEXTABLE = (
  where => 'WHERE',
  is    => 'TRAIT',
  does  => 'TRAIT',
  '=>'  => ',',
);

sub _null_token {
    { type => 'NUL' }
}

sub _symbol_token {
    my ($self, $sym, $orig) = @_;
    return unless $sym;

    $self->_make_token(($LEXTABLE{$sym} || $sym), $sym, $orig);
}

sub _class_token {
    my ($self, $cls, $orig) = @_;
    return unless $cls;

    $self->_make_token(($LEXTABLE{$cls} || 'ident'), $cls, $orig);
}

sub _var_token {
    my ($self, $var, $orig) = @_;
    return unless $var;
    $self->_make_token('var', $var, $orig);
}

sub _make_token {
    my ($self, $type, $literal, $orig) = @_;

    {type => $type, literal => $literal, orig => $orig}
}

sub next_token {
  my ($self, $data) = @_;

  return { type => 'NUL' } if $$data =~ m/^\s*$/;

  my $re = qr/^ (\s* (?:
    ( => | [(){}\[\],=|!?] | :{1,2} ) |
    ( [A-Za-z_][a-zA-Z0-0_-]+ ) |
    ( [\$\%\@] (?: [_A-Za-z][a-zA-Z0-9_]* )? ) |
  ) (?:\s*\#.*?[\r\n])?\s*) /x;

  # symbols in $2
  # class-name/identifier in $3
  # $var in $4

  unless ( $$data =~ s/$re//) {
    croak "Error parsing signature at '" . substr($$data, 0, 10) . "'";
  }

  my ($orig, $sym, $cls,$var) = ($1,$2,$3, $4);

     $self->_symbol_token($sym, $orig)
  || $self->_class_token($cls, $orig)
  || $self->_var_token($var, $orig);
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

=item * Document the parameter return types.

=item * Probably lots of other things

=back

=head1 METHODS

There are only two public methods to this module, both of which should be
called as class methods. Both methods accept  either a single (non-ref) scalar
as the value for the L</input> attribute, or normal new style arguments (hash
or hash-ref).

=head2 signature

 my $sig = Parse::Method::Signatures->signature( '(Str $foo)' )

Attempts to parse the (bracketed) method signature. Returns a value or croaks
on error.

=head2 param

  my $param = Parse::Method::Signatures->param( 'Str $foo where { length($_) < 10 }')

Attempts to parse the specification for a single parameter. Returns value or
croaks on error.

=head1 ATTRIBUTES

All the attributes on this class are read-only.

=head2 input

B<Type:> Str

The string to parse.

=head2 offset

B<Type:> Int

Offset into L</input> at which to start parsing. Useful for using with
Devel::Declare linestring

=head2 signature_class

B<Default:> Parse::Method::Signatures::Sig

B<Type:> Str (loaded on demand class name)

=head2 param_class

B<Default:> Parse::Method::Signatures::Param

B<Type:> Str (loaded on demand class name)

=head2 type_constraint_class

B<Default:> L<Parse::Method::Signatures::TypeConstraint>

B<Type:> Str (loaded on demand class name)

Class that is used to turn the parsed type constraint into an actual
L<Moose::Meta::TypeConstraint> object.

=head2 type_constraint_callback

B<Type:> Code Ref

Passed to the constructor of L</type_constraint_class>. Default implementation
of this callback asks Moose for a type constrain matching the name passed in.
If you have more complex requirements, such as parsing types created by
L<MooseX::Types> then you will want a callback similar to this:

 # my $target_package defined elsewhere.
 my $tc_cb = sub {
   my ($pms_tc, $name) = @_;
   my $code = $target_package->can($name);
   $code ? eval { $code->() } 
         : $pms_tc->find_registered_constraint($name);
 }

=head1 CAVEATS

Like Perl6::Signature, the parsing of certain constructs is currently only a
'best effort' - specifically default values and where code blocks might not
successfully for certain complex cases. Patches/Failing tests welcome.

Additionally, default value specifications are not evaluated which means that
no such lexical or similar errors will not be produced by this module.
Constant folding will also not be performed.

=head1 AUTHOR

Ash Berlin <ash@cpan.org>.

Thanks to Florian Ragwitz <rafl@debian.org>.

=head1 SEE ALSO

L<Devel::Declare> which is used by most modules that use this (currently by
all modules known to the author.)

L<http://github.com/ashb/trycatch/tree>.

L<MooseX::Method::Signatures> and L<MooseX::Declare> which are due to be ported
to use this module.

=head1 LICENSE

Licensed under the same terms as Perl itself.

