package Parse::Method::Signatures;

use Moose;
use MooseX::Types::Moose qw/ArrayRef HashRef ScalarRef CodeRef Int Str/;
use Text::Balanced qw(
  extract_codeblock
  extract_variable
  extract_quotelike
  extract_bracketed
);

use Data::Dump qw/pp/;
use PPI;
use Moose::Util::TypeConstraints;
use Parse::Method::Signatures::ParamCollection;
use Parse::Method::Signatures::Types qw/PositionalParam NamedParam UnpackedParam/;
use Carp qw/croak/;

use namespace::clean -except => 'meta';

our $VERSION = '1.002000';
our %LEXTABLE;

# Setup what we need for the magic EOF token
@PPI::Token::EOF::ISA = 'PPI::Token';
@PPI::Token::StringifiedWord::ISA = 'PPI::Token::Word'; # Used for LHS of fat comma

class_type "PPI::Document";
class_type "PPI::Element";

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

has 'ppi_doc' => (
    is => 'ro',
    isa => 'PPI::Document',
    lazy_build => 1,
    builder => 'parse',
);

# A bit dirty, but we set this with local most of the time
has 'ppi' => (
    is => 'ro',
    isa => 'PPI::Element',
    lazy_build => 1,
    writer => '_set_ppi'
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

sub parse {
  my ($self) = @_;
  
  my $input = substr($self->input, $self->offset);
  my $doc = PPI::Document->new(\$input);

  # Append the magic EOF Token
  $doc->add_element(bless {}, "PPI::Token::EOF");

  return $doc;
}

sub _build_ppi {
  my ($self) = @_;
  return $self->ppi_doc->find_first("PPI::Statement")->first_element;
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

  $param = $self->create_param($param);

  return !$class_meth
      ? $param
      : wantarray
      ? ($param, "")
      : $param;
}


sub tc {
  my ($self, $required) = @_;

  my $ident = $self->_ident;

  #TODO: Work out how to give some previous context to this.
  $self->error("Error parsing type constraint", $self->ppi)
    if !$ident && $required;

  $self->_tc_params($ident);
}

# Handle parameterized TCs. e.g.:
# ArrayRef[Str]
# Dict[Str => Str]
# Dict["foo bar", Baz]
sub _tc_params {
  my ($self, $tc) = @_;

  my $ppi = $self->ppi;
  return $tc->clone 
    unless $ppi->content eq '[';

  # Get from the '[' token the to Strucure::Constructor 
  $ppi = $ppi->parent;

  $ppi->finish or $self->error("Runaway '[]' in type constraint", $ppi, 1);

  # [ Str => Str ] = { Struct => { Stmt => [ 'Str', '=>', 'Str' ] } }
  $ppi->schildren == 1 or $self->error("Error parsing type constraint", $ppi);
  $self->consume_token; # consume '[';

  my $new = PPI::Statement::Expression->new($tc);
  my $list = PPI::Structure::Constructor->new($ppi->start);

  $new->add_element($list);

  # Maybe break out this into seperate method?
  $self->_add_with_ws($list, $self->_tc_param);

  while ($self->ppi->content =~ /^,|=>$/ ) {

    my $op = $self->consume_token;
    $self->_stringify_last($list) if $op->content eq '=>';
    $self->_add_with_ws($list, $op, 1);

    $list->add_element($self->tc(1));
  }

  $self->error("Error parsing type constraint", $self->ppi)
    if $self->ppi != $ppi->finish;

  # Hmm we seem to have to call a private method. sucky
  $self->_add_ws($list)->_set_finish($self->consume_token->clone);

  return $new;
}

# Valid token for individual component of parameterized TC
sub _tc_param {
  my ($self) = @_;

  (my $class = $self->ppi->class) =~ s/^PPI:://;
  return $self->consume_token
      if $class eq 'Token::Number' ||
         $class =~ /^Token::Quote::(?:Single|Double|Literal|Interpolate)/;

  return $self->tc(1);
}

# Stringify LHS of fat comma
sub _stringify_last {
  my ($self, $list) = @_;
  my $last = $list->last_token;
  return unless $last->isa('PPI::Token::Word');

  # Is this conditional on the content of the word?
  bless $last, "PPI::Token::StringifiedWord";
  return $list;
}

sub _tc_union {
  my ($self, $tc) = @_;

  while ($self->ppi->isa('PPI::Token::Operator') &&
         $self->ppi->content eq '|') {
    
    
  }

  return $tc;
}

sub error {
  my ($self, $msg, $token, $no_in) = @_;
  Carp::croak(
    $msg . " near '$token'" . 
    ($no_in ? ""
           : " in '" . $token->statement . "'" 
    )
  );
}

# Add $token to $collection, preserving WS/comment nodes prior to $token
sub _add_with_ws {
  my ($self, $collection, $token, $trailing) = @_;

  my @elements = ($token);

  my $t = $token->previous_token;

  while ($t && !$t->significant) {
    unshift @elements, $t;
    $t = $t->previous_token;
  }

  if ($trailing) {
    $t = $token->next_token;
    while ($t && !$t->significant) {
      push @elements, $t;
      $t = $t->next_token;
    }
  }

  $collection->add_element($_->clone) for @elements;

  return $collection;
}

# Add ws prior to $self->ppi to $collection
sub _add_ws {
  my ($self, $collection) = @_;

  my @toks;
  my $t = $self->ppi->previous_token;
  while ($t && !$t->significant) {
    unshift @toks, $t;
    $t = $t->previous_token;
  }
  $collection->add_element($_->clone) for @toks;
  return $collection;
}

%LEXTABLE = (
  where => 'WHERE',
  is    => 'TRAIT',
  does  => 'TRAIT',
  '=>'  => ',',
);

sub _ident {
  my ($self) = @_;

  my $ppi = $self->ppi;
  return $self->consume_token
    if $ppi->isa('PPI::Token::Word') &&
       !exists $LEXTABLE{"$ppi"};
}

sub consume_token {
  my ($self) = @_;

  my $ppi = $self->ppi;
  my $ret = $ppi;

  while (!$ppi->isa('PPI::Token::EOF') ) {
    $ppi = $ppi->next_token;
    last if $ppi->significant;
  }

  $self->_set_ppi( $ppi );
  return $ret;
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

