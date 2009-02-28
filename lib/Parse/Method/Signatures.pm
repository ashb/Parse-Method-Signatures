package Parse::Method::Signatures;

use Moose;
use MooseX::Types::Moose qw/ArrayRef HashRef ScalarRef CodeRef Int Str/;

use Data::Dump qw/pp/;
use PPI;
use Moose::Util::TypeConstraints;
use Parse::Method::Signatures::ParamCollection;
use Parse::Method::Signatures::Types qw/PositionalParam NamedParam UnpackedParam/;
use Carp qw/croak/;

use namespace::clean -except => 'meta';

our $VERSION = '1.002000';
our $ERROR_LEVEL = 0;
our %LEXTABLE;

# Setup what we need for specific PPI subclasses
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
  $doc->add_element(PPI::Token::EOF->new(""));

  # Annoyingly "m($x)" gets treated as a regex operator. This isn't what we 
  # want. so replace it with a Word, then a list. The way we do this is by
  # taking the operator off the front, then reparsing the rest of the content
  # This will look the same (so wont affect anything in a code block) but is
  # just store different token wise.
  $self->_replace_regexps($doc);

  return $doc;
}

sub _replace_regexps {
  my ($self, $doc) = @_;

  foreach my $node ( @{ $doc->find('Token::Regexp') || [] } ) {
    my $str = $node->content;

    # Rather annoyingly, there are *no* methods on Token::Regexp;
    my ($word, $rest) = $str =~ /^(@{[$node->{operator}]})(.*)$/s;

    my $subdoc = PPI::Document->new(\$rest);
    my @to_add = reverse map { $_->remove } $subdoc->children;
    push @to_add, new PPI::Token::Word($word);
    # insert_after restricts what you can insert.
    # $node->insert_after($_) for @to_add;
    $node->__insert_after($_) for @to_add;

    $node->delete;
  }
}

sub _build_ppi {
  my ($self) = @_;
  my $ppi = $self->ppi_doc->first_token;

  if ($ppi->class eq 'PPI::Token::Word' && exists $LEXTABLE{"$ppi"}) {
    bless $ppi, "PPI::Token::LexSymbol";
    $ppi->{lex} = $LEXTABLE{"$ppi"};
  }
  return $ppi;
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

  if ($param && $self->ppi->content eq ':') {
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

    while ($self->ppi->content eq ',') {
      $self->consume_token;

      my $err_ctx = $self->ppi;
      $param = $self->param;
      $self->error($err_ctx, "Parameter expected")
        if !$param;

      my $is_named = NamedParam->check($param);
      if (!$is_named) {
        if ($param->required && $opt_pos_param) {
          $self->error($err_ctx, "Invalid: Required positional param " .
            " found after optional one");
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

  #return wantarray ? ($sig, $self->remaining_input) : $sig;
  return $sig;
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

  $param->{required} = 1;

  $self->_param_labeled($param, 0)
    || $self->_param_named($param, 0)
    || $self->_param_variable($param, 0)
    || return;#$self->error($self->ppi);

  $self->_param_constraint_or_traits($param);

  $param = $self->create_param($param);

  return !$class_meth
      ? $param
      : wantarray
      ? ($param, "")
      : $param;
}

sub _param_constraint_or_traits {
  my ($self, $param) = @_;

  while ($self->_param_where($param) ||
         $self->_param_traits($param) ) {
    # No op;

  }
}

sub _param_where {
  my ($self, $param) = @_;

  return unless $self->ppi->isa('PPI::Token::LexSymbol')
             && $self->ppi->lex eq 'WHERE';

  $self->consume_token;

  $param->{constraints} ||= [];

  my $ppi = $self->ppi;

  $self->error($ppi, "Block expected after where")
    unless $ppi->class eq 'PPI::Token::Structure'
        && $ppi->content eq '{';

  # Go from token to block
  $ppi = $ppi->parent;

  $ppi->finish or $self->error($ppi, 
    "Runaway '" . $ppi->braces . "' in " . $self->_parsing_area(1), 1);

  push @{$param->{constraints}}, $ppi->content;

  $self->_set_ppi($ppi->finish);
  $self->consume_token;
  return 1;
}

sub _param_traits {
  my ($self, $param) = @_;
  return unless $self->ppi->isa('PPI::Token::LexSymbol')
             && $self->ppi->lex eq 'TRAIT';

  $self->consume_token;
}

sub _param_labeled {
  my ($self, $param) = @_;

  return unless 
    $self->ppi->content eq ':' &&
    $self->ppi->next_token->isa('PPI::Token::Word');

  $self->consume_token;

  $self->error($self->ppi, "Invalid label")
    if $self->ppi->content =~ /[^-\w]/;

  $param->{named} = 1;
  $param->{required} = 0;
  $param->{label} = $self->consume_token->content;

  $self->assert_token('(');
  $self->_unpacked_param($param) 
    || $self->_param_variable($param)
    || $self->error($self->ppi);

  $self->assert_token(')');

  return 1;
}

sub _unpacked_param {
  my ($self, $param) = @_;

  return $self->bracketed('[', \&unpacked_array, $param) ||
         $self->bracketed('{', \&unpacked_hash, $param);
}

sub _param_named {
  my ($self, $param) = @_;

  return unless
    $self->ppi->content eq ':' &&
    $self->ppi->next_token->isa('PPI::Token::Symbol');
  $param->{required} = 0;
  $param->{named} = 1;
  $self->consume_token;
 
  return $self->_param_variable($param);
}

sub _param_variable {
  my ($self, $param) = @_;

  my $ppi = $self->ppi;
  return unless $ppi->isa('PPI::Token::Symbol');
  $ppi->symbol_type eq $ppi->raw_type or $self->error($ppi);

  $param->{sigil} = $ppi->raw_type;
  $param->{variable_name} = $self->consume_token->content;

  if ($self->ppi->class eq 'PPI::Token::Operator') {
    my $c = $self->ppi->content;
    if ($c eq '?') {
      $param->{required} = 0;
      $self->consume_token;
    } elsif ($c eq '!') {
      $param->{required} = 1;
      $self->consume_token;
    }
  }

  return 1;
}

sub unpacked_hash {
  my ($self, $list, $param) = @_;

  my $params = [];
  while ($self->ppi->content ne '}') {
    my $errctx = $self->ppi;
    my $p = $self->param
      or $self->error($self->ppi);

    $self->error($errctx, "Cannot have positional parameters in an unpacked-array")
      if $p->sigil eq '$' && PositionalParam->check($p);
    push @$params, $p;

    last if $self->ppi->content eq '}';
    $self->assert_token(',');
  }
  $param->{params} = $params;
  $param->{sigil} = '$';
  $param->{unpacking} = 'Hash';
  return $param;
}

sub unpacked_array {
  my ($self, $list, $param) = @_;

  my $params = [];
  while ($self->ppi->content ne ']') {
    my $watermark = $self->ppi;
    my $param = $self->param
      or $self->error($self->ppi);

    $self->error($watermark, "Cannot have named parameters in an unpacked-array")
      if NamedParam->check($param);

    $self->error($watermark, "Cannot have optional parameters in an unpacked-array")
      unless $param->required;

    push @$params, $param;

    last if $self->ppi->content eq ']';
    $self->assert_token(',');
  }
  $param->{params} = $params;
  $param->{sigial} = '$';
  $param->{unpacking} = 'Array';
  return $param;
}

sub tc {
  my ($self, $required) = @_;

  my $ident = $self->_ident;

  $self->error($self->ppi)
    if !$ident && $required;

  return $self->_tc_union(
    $self->bracketed('[', \&_tc_params, $ident)
      || $ident->clone
  );
}

# Handle parameterized TCs. e.g.:
# ArrayRef[Str]
# Dict[Str => Str]
# Dict["foo bar", Baz]
sub _tc_params {
  my ($self, $list, $tc) = @_;

  my $new = PPI::Statement::Expression->new($tc);
  $new->add_element($list);

  $self->_add_with_ws($list, $self->_tc_param);

  while ($self->ppi->content =~ /^,|=>$/ ) {

    my $op = $self->consume_token;
    $self->_stringify_last($list) if $op->content eq '=>';
    $self->_add_with_ws($list, $op, 1);

    $list->add_element($self->tc(1));
  }

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

sub _tc_union {
  my ($self, $tc) = @_;
  
  return $tc unless $self->ppi->content eq '|';

  my $union = PPI::Statement::Expression::Union->new($tc);
  while ( $self->ppi->content eq '|' ) {
   
    $self->consume_token;
    $union->add_element($self->tc(1));
  }

  return $union;
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

# Handle the boring bits of bracketed product, then call $code->($self, ...) 
sub bracketed {
  my ($self, $type, $code, @args) = @_;

  local $ERROR_LEVEL = $ERROR_LEVEL + 1;
  my $ppi = $self->ppi;
  return unless $ppi->content eq $type;

  $self->consume_token; # consume '[';

  # Get from the '[' token the to Strucure::Constructor 
  $ppi = $ppi->parent;

  $ppi->finish or $self->error($ppi, 
    "Runaway '" . $ppi->braces . "' in " . $self->_parsing_area(1), 1);

  my $list = PPI::Structure::Constructor->new($ppi->start);

  my $ret = $code->($self, $list, @args);

  $self->error($self->ppi)
    if $self->ppi != $ppi->finish;

  # Hmm we seem to have to call a private method. sucky
  $self->_add_ws($list)->_set_finish($self->consume_token->clone);

  return $ret;
}

# Work out what sort of production we are in for sane default error messages
sub _parsing_area { 
  shift;
  my $height = shift || 0;
  my (undef, undef, undef, $sub) = caller($height+$ERROR_LEVEL);

  return "type constraint" if $sub =~ /(?:\b|_)tc(?:\b|_)/;
  return "unpacked parameter"      
                           if $sub =~ /(?:\b|_)unpacked(?:\b|_)/;
  return "parameter"       if $sub =~ /(?:\b|_)param(?:\b|_)/;
  return "signature"       if $sub =~ /(?:\b|_)signature(?:\b|_)/;

  " unknown production ($sub)";
}

# error(PPI::Token $token, Str $msg?, Bool $no_in = 0)
sub error {
  my ($self, $token, $msg, $no_in) = @_;

  unless ($msg) {
    $msg = "Error parsing " . $self->_parsing_area(2);
  }

  Carp::croak(
    $msg . " near '$token'" . 
    ($no_in ? ""
           : " in '" . $token->statement . "'" 
    )
  );
}

sub assert_token {
  my ($self, $need, $msg) = @_;

  if ($self->ppi->content ne $need) {
    $self->error($self->ppi, "'$need' expected whilst parsing " . $self->_parsing_area(2));
  }
  return $self->consume_token;
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
);

sub _ident {
  my ($self) = @_;

  my $ppi = $self->ppi;
  return $self->consume_token
    if $ppi->class eq 'PPI::Token::Word';
  return undef;
}

sub consume_token {
  my ($self) = @_;

  my $ppi = $self->ppi;
  my $ret = $ppi;

  while (!$ppi->isa('PPI::Token::EOF') ) {
    $ppi = $ppi->next_token;
    last if $ppi->significant;
  }

  if ($ppi->class eq 'PPI::Token::Word' && exists $LEXTABLE{"$ppi"}) {
    bless $ppi, "PPI::Token::LexSymbol";
    $ppi->{lex} = $LEXTABLE{"$ppi"};
  }
  $self->_set_ppi( $ppi );
  return $ret;
}

__PACKAGE__->meta->make_immutable;


{ package 
    PPI::Statement::Expression::Union;
  use base 'PPI::Statement::Expression';

  sub content {
    my ($self) = @_;

    join('|', $self->children );
  }
}

{ package 
    PPI::Token::LexSymbol;
  use base 'PPI::Token::Word';

  sub lex {
    my ($self) = @_;
    return $self->{lex}
  }
}

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

