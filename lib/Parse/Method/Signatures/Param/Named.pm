package Parse::Method::Signatures::Param::Named;

use Moose;
use MooseX::Types::Moose qw/Str/;

use namespace::clean -except => 'meta';

extends 'Parse::Method::Signatures::Param';

has label => (
    is         => 'ro',
    isa        => Str,
    lazy_build => 1,
);

sub _build_label {
    my ($self) = @_;
    # strip sigil
    return substr($self->variable_name, 1);
}

sub _stringify_variable_name {
    my ($self, $required) = @_;
    my $ret = q{:};
    $ret .= $self->variable_name;
    $ret .= '!' if $required;
    return $ret;
}

__PACKAGE__->meta->make_immutable;

1;
