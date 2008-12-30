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

sub _label_from_variable_name {
    my ($self) = @_;
    # strip sigil
    return substr($self->variable_name, 1);
}

sub _build_label {
    my ($self) = @_;
    return $self->_label_from_variable_name;
}

sub _stringify_variable_name {
    my ($self, $required) = @_;
    my $ret = q{:};
    my ($before, $after) = (q{}) x 2;

    if ($self->label ne $self->_label_from_variable_name) {
        $before = $self->label . q{(};
        $after  = q{)};
    }

    $ret .= $before . $self->variable_name . $after;

    $ret .= '!' if $required;
    return $ret;
}

__PACKAGE__->meta->make_immutable;

1;
