package Parse::Method::Signatures::Param;

use Moose::Role;
use namespace::clean -except => 'meta';

has required => (
    is       => 'ro',
    isa      => 'Bool',
    required => 1
);

requires 'to_string';

1;
