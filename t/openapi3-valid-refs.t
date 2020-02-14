use Mojo::Base -strict;
use Test::More;
use JSON::Validator::Schema::OpenAPIv3;

my $schema = JSON::Validator::Schema::OpenAPIv3->new->data(
  'spec/v3-valid_file_refs.yaml');
is @{$schema->errors}, 0, 'contains valid refs';

done_testing;
