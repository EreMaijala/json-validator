use Mojo::Base -strict;
use Test::More;
use JSON::Validator::Schema::OpenAPIv3;

my $schema = JSON::Validator::Schema::OpenAPIv3->new->data(
  'spec/v3-invalid_file_refs.yaml');
like $schema->errors->[0], qr{Properties not allowed: definitions},
  'definitions not allowed';

$schema->data('spec/v3-invalid_file_refs_no_path.yaml');
like $schema->errors->[0], qr{Properties not allowed: definitions},
  'wrong placement of data';

done_testing;
