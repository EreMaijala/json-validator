package JSON::Validator::Schema::OpenAPIv3;
use Mojo::Base 'JSON::Validator::Schema::OpenAPIv2';

has specification => 'https://spec.openapis.org/oas/3.0/schema/2019-04-02';

sub base_url {
  my $self = shift;
  my $data = $self->data;

  # Set
  if (@_) {
    my $url    = Mojo::URL->new(shift);
    my $server = Mojo::URL->new($data->{servers}[0] || '');
    $server->scheme($url->scheme) if $url->scheme;
    $server->host($url->host)     if $url->host;
    $server->port($url->port)     if $url->port;
    $server->path($url->path)     if $url->path;
    $data->{servers}[0] = $server->to_string;
    return $self;
  }

  # Get
  my $servers = $data->{servers} || [];
  return Mojo::URL->new($servers->[0] || '');
}

sub _response_schema {
  my ($self, $schema) = @_;
  +{
    description => 'Default Mojolicious::Plugin::OpenAPI response.',
    content     => {'application/json' => {schema => $schema}},
  };
}

sub _sub_schemas         { shift->data->{components}{schemas} ||= {} }
sub _sub_schemas_pointer {'#/components/schemas'}

1;

=encoding utf8

=head1 NAME

JSON::Validator::Schema::OpenAPIv3 - OpenAPI version 3

=head1 SYNOPSIS

See L<JSON::Validator::Schema::OpenAPIv2/SYNOPSIS>.

=head1 DESCRIPTION

This class represents L<https://spec.openapis.org/oas/3.0/schema/2019-04-02>.

=head1 ATTRIBUTES

=head2 specification

  my $str    = $schema->specification;
  my $schema = $schema->specification($str);

Defaults to "L<https://spec.openapis.org/oas/3.0/schema/2019-04-02>".

=head1 METHODS

=head2 base_url

  $schema = $schema->base_url("https://example.com/api");
  $schema = $schema->base_url(Mojo::URL->new("https://example.com/api"));
  $url    = $schema->base_url;

Can either retrieve or set the base URL for this schema. This method will
construct the C<$url> from "/servers/0" in the schema, or set it from the input
URL.

=head1 SEE ALSO

L<JSON::Validator>, L<JSON::Validator::Schema::OpenAPIv2>,
L<Mojolicious::Plugin::OpenAPI>.

=cut
