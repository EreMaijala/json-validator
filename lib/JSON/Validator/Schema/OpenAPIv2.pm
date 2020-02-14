package JSON::Validator::Schema::OpenAPIv2;
use Mojo::Base 'JSON::Validator::Schema::Draft4';

use JSON::Validator::Util 'E';
use Mojo::URL;
use Scalar::Util 'looks_like_number';
use Time::Local ();

use constant DEBUG   => $ENV{JSON_VALIDATOR_DEBUG} || 0;
use constant IV_SIZE => eval 'require Config;$Config::Config{ivsize}';

our %COLLECTION_RE
  = (pipes => qr{\|}, csv => qr{,}, ssv => qr{\s}, tsv => qr{\t});

has allow_invalid_ref => 0;

has errors => sub {
  my $self = shift;
  my $clone
    = $self->new(%$self, allow_invalid_ref => 0)->data($self->specification);
  my @errors = $clone->validate($self->data);

  $self->get(
    ['paths', undef, undef, 'parameters'],
    sub {
      push @errors, E $_[1], 'Only one parameter can have "in":"body"'
        if +(grep { $_->{in} eq 'body' } @{$_[0] || []}) > 1;
    }
  );

  return \@errors;
};

has specification => 'http://swagger.io/v2/schema.json';

sub base_url {
  my $self = shift;
  my $data = $self->data;

  # Set
  if (@_) {
    my $url = Mojo::URL->new(shift);
    $data->{schemes}[0] = $url->scheme    if $url->scheme;
    $data->{host}       = $url->host_port if $url->host_port;
    $data->{basePath}   = $url->path      if $url->path;
    return $self;
  }

  # Get
  my $url     = Mojo::URL->new;
  my $schemes = $data->{schemes} || [];
  $url->scheme($schemes->[0] || 'http');

  my ($host, $port) = split ':', ($data->{host} || '');
  $url->host($host) if length $host;
  $url->port($port) if length $port;

  $url->path($data->{basePath}) if $data->{basePath};

  return $url;
}

sub data {
  my $self = shift;
  return $self->{data} // {} unless @_;

  if ($self->allow_invalid_ref) {
    my $clone = $self->new(%$self, allow_invalid_ref => 0);
    $self->{data} = $clone->data(shift)->bundle({replace => 1})->data;
  }
  else {
    $self->{data} = $self->_resolve(shift);
  }

  if (my $class = $self->version_from_class) {
    my $version = $class->can('VERSION') && $class->VERSION;
    $self->{data}{info}{version} = "$version" if length $version;
  }

  delete $self->{errors};
  return $self;
}

sub default_response_schema {
  +{
    type       => 'object',
    required   => ['errors'],
    properties => {
      errors => {
        type  => 'array',
        items => {
          type     => 'object',
          required => ['message'],
          properties =>
            {message => {type => 'string'}, path => {type => 'string'}}
        },
      },
    },
  };
}

sub ensure_default_response {
  my ($self, $params) = @_;

  my $name       = $params->{name} || 'DefaultResponse';
  my $def_schema = $self->_sub_schemas->{$name}
    ||= $self->default_response_schema;
  tie my %ref, 'JSON::Validator::Ref', $def_schema, $self->_sub_schemas_pointer;

  my $codes      = $params->{codes} || [400, 401, 404, 500, 501];
  my $res_schema = $self->_response_schema(\%ref);
  $self->get(
    ['paths', undef, undef, 'responses'],
    sub { $_[0]->{$_} ||= $res_schema for @$codes },
  );

  delete $self->{errors};
  return $self;
}

sub validate_request {
  my ($self, $c, $schema) = @_;
  local $self->{validate_request} = 1;

  my ($cache, @errors) = {};
  for my $p (@{$schema->{parameters} || []}) {
    my ($in, $name, $type) = @$p{qw(in name type)};
    my ($exists, $value) = (0, undef);

    if ($in eq 'body') {
      $value  = $cache->{$in} ||= $c->openapi->get_request_data($in);
      $exists = length $value if defined $value;
    }
    elsif ($in eq 'formData' and $type eq 'file') {
      $value  = $cache->{$in} ||= $c->openapi->get_request_data('upload');
      $exists = $value ? 1 : 0;
    }
    else {
      my $key = $in eq 'header' ? lc $name : $name;
      $value  = $cache->{$in} ||= $c->openapi->get_request_data($in);
      $exists = exists $value->{$key};
      $value  = $value->{$key};
    }

    if (defined $value and ($type || '') eq 'array') {
      $value = $self->_coerce_by_collection_format($value, $p);
    }

    ($value, $exists) = $self->_get_default_value_from_parameter($p)
      unless $exists;

    if ($type and defined $value) {
      $value = $value->[-1] if $type ne 'array' and ref $value eq 'ARRAY';

      if ($type eq 'integer' or $type eq 'number') {
        $value += 0 if looks_like_number $value;
      }
      elsif ($type eq 'boolean') {
        if (!$value or $value =~ /^(?:false)$/) {
          $value = Mojo::JSON->false;
        }
        elsif ($value =~ /^(?:1|true)$/) {
          $value = Mojo::JSON->true;
        }
      }
    }

    if (my @e = $self->_validate_request_value($p, $name => $value)) {
      push @errors, @e;
    }
    elsif ($exists and defined $value) {
      $c->openapi->set_request_data($in, $name => $value);
    }
  }

  return @errors;
}

sub validate_response {
  my ($self, $c, $sub_schema, $status, $data) = @_;

  return E '/' => "No responses rules defined for status $status."
    unless my $res_schema
    = $sub_schema->{responses}{$status} || $sub_schema->{responses}{default};

  my @errors;
  push @errors, $self->_validate_response_headers($c, $res_schema->{headers})
    if $res_schema->{headers};

  if ($res_schema->{'x-json-schema'}) {
    warn "[OpenAPI] Validate using x-json-schema\n" if DEBUG;
    push @errors, $self->_validate($data, '', $res_schema->{'x-json-schema'});
  }
  elsif ($res_schema->{schema}) {
    warn "[OpenAPI] Validate using schema\n" if DEBUG;
    push @errors, $self->_validate($data, '', $res_schema->{schema});
  }

  return @errors;
}

sub version_from_class {
  my $self = shift;
  return $self->{version_from_class} || '' unless @_;

  my $class = shift;
  $self->{version_from_class} = $class;
  $self->{data}{info}{version} = $class->VERSION;
  return $self;
}

sub _build_formats {
  my $self    = shift;
  my $formats = $self->SUPER::_build_formats;

  $formats->{binary} = sub {undef};
  $formats->{byte}   = \&_match_byte_string;
  $formats->{date}   = JSON::Validator::Formats->can('check_date');
  $formats->{double} = sub { _match_number(double => $_[0], '') };
  $formats->{float}  = sub { _match_number(float => $_[0], '') };
  $formats->{int32}  = sub { _match_number(int32 => $_[0], 'l') };
  $formats->{int64}
    = sub { _match_number(int64 => $_[0], IV_SIZE >= 8 ? 'q' : '') };
  $formats->{password} = sub {undef};

  return $formats;
}

sub _coerce_by_collection_format {
  my ($self, $data, $p) = @_;
  return $data unless $p->{collectionFormat};

  my $schema = $p->{schema} || $p;
  my $type
    = ($schema->{items} ? $schema->{items}{type} : $schema->{type}) || '';

  if ($p->{collectionFormat} eq 'multi') {
    $data  = [$data] unless ref $data eq 'ARRAY';
    @$data = map { $_ + 0 } @$data if $type eq 'integer' or $type eq 'number';
    return $data;
  }

  my $re     = $COLLECTION_RE{$p->{collectionFormat}} || ',';
  my $single = ref $data eq 'ARRAY' ? 0 : ($data = [$data]);

  for my $i (0 .. @$data - 1) {
    my @d = split /$re/, ($data->[$i] // '');
    $data->[$i]
      = ($type eq 'integer' or $type eq 'number') ? [map { $_ + 0 } @d] : \@d;
  }

  return $single ? $data->[0] : $data;
}

sub _get_default_value_from_parameter {
  my ($self, $p) = @_;
  return ($p->{schema}{default}, 1)
    if $p->{schema} and exists $p->{schema}{default};
  return ($p->{default}, 1) if exists $p->{default};
  return (undef,         0);
}

sub _match_byte_string {
  $_[0] =~ /^[A-Za-z0-9\+\/\=]+$/ ? undef : 'Does not match byte format.';
}

sub _match_number {
  my ($name, $val, $format) = @_;
  return 'Does not look like an integer'
    if $name =~ m!^int! and $val !~ /^-?\d+(\.\d+)?$/;
  return 'Does not look like a number.' unless looks_like_number $val;
  return undef unless $format;
  return undef if $val eq unpack $format, pack $format, $val;
  return "Does not match $name format.";
}

sub _resolve_ref {
  my ($self, $topic, $url) = @_;
  $topic->{'$ref'} = "#/definitions/$topic->{'$ref'}"
    if $topic->{'$ref'} =~ /^\w+$/;
  return $self->SUPER::_resolve_ref($topic, $url);
}

sub _response_schema {
  my ($self, $schema) = @_;
  return {
    description => 'Default Mojolicious::Plugin::OpenAPI response.',
    schema      => $schema
  };
}

sub _sub_schemas         { shift->data->{definitions} ||= {} }
sub _sub_schemas_pointer {'#/definitions'}

sub _validate_request_value {
  my ($self, $p, $name, $value) = @_;
  my $type = $p->{type} || 'object';
  my @e;

  return if !defined $value and !$p->{required};

  my $in     = $p->{in} // 'body';
  my $schema = {
    properties => {$name => $p->{'x-json-schema'} || $p->{schema} || $p},
    required   => [$p->{required} ? ($name) : ()]
  };

  if ($in eq 'body') {
    warn "[OpenAPI] Validate $in $name\n" if DEBUG;
    return $self->_validate({$name => $value}, '', $schema);
  }
  elsif (defined $value) {
    warn "[OpenAPI] Validate $in $name=$value\n" if DEBUG;
    return $self->_validate({$name => $value}, '', $schema);
  }
  else {
    warn "[OpenAPI] Validate $in $name=undef\n" if DEBUG;
    return $self->_validate({$name => $value}, '', $schema);
  }

  return;
}

sub _validate_response_headers {
  my ($self, $c, $schema) = @_;
  my $input = $c->openapi->get_response_data('header');
  my @errors;

  for my $name (keys %$schema) {
    my $p = $schema->{$name};

    # jhthorsen: I think that the only way to make a header required,
    # is by defining "array" and "minItems" >= 1.
    if ($p->{type} eq 'array') {
      push @errors, $self->_validate($input->{$name}, '', $p);
    }
    elsif ($input->{$name}) {
      push @errors, $self->_validate($input->{$name}[0], '', $p);
      $c->openapi->set_response_data(
        header => $name => $input->{$name}[0] ? 'true' : 'false')
        if $p->{type} eq 'boolean' and !@errors;
    }
  }

  return @errors;
}

sub _validate_type_array {
  my ($self, $data, $path, $schema) = @_;

  if (ref $schema->{items} eq 'HASH'
    and ($schema->{items}{type} || '') eq 'array')
  {
    $data = $self->_coerce_by_collection_format($data, $schema->{items});
  }

  return $self->SUPER::_validate_type_array($data, $path, $schema);
}

sub _validate_type_file {
  my ($self, $data, $path, $schema) = @_;

  return unless $schema->{required} and (not defined $data or not length $data);
  return E $path => 'Missing property.';
}

sub _validate_type_object {
  my ($self, $data, $path, $schema) = @_;
  return shift->SUPER::_validate_type_object(@_) unless ref $data eq 'HASH';
  return shift->SUPER::_validate_type_object(@_)
    unless $self->{validate_request};

  my (@errors, %ro);
  for my $name (keys %{$schema->{properties} || {}}) {
    next unless $schema->{properties}{$name}{readOnly};
    push @errors, E "$path/$name", "Read-only." if exists $data->{$name};
    $ro{$name} = 1;
  }

  my $discriminator = $schema->{discriminator};
  if ($discriminator and !$self->{inside_discriminator}) {
    return E $path, "Discriminator $discriminator has no value."
      unless my $name = $data->{$discriminator};
    return E $path, "No definition for discriminator $name."
      unless my $dschema = $self->{root}->get("/definitions/$name");
    local $self->{inside_discriminator} = 1;    # prevent recursion
    return $self->_validate($data, $path, $dschema);
  }

  local $schema->{required} = [grep { !$ro{$_} } @{$schema->{required} || []}];

  return @errors, $self->SUPER::_validate_type_object($data, $path, $schema);
}

1;

=encoding utf8

=head1 NAME

JSON::Validator::Schema::OpenAPIv2 - OpenAPI version 2 / Swagger

=head1 SYNOPSIS

  use JSON::Validator::Schema::OpenAPIv2;
  my $schema = JSON::Validator::Schema::OpenAPIv2->new({...});

  # Validate request against a sub schema
  my $sub_schema = $schema->get("/paths/whatever/get");
  my @errors = $schema->validate_request($c, $sub_schema);
  if (@errors) return $c->render(json => {errors => \@errors}, status => 400);

  # Do your logic inside the controller
  my $res = $c->model->get_stuff;

  # Validate response against a sub schema
  @errors = $schema->validate_response($c, $sub_schema, 200, $res);
  if (@errors) return $c->render(json => {errors => \@errors}, status => 500);

  return $c->render(json => $res);

See L<Mojolicious::Plugin::OpenAPI> for a simpler way of using
L<JSON::Validator::Schema::OpenAPIv2>.

=head1 DESCRIPTION

This class represents L<http://swagger.io/v2/schema.json>.

=head1 ATTRIBUTES

=head2 allow_invalid_ref

  $bool   = $schema->allow_invalid_ref;
  $schema = $schema->allow_invalid_ref(1);

Setting this attribute to a true value, will resolve all the "$ref"s inside the
schema before it is set in L</data>. This can be useful if you don't want to be
restricted by the shortcomings of the OpenAPIv2 specification, but still want a
valid schema.

Note however that circular "$ref"s I<are> not supported by this.

=head2 errors

  $array_ref = $schema->errors;

Uses L</specification> to validate L</data> and returns an array-ref of
L<JSON::Validator::Error> objects if L</data> contains an invalid schema.

=head2 formats

  $schema   = $schema->formats({});
  $hash_ref = $schema->formats;

Open API support the following formats in addition to the formats defined in
L<JSON::Validator::Schema::Draft4>:

=over 4

=item * byte

A padded, base64-encoded string of bytes, encoded with a URL and filename safe
alphabet. Defined by RFC4648.

=item * date

An RFC3339 date in the format YYYY-MM-DD

=item * double

Cannot test double values with higher precision then what
the "number" type already provides.

=item * float

Will always be true if the input is a number, meaning there is no difference
between  L</float> and L</double>. Patches are welcome.

=item * int32

A signed 32 bit integer.

=item * int64

A signed 64 bit integer. Note: This check is only available if Perl is
compiled to use 64 bit integers.

=back

=head2 specification

  my $str    = $schema->specification;
  my $schema = $schema->specification($str);

Defaults to "L<http://swagger.io/v2/schema.json>".

=head1 METHODS

=head2 base_url

  $schema = $schema->base_url("https://example.com/api");
  $schema = $schema->base_url(Mojo::URL->new("https://example.com/api"));
  $url    = $schema->base_url;

Can either retrieve or set the base URL for this schema. This method will
construct the C<$url> from "/schemes/0", "/host" and "/basePath" in the schema
or set all or some of those attributes from the input URL.

=head2 data

Same as L<JSON::Validator::Schema/data>, but will bundle the schema if
L</allow_invalid_ref> is set, and also change "/data/info/version" if
L</version_from_class> is set.

=head2 default_response_schema

  $hash_ref = $schema->default_response_schema;

Will return the structure of the default response schema used by
L<Mojolicious::Plugin::OpenAPI> or the like.

=head2 ensure_default_response

  $schema = $schema->ensure_default_response({codes => [400, 500], name => "DefaultResponse"});
  $schema = $schema->ensure_default_response;

This method will look through the "responses" definitions in the schema and add
response definitions, unless already defined. The default schema will allow
responses like this:

  {"errors":[{"message:"..."}]}
  {"errors":[{"message:"...","path":"/foo"}]}

=head2 validate_request

  my @errors = $schema->validate_request($c, $sub_schema);

Used to validate a web request, based on C<$sub_schema>. The C<$c> (controller object)
need to support this API:

  $c->openapi->get_request_data($in);
  $c->openapi->set_request_data($in, $name => $value);

=head2 validate_response

  my @errors = $schema->validate_response($c, $sub_schema, $status_code, $data);

Used to validate a web response, based on C<$sub_schema>. The C<$c> (controller object)
need to support this API:

  $c->openapi->get_response_data($in);
  $c->openapi->set_response_data($in, $name => $value);

=head2 version_from_class

  my $str    = $schema->version_from_class;
  my $schema = $schema->version_from_class("My::App");

The class name (if present) will be used to set "/data/info/version" inside the
schame stored in L</data>.

=head1 SEE ALSO

L<JSON::Validator>, L<Mojolicious::Plugin::OpenAPI>,
L<http://openapi-specification-visual-documentation.apihandyman.io/>

=cut
