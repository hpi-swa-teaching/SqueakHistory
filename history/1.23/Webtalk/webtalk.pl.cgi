#!/usr/bin/perl
require 5.001;

#
# webtalk cgi interface
# (c) 1996 by tim jones
#
# based on examples by William E. Weinman and Larry Wall

#################
#
# configuration 
#
#################

$hostname = 'warmspot';
$webtalkServer = 'wotan'; 
$port = 4248;

###############################
#
# connect to smalltalk server
#
###############################

use Socket;

$sockaddr = 'S n a4 x8';
#chop($hostname = `hostname`);

($name, $aliases, $proto) = getprotobyname('tcp');
($name, $aliases, $port) = getservbyname($port, 'tcp') unless $port =~ /^\d+$/;
($name, $aliases, $type, $len, $thisaddr) = gethostbyname($hostname);
($name, $aliases, $type, $len, $thataddr) = gethostbyname($webtalkServer);
 
$this = pack($sockaddr, AF_INET, 0, $thisaddr);
$that = pack($sockaddr, AF_INET, $port, $thataddr);
 
if (!socket(S, PF_INET, SOCK_STREAM, $proto))
{
  $em = "webtalk cgi interface couldn't create socket";
  &quit_with_error;
}

if (!bind(S, $this))
{
  $em = "webtalk cgi interface couldn't bind socket";
  &quit_with_error;
}

if (!connect(S, $that))
{
  $em = "webtalk cgi interface couldn't connect socket";
  &quit_with_error;
}

select(S); $| = 1; select(STDOUT);

######################################################
#
# send all standard CGI variables to the smalltalk server
#
######################################################

print S "~GATEWAY_INTERFACE = $ENV{'GATEWAY_INTERFACE'}";

#print S "~REQUEST_METHOD = $ENV{'REQUEST_METHOD'}";
#print S "~SCRIPT_NAME = $ENV{'SCRIPT_NAME'}";
#print S "~QUERY_STRING = $ENV{'QUERY_STRING'}";

print S "~SERVER_SOFTWARE = $ENV{'SERVER_SOFTWARE'}";
print S "~SERVER_NAME = $ENV{'SERVER_NAME'}";
print S "~SERVER_PROTOCOL = $ENV{'SERVER_PROTOCOL'}";
print S "~SERVER_PORT = $ENV{'SERVER_PORT'}";
print S "~HTTP_USER_AGENT = $ENV{'HTTP_USER_AGENT'}";

#print S "~HTTP_ACCEPT = $ENV{'HTTP_ACCEPT'}";
#print S "~PATH_INFO = $ENV{'PATH_INFO'}";
#print S "~PATH_TRANSLATED = $ENV{'PATH_TRANSLATED'}";

print S "~REMOTE_HOST = $ENV{'REMOTE_HOST'}";
print S "~REMOTE_ADDR = $ENV{'REMOTE_ADDR'}";

#print S "~REMOTE_USER = $ENV{'REMOTE_USER'}";
#print S "~REMOTE_IDENT = $ENV{'REMOTE_IDENT'}";
#print S "~AUTH_TYPE = $ENV{'AUTH_TYPE'}";
#print S "~CONTENT_TYPE = $ENV{'CONTENT_TYPE'}";
#print S "~CONTENT_LENGTH = $ENV{'CONTENT_LENGTH'}";

###################################################
#
#  now handle standard input -
#  parse it, and pass the variables to the smalltalk server
#
###################################################

read(STDIN, $qs, $ENV{"CONTENT_LENGTH"});
&parse_and_send;

###################################################
#
#  now handle the QUERY_STRING environment variable -
#  parse it, and pass the variables to the smalltalk server
#
####################################################

$qs = $ENV{'QUERY_STRING'};
&parse_and_send;

################################################
#
# now get the smalltalk server's html page response 
# and pass it on
#
################################################

print S "&";

print "Content-type: text/html\n\n";

while(<S>) {
	print;
}

exit 0;

###############################################################
#
# subroutine to parse qs and print the variables to the socket
#
################################################################

sub parse_and_send
{
  # split it up into an array by the '&' character
    @array = split(/&/,$qs);

    foreach $i (0 .. $#array) 
    {
	# convert the plus chars to spaces
	$array[$i] =~ s/\+/ /g;

	# convert the wiggle chars to spaces
	#$array[$i] =~ s/~/ /g;

	# convert the hex tokens to characters
	$array[$i] =~ s/%(..)/pack("c",hex($1))/ge;

	# split into name and value
	($name, $value) = split(/=/,$array[$i],2);

	# create the associative element
	$array{$name} = $value;
    }

    foreach $name (sort keys(%array)) 
    { printf S "~$name = %s", $array{$name} }

}

###############################################################
#
# subroutine to abort with an error page
#
################################################################

sub quit_with_error
{
 # Send the MIME header
  print "Content-type: text/plain\n\n";
  print $em;
  exit 1;
}
















