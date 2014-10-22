# ��� ���������� upload-� ����� � ���������� �� �������� ��������� ���� � 
# ������ ".can_upload". � ��������� ������ upload ����������.
# ���� �� ���� ���� �����, �� �������� ������� ��������� �� ���� ���������:
#	dir=���_���������� ��� �������
#	maxsize=������������_������ ������������� �����
# � ����� ������, �� ��������� ������ ������� ���������� ����� ��������� 
# (���� ������ ��� �� ���� ���������� �������� � ������ �����).

package CGI::WebIn;
#use strict;
use Exporter; 
our $VERSION = '1.51';
our @ISA=qw(Exporter);
our @EXPORT=qw(
	%IN 
	%GET 
	%POST 
	%COOKIES 
	SetCookie 
	DropCookie
);


####################### ���������, ����������� ������� #####################
$CANUPL_FILE=".can_upload";  # ��� �����, ������������ �������
$MULTICHUNK_SIZE = 20000;    # ����� ����� ���������� STDIN-�
$uniq_tempnam = 0;

# ��������� ������������.
# ��������� ���������� ����������� ����������. ����������, ��� ������������ 
# � �������� ��������. ����� ����� �� ������� �� �������. ��� ��������� 
# ������ �������� �� ������ �������!
$Div1=".";		# ALWAYS should be one nondigit!!!
$Div2=".";		# may be the same as $Div1


####################### ����������� � ������� �������� #####################
%IN=();                          # ������ �����	
%GET=();                         # ������ GET
%POST=();                        # ������ POST
%COOKIES=();                     # ��� ��������� Cookies
%IMPORT_MOD=();                  # ������, ������������� ������ ���������� (�����)


############################# ��������� ����� ##############################
# ������� GET
if($ENV{QUERY_STRING}) {
	ParseURLEnc($ENV{QUERY_STRING},"get"); 
}
# ����� POST
if(uc($ENV{REQUEST_METHOD}) eq "POST") {
	if(exists($ENV{'CONTENT_TYPE'}) && $ENV{'CONTENT_TYPE'}=~m|^multipart/form-data|i) {
		ParseMultipart();
	} else {
		read(STDIN,my $data,$ENV{CONTENT_LENGTH});
		ParseURLEnc($data,"post");
	}
}
# �, �������, Cookies
if($ENV{HTTP_COOKIE} || $ENV{COOKIE}) {
	&ParseCookies();
}


# ����������� � �������������.
sub URLEncode { my ($s)=@_; $s=~s/([^;\/?:@&=+\$,A-Za-z0-9\-_.!~*'()])/sprintf("%%%02X",ord $1)/sg; return $s }
sub URLDecode {	my ($s)=@_; $s=~tr/+/ /; $s=~s/%([0-9A-Fa-f]{2})/chr(hex($1))/esg; return $s }


sub export_vars
{	my ($opt,$to)=@_;
	if(!scalar(@_)) {
		# ����� ��� ���������� - ������ � �������������� �� ��� ������-�������
		while(my ($mod,$opt)=each(%IMPORT_MOD)) {
			export_vars($opt,$mod);
		}
	} else {
		# ����� � ����������� - ������� ���������� ������ � ���������� ������
		return if !$opt;
		$opt="gpces" if lc($opt) eq "a" || $opt eq "1";
		# ��������� ���������� � ���, ��� ������ "�����" ��������������� � 
		# � ����������. ��������, ��� ������ SetCookie() ��������������� 
		# ���������� ��������� �� ������ � %COOKIES, �� � �� ���� �������.
		$IMPORT_MOD{$to}=$opt;
		# ������������ ��� �� ������������ ����������
		my $Bad=\%{$to."::"};
		foreach my $op (split //,$opt) {
			$op=lc($op);
			my $Hash = 
				$op eq "g" && \%GET ||
				$op eq "p" && \%POST ||
				$op eq "c" && \%COOKIES ||
				$op eq "e" && \%ENV || next;
			while(my ($k,$v)=each(%$Hash)) {
				next if exists $Bad{$k};
				*{$to."::".$k}=ref($v)? $Hash->{$k} : \$Hash->{$k};	
			}
		}
	}
}


# ������� ���������� ��� ������ use
sub import
{	my ($pkg,$opt)=@_;
	export_vars($opt,caller());
	CGI::WebIn->export_to_level(1);
}

# ������� ��������� �����.
sub END
{	map { unlink($_) } @TempFiles if @TempFiles;
}


# void ProcessPar(straing $key, string $value, string $type)
# ��������� ���� $key=>$value � ��� %IN (� �������� �������������� �����),
# � ����� � ��� %GET, %POST ��� %COOKIES, � ����������� �� �������� $type 
# (get, post, cookies ��������������).
# ������ ������ "{}" ���������� �� �������� "{$v}"!
sub ProcessPar($$$)
{	my ($k,$v,$type)=@_;
	return if !defined($k);
	$v=~s/\r//g if defined $v && !ref $v;
	
	# �������� ��� �������, ����������� �����. ��������
	my @Ind=();
	return if $k!~/^([^}{\[\]]+)(.*)$/;
	# ������ ���
	push(@Ind,"{$1}"); 
	$k=$2;
	# �������� ��������� �������
	while(1) {
		last if 
			$k!~/^
				\[
					(?:
						[^]"']* |
						"(?:[^"]+|\\")*" |
						'(?:[^']+|\\')*'
					)*
				\]
					|
				\{
					(?:
						[^}"']* |
						"(?:[^"]+|\\")*" |
						'(?:[^']+|\\')*'
					)*
				\}	
			/sx; 
		push(@Ind,$&); 
		$k=$';
	}

	my @Outs=(\%IN);
	push(@Outs,\%GET) if lc($type) eq "get";
	push(@Outs,\%POST) if lc($type) eq "post";
	push(@Outs,\%COOKIES) if lc($type) eq "cookie";
	foreach my $cur (@Outs) {
		for(my $i=0; $i<=$#Ind; $i++) {
			my $ind=substr($Ind[$i],1,-1);
			# ������� ����� ����� ���������
			$ind=~s/^(['"])(.*)\1$/$2/sg;
			$ind=~s/\\(['"])/$1/sg;
			my $type=substr($Ind[$i],0,1);
			my $nexttype=substr(defined $Ind[$i+1]?$Ind[$i+1]:"",0,1);
			my $lastelem=undef;
			my $nextelem=($nexttype eq "{")?{}:($nexttype eq "[")?[]:($lastelem=$v);
			if($type eq "{") {
				$ind=$v if $ind eq "" && defined $v;
				if(ref($cur) ne "HASH") { last; }
				if(!defined $cur->{$ind} || ref($cur->{$ind}) ne ref($nextelem) || !ref($nextelem) || defined $lastelem) { $cur->{$ind}=$nextelem }
				$cur=$cur->{$ind};
			} elsif($type eq "[") {
				if(ref($cur) ne "ARRAY") { last; }
				if(!defined $cur->[$ind] || ref($cur->[$ind]) ne ref($nextelem) || defined $lastelem) { $cur->[$ind]=$nextelem }
				$cur=$cur->[$ind];			
			}
		}
	}
#	use Profiler; PrintDump("IN",\%IN);
}

use AutoLoader 'AUTOLOAD';
return 1;
__END__


# void ParseURLEnc(string $input, string $type)
sub ParseURLEnc
{	my ($tosplit,$type) = @_;
	my (@pairs) = split(/[&?]/,$tosplit);
	my ($param,$value);
	foreach (@pairs) {
		($param,$value) = split('=',$_,2);
		$param = URLDecode(defined($param)?$param:"");
		$value = URLDecode(defined($value)?$value:"");
		ProcessPar($param,$value,$type);
	}
}


# string tempnam([string $Dir])
# ���������� ���������� (������������ PID � ������) ��� ����� � ����������, ��������� � 
# ����������. �� ��������� - � ����������, ��������� � ���������� ��������� TMP ��� TEMP,
# ���, � ������� ������, � �������. � ����� ������ ������� ��� �����, �������
# �����, ��������������� tempnam(), ����� �������!
# ������ ���������� ������ ���� � ���������� �����.
sub tempnam(;$)
{	my ($dir)=@_; 
	foreach my $cd ($dir,$ENV{TMP},$ENV{TEMP},"/tmp",".") {
		if(defined $cd && -d $cd && -w $cd) { $dir=$cd; last; }
	}
	my $nm=$dir."/".time()."-$$-".(++$uniq_tempnam).".tmp";
	if($nm!~m{^[/\\]}) {
		require Cwd;
		$nm=Cwd::getcwd()."/".$nm;
	}
	push(@TempFiles,$nm);
	return $nm;
}

# ��������� Multipart-������ �����
# void ParseMultipart()
sub ParseMultipart
{	# ������������� ���������� � ������ ��������� ��� ������� (���� ���������)
	my %UploadProp=();	# �������� �������
	if(-e $CANUPL_FILE) {
		local *F; open(F,"<$CANUPL_FILE"); 
		while(my $st=<F>) {
			$st=~s/^\s+|\s+$|#.*$//gs;
			next if $st eq "";
			my ($k,$v)=split(/=/,$st,2);
			$UploadProp{$k}=$v;
		}
		close(F);
	}
	#------- ������ � STDIN � ������������ "�����������" ������ ������� � �����
	local $InBuf=""; local $InLength=$ENV{CONTENT_LENGTH};
	sub _ReadInput
	{	my $need=$MULTICHUNK_SIZE>$InLength?$InLength:$MULTICHUNK_SIZE;
		my $nBuf=length($InBuf)<$need?length($InBuf):$need;
		my $out=substr($InBuf,0,$nBuf); $InBuf=substr($InBuf,$nBuf);
		read(STDIN,$out,$need-$nBuf,$nBuf);
		$InLength-=length($out);
		return $out;
	} 
	sub _PutBack 
	{	my ($data)=@_;
		$InBuf=$data.$InBuf;
		$InLength+=length($data);
	}
	sub _IsEof { return !$InLength; }
	#-------- ����� ���������� �������
	
	binmode(STDIN);
# ������� ������ ����������� � ��������� "\r\n"
	my ($bound,$CRLF) = _ReadInput()=~/(^[^\r\n]*)([\r\n]*)/; # �������� ����������� ��� \n
	_PutBack($'); # ������� ���������� �������
# ������ ������ ������, ����������� ������������
	while((my $Data=_ReadInput()) ne "") {
		if(substr($Data,0,2) eq "--") { last; }	# ���������, �� ����� �� ���
	# �������� ��������� ������ � %Headers
		$Data=~/^[$CRLF]*(.*?)$CRLF$CRLF/s;		# �������� ������ ���������
		_PutBack($');	 								# ������� ���������� �������
		my @Lines=split(/$CRLF/,$1);				# ������ ���������
		my %Headers=();
		foreach my $st (@Lines) {
			my ($k,$v)=split(/: */,$st,2);
			$Headers{lc($k)}=$v;
		}
		if(!%Headers) { die "Malformed multipart POST (no header)\n"; }

	# �������� ��� ���� � ��� ����� (���� ������)
		my ($name)=$Headers{'content-disposition'}=~/ name="?([^\";]*)"?/;
		my ($filename) = $Headers{'content-disposition'}=~/ filename="?([^\";]*)"?/;

	# ���� ��� �� �������, �� ������ ������ � ����������
		if(!defined $filename || $filename eq "")  {
			my ($body,$i);
			for($body=""; ($i=index($body,$bound))<0 && !_IsEof(); ) { $body.=_ReadInput(); }
			if($i<0) { die "Malformed multipart POST (no boundary after body)\n"; }
			_PutBack(substr($body,$i+length($bound)));	# ���������� ������� �����
			ProcessPar($name,substr($body,0,$i-length($CRLF)),"post");
			next;
		}

	# ����� ��� �������. ���������� ��������� ����.
		my $temp=defined $UploadProp{dir}? tempnam($UploadProp{dir}):tempnam();
		local *F; open(F,">$temp") or die("Cannot open temporary file $temp"); binmode(F);
		my $Written=0;
		while(1) {
			my $body1=_ReadInput();
			my $body2=_ReadInput();
			my $body=$body1.$body2;
			# ����� ����� ����� (�����������)?
			if(($i=index($body,$bound))>=0) {
				$Written+=$i-length($CRLF);
				print F substr($body,0,$i-length($CRLF));
				_PutBack(substr($body,$i+length($bound)));
				last;
			}
			# �� �����. ����� �������, �� ������� �� ����� ���� � ��������� �� �������
			#print "$Written $UploadProp{maxsize}<br>";
			if(%UploadProp && (!defined $UploadProp{maxsize} || $Written<=$UploadProp{maxsize})) {
				# ���� �� �������, �� ���������� ��� � �����
				$Written+=length($body1);
				print F $body1;
			}
			# ����� �� ����������.
			_PutBack($body2);			
		}
		close(F);
		ProcessPar($name."{filename}",$filename,"post");
		if(!%UploadProp || (defined $UploadProp{maxsize} && $Written>$UploadProp{maxsize})) {
			# ���� ������� �������, ���� upload ��������
			close(F); 
			unlink($temp);
		} else {
			# ����� ��� � �������
			ProcessPar($name."{headers}",\%Headers,"post");
			ProcessPar($name."{file}",$temp,"post");
			ProcessPar($name."{size}",(-s $temp),"post");
			if($Headers{'content-type'}) { ProcessPar($name."{type}",$Headers{'content-type'},"post"); }
		}
	}
}


# ��������� ��������� cookies
sub ParseCookies
{	my @Pairs = split("; *",$ENV{HTTP_COOKIE} || $ENV{COOKIE} || "");
	foreach (@Pairs) {
		my ($key,$value);
		if(/^([^=]+)=(.*)/) { $key = $1; $value = $2; } else { $key = $_; $value = ''; }
		$key=URLDecode($key);
		$value=URLDecode($value);
		my $v=Unserialize($value);

#		use Profiler; 
#		PrintDump("Value",$value);
#		PrintDump("V",$v);

		$value=defined($v)?$v:$value;
		ProcessPar($key,$value,"cookie");
	}
}


# int ExpireCalc(string $tm)
# This routine creates an expires time exactly some number of
# hours from the current time.  It incorporates modifications from Mark Fisher.
# Format for time $tm can be in any of the forms...
#	"now" 	-- expire immediately
#	"+180s" 	-- in 180 seconds
#	"+2m" 	-- in 2 minutes
#	"+12h" 	-- in 12 hours
#	"+1d"  	-- in 1 day
#	"+3M"  	-- in 3 months
#	"+2y"  	-- in 2 years
#	"-3m"  	-- 3 minutes ago(!)
sub ExpireCalc($)
{	my($time)=@_;
	my(%mult)=('s'=>1, 'm'=>60, 'h'=>60*60, 'd'=>60*60*24, 'M'=>60*60*24*30, 'y'=>60*60*24*365);
	my($offset);
	if(lc($time) eq 'now') { $offset = 0; } 
	elsif($time=~/^([+-](?:\d+|\d*\.\d*))([mhdMy]?)/) { $offset = ($mult{$2} || 1)*$1; }
	else { return $time; }
	return (time+$offset);
}


# int Expires(int $time, string $format)
# This internal routine creates date strings suitable for use in
# cookies ($format="cookie") and HTTP headers ($format="http" or nothing). 
# (They differ, unfortunately.) Thanks to Fisher Mark for this.
sub Expires($;$)
{	my($time,$format) = @_; $format ||= 'http';
	my(@MON)=qw/Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec/;
	my(@WDAY) = qw/Sun Mon Tue Wed Thu Fri Sat/;
	# pass through preformatted dates for the sake of expire_calc()
	$time = ExpireCalc($time);	return $time unless $time =~ /^\d+$/;
	# cookies use '-' as date separator, HTTP uses ' '
	my($sc) = ' '; $sc = '-' if $format eq "cookie";
	my($sec,$min,$hour,$mday,$mon,$year,$wday) = gmtime($time);
	return sprintf("%s, %02d$sc%s$sc%04d %02d:%02d:%02d GMT",
           $WDAY[$wday],$mday,$MON[$mon],$year+1900,$hour,$min,$sec);
}


# void SetCookie(string $name, string $value [,int $expire][,$path][,$domain][bool $secure])
# ������������� cookie � ������ $name � �������� $value ($value ����� ���� ������� ��������
# - � ���������, ������� �� ������ ��� ���).
# ���� $value �� ����� (undef), cookie ���������.
# ���� $expire �� �����, ����� ����� ���������� �����������. ���� �����, �� ����� 
# ���� - ��������� one-session cookie.
# �������� $expire ����� �������� � ����, ������� "��������" ������� ExpireCalc().
sub SetCookie($;$;$;$;$;$)
{	my ($name,$value,$expires,$path,$domain,$secure)=@_;
	my $NeedDel=0;

#	[12.03.2002] ����� � ��� �����.
#	if(!defined $path) { 
#		$path=$ENV{SCRIPT_NAME};
#		$path=~s{/[^/]*$}{}sg;
#		$path.="/";
#	}

	if(!defined $expires) { $expires="+20y"; }
	if(!defined $value)   { $value=""; $expires="-3y"; $NeedDel=1; }
	
	my @Param;
	push(@Param,URLEncode($name)."=".URLEncode(Serialize($value)));
	push(@Param,"domain=$domain") if defined $domain;
	push(@Param,"path=$path") if defined $path;
	push(@Param,"expires=".Expires($expires,"cookie")) if $expires;
	push(@Param,'secure') if $secure;
	
	my $cook="Set-Cookie: ".join("; ",@Param);
	eval {
		local ($SIG{__WARN__},$SIG{__DIE__})=(sub {}, sub {});
		require CGI::WebOut;
	};
	if($@) {
		# ���� �� ����� ��������� CGI::WebOut, �� ������ ��������.
		print $cook;
	} else {
		CGI::WebOut::Header($cook);
	}
	if(!$NeedDel) { ProcessPar($name,$value,"cookie"); } 
		else { ProcessPar($name,undef,"cookie"); }
	# ������������ Cookie �� ��� ������ ������
	export_vars();
}


# void DropCookie(string $name [,$path] [,$domain])
# ������� cookie � ������ $name. ��������� $path � $domain 
# ������ ����� ��������� � ����, ������� ���� ������ ��� 
# ��������� Cookie.
sub DropCookie($;$;$)
{	my ($name,$path,$domain)=@_;
	SetCookie($name,undef,undef,$path,$domain);
}


# string Serialize(mixed @args)
# ����������� � ������ ����� (�����������) ������. ��� ��� �� ����������� ����������
# ���� ������� ������ - ����� ����� ������ �������.
# ������ ����������� �������, ���������� ������ �� ������� � ����������� ������.
# � ������ ������ ���������� undef � ������� warning.
sub Serialize
{	my $st="L".($#_+1).$Div2;
	foreach my $Arg (@_) {
		while((my $Ref=ref($Arg)) eq "REF") { $st.="r"; $Arg=$$Arg; }
		if(ref($Arg) ne "") { $st.="r"; }
		if(ref($Arg) eq "") { $st.=length($Arg).$Div1.$Arg; }
		elsif(ref($Arg) eq "SCALAR") { $st.=length($$Arg).$Div1.$$Arg; }
		elsif(ref($Arg) eq "ARRAY") { $st.=Serialize(@$Arg); }
		elsif(ref($Arg) eq "HASH") { $st.="H".Serialize(%$Arg); }
		else { warn("Serialize: invalid field type '".ref($Arg)."'"); return undef; }
	}
	return $st;
}


# mixed _Unserialize(string $st)
# ������������� ������, ��������� ����� ��� ������ Serialize(). ���������� ��, ���
# ���� �����-�� �������� � ���������� Serialize.
# � ������ ������ ���������� undef � ������ warning.
sub _Unserialize($;$)
{	my ($st,$TotalLen)=@_;
	# ������� ����� ������
	my $RefCount; 
	for($RefCount=0; substr($st,$RefCount,1) eq "r"; $RefCount++) {;}
	$$TotalLen+=$RefCount; $st=substr($st,$RefCount);
	# ���������� ���
	my $Type="S";   # ����� ���� ���: "HL" (��, 2 �������!!!) ��� "L" 
	if(substr($st,0,1) eq "H") { $Type="H"; $st=substr($st,2); $$TotalLen+=2; }
		elsif(substr($st,0,1) eq "L") { $Type="L"; $$TotalLen++; $st=substr($st,1); }
	# ��������� �������� � ����������� �� ����	
	my $PResult;
	if($Type eq "S") {
		# ��� - ������� ������.
		my $len=substr($st,0,my $p=index($st,$Div1));  # 0123.aaabbb
		$st=substr($st,$p+1); $$TotalLen+=$p+1+$len;   # ^   ^p
		# ������������� �������� ������
		my $s=substr($st,0,$len); $PResult=\$s;
	} elsif($Type eq "L" || $Type eq "H") {
		my @Unpack;
		my $size=substr($st,0,my $p=index($st,$Div2));
		$st=substr($st,$p+1); $$TotalLen+=$p+1;
		foreach my $i (0..$size-1) {
			my $len; push(@Unpack,_Unserialize($st,\$len));
			$$TotalLen+=$len;
			$st=substr($st,$len);
		}
		if($Type eq "L") { $PResult=\@Unpack; } else { my %Hash=@Unpack; $PResult=\%Hash; }
	}
	# We have the pointer to the object $PResult. Returning the (n-1)-th reference on it.
	for(my $i=0; $i<$RefCount; $i++) { my $tmp=$PResult; $PResult=\$tmp; }
	if(ref($PResult) eq "ARRAY") { return wantarray?@$PResult:@$PResult[0]; }
		elsif(ref($PResult) eq "HASH") { return %$PResult; }
			else { return $$PResult; }
}


sub Unserialize($;$)
{	return undef if !defined $_[0];
	my @Result=(); my $err=0;
	local $SIG{__WARN__}=sub { $err=1; };
	local $SIG{__DIE__}=sub { $err=1; };
	eval { @Result=_Unserialize($_[0]); };
	if($err||$@) { return undef; }
	return wantarray?@Result:$Result[0];
}

return 1;
__END__


=head1 NAME

CGI::WebIn - Perl extension for reading CGI form data

=head1 SYNOPSIS

  use CGI::WebOut;
  use CGI::WebIn(1);

  # just to avoid "typo warning"
  our ($doGo,%Address,$Count); 

  # count visits
  SetCookie("Count",++$Count,"+10y");

  # is the button pressed?
  if($doGo) {
      print "Hello from $Address{Russia}{Moscow}!";
  }

  print <<EOT;
    You have visited this page $Count times.
    <form action=$SCRIPT_NAME method=post enctype=multipart/form-data>
    <input type=text name="Address{Russia}{Moscow}" value="house">
    <input type=submit name=doGo value="Say hello">
    </form>
  EOT

=head1 FEATURES

=over 5

=item *

Handle multi-dimentional GET/POST form data (see SYNOPSIS).

=item *

Handle multipart forms for file uploads.

=item *

Full real-time multi-dimentional cookie support.

=item *

PHP-style form data exporting.

=item *

Fast URL encode/decode subroutines.

=back


=head1 OVERVIEW

This module is used to make CGI programmer's work more comfortable. 
The main idea is to handle input stream (C<STDIN>) and C<QUERY_STRING>
environment variable sent by browser and parse their correctly
(including multipart forms). Resulting variables are put to C<%GET>, 
C<%POST>, C<%COOKIES> and C<%IN> (C<%IN> holds ALL the data). Also 
allows you to get/set cookies (any structure, not only scalars!) with 
C<SetCookie()> subroutine.

If this module is included without any arguments:

  use CGI::WebIn;

it exports the following: C<%IN>, C<%GET>, C<%POST>, C<%COOKIES>, 
C<SetCookie()> and C<DropCookie()>

You can specify additional information to be exported by using 
include arguments:

  use CGI::WebIn 'gpce';

means that all the GET, POST, Cookies and then environment 
variables will be exported to "usual" package variables. 
You must not be afraid to write everywhere C<'gpce'> - the 
following instruction does the same:

  use CGI::WebIn 'gpce';

=head1 DESCRIPTION

=over 5

=item C<use CGI::WebIn(1)>

Reads all the CGI input and exports it to the caller module
(like PHP does).

=item C<%IN, %GET, %POST and %COOKIES>

C<%IN> contains all the form data. C<%GET>, C<%POST> and C<%COOKIES> 
holds GET, POST and Cookies variables respectively.

=item C<void SetCookie($name, $value [,int $expire][,$path][,$domain][bool $secure])>

Sets the cookie in user browser. Value of that cookie is placed to C<%COOKIES> 
and ALL exported client modules immediately. Format for time C<$expire> can be 
in any of the forms:

  <stamp> - UNIX timestamp
  0       - one-session cookie
  undef   - drop this cookie
  "now"   - expire immediately
  "+180s" - in 180 seconds
  "+2m"   - in 2 minutes
  "+12h"  - in 12 hours
  "+1d"   - in 1 day
  "+3M"   - in 3 months
  "+2y"   - in 2 years
  "-3m"   - 3 minutes ago(!)

=item C<void DropCookie(string $name [,string $path] [,string $domain])>

Destroys the specified cookie. Make sure the C<$path> and C<$domain> parameters are 
the same to previous C<SetCookie()> call.

=item C<file uploading support>

To enable file uploading, you must create the following 
file C<.can_upload>:

  # directory to upload user files
  dir = .
  # maximum allowed size of the file to upload
  maxsize = 100000

and place it to the current directory. If there is no C<.can_upload> 
file, uploading is disabled.

=back

=head1 AUTHOR

Dmitry Koteroff <dk@dklab.ru>, http://www.dklab.ru

=head1 SEE ALSO

C<CGI::WebOut>.

=cut
