# Для разрешения upload-а нужно в директорию со скриптом поместить файл с 
# именем ".can_upload". В противном случае upload запрещаеся.
# Если же этот файл задан, то делается попытка прочитать из него параметры:
#	dir=имя_директории для закачки
#	maxsize=максимальный_размер закачиваемого файла
# В любом случае, по окончание работы скрипта закачанные файлы удаляются 
# (если только они не были перемещены скриптом в другое место).

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


####################### Константы, управляющие работой #####################
$CANUPL_FILE=".can_upload";  # имя файла, разрешающего закачку
$MULTICHUNK_SIZE = 20000;    # длина блока считывания STDIN-а
$uniq_tempnam = 0;

# Настройки сериализации.
# Некоторые внутренние настроечные переменные. Фактически, они используются 
# в качестве констант. Лучше всего их никогда не трогать. Эти константы 
# должны состоять из одного символа!
$Div1=".";		# ALWAYS should be one nondigit!!!
$Div2=".";		# may be the same as $Div1


####################### Преременные с данными браузера #####################
%IN=();                          # Данные формы	
%GET=();                         # Данные GET
%POST=();                        # Данные POST
%COOKIES=();                     # Все пришедшие Cookies
%IMPORT_MOD=();                  # Модули, затребовавшие импорт переменных (ключи)


############################# Стартовая часть ##############################
# Сначала GET
if($ENV{QUERY_STRING}) {
	ParseURLEnc($ENV{QUERY_STRING},"get"); 
}
# Потом POST
if(uc($ENV{REQUEST_METHOD}) eq "POST") {
	if(exists($ENV{'CONTENT_TYPE'}) && $ENV{'CONTENT_TYPE'}=~m|^multipart/form-data|i) {
		ParseMultipart();
	} else {
		read(STDIN,my $data,$ENV{CONTENT_LENGTH});
		ParseURLEnc($data,"post");
	}
}
# И, наконец, Cookies
if($ENV{HTTP_COOKIE} || $ENV{COOKIE}) {
	&ParseCookies();
}


# Кодирование и декодирование.
sub URLEncode { my ($s)=@_; $s=~s/([^;\/?:@&=+\$,A-Za-z0-9\-_.!~*'()])/sprintf("%%%02X",ord $1)/sg; return $s }
sub URLDecode {	my ($s)=@_; $s=~tr/+/ /; $s=~s/%([0-9A-Fa-f]{2})/chr(hex($1))/esg; return $s }


sub export_vars
{	my ($opt,$to)=@_;
	if(!scalar(@_)) {
		# Вызов без параметров - обойти и экспортировать во все модули-клиенты
		while(my ($mod,$opt)=each(%IMPORT_MOD)) {
			export_vars($opt,$mod);
		}
	} else {
		# Вызов с параметрами - экспорт переменных только в укакзанный модуль
		return if !$opt;
		$opt="gpces" if lc($opt) eq "a" || $opt eq "1";
		# Сохраняем информацию о том, что модуль "хочет" экспортирования и 
		# в дальнейшем. Например, при вызове SetCookie() соответствующая 
		# переменная создастся не только в %COOKIES, но и во всех модулях.
		$IMPORT_MOD{$to}=$opt;
		# Экспортируем еще не существующие переменные
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


# Функция вызывается при вызове use
sub import
{	my ($pkg,$opt)=@_;
	export_vars($opt,caller());
	CGI::WebIn->export_to_level(1);
}

# Удаляет временные файлы.
sub END
{	map { unlink($_) } @TempFiles if @TempFiles;
}


# void ProcessPar(straing $key, string $value, string $type)
# Добавляет пару $key=>$value в хэш %IN (с разбором многоуровневых хэшей),
# а также в хэш %GET, %POST или %COOKIES, в зависимости от значения $type 
# (get, post, cookies соответственно).
# Пустые скобки "{}" заменяются на значение "{$v}"!
sub ProcessPar($$$)
{	my ($k,$v,$type)=@_;
	return if !defined($k);
	$v=~s/\r//g if defined $v && !ref $v;
	
	# Получаем все индексы, обрамленные соотв. скобками
	my @Ind=();
	return if $k!~/^([^}{\[\]]+)(.*)$/;
	# Первое имя
	push(@Ind,"{$1}"); 
	$k=$2;
	# Получаем остальные индексы
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
			# Убираем слэши перед кавычками
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
# Возвращает уникальное (используется PID и таймер) имя файла в директории, указанной в 
# параметрах. По умолчанию - в директории, указанной в переменной окружения TMP или TEMP,
# или, в крайнем случае, в текущей. В конце работы скрипта все файлы, имеющие
# имена, сгенерированные tempnam(), будут удалены!
# Всегда возвращает полный путь к временному файлу.
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

# Обработка Multipart-данных формы
# void ParseMultipart()
sub ParseMultipart
{	# Устанавливаем директорию и другие параметры для закачки (если разрешена)
	my %UploadProp=();	# свойства закачки
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
	#------- Работа с STDIN с возможностью "запихивания" данных обратно в поток
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
	#-------- Конец внутренних функций
	
	binmode(STDIN);
# Сначала читаем разделитель и финальные "\r\n"
	my ($bound,$CRLF) = _ReadInput()=~/(^[^\r\n]*)([\r\n]*)/; # Выделяем разделитель БЕЗ \n
	_PutBack($'); # Остаток запихиваем обратно
# Теперь читаем записи, завершенные разделителем
	while((my $Data=_ReadInput()) ne "") {
		if(substr($Data,0,2) eq "--") { last; }	# Проверяем, не конец ли это
	# Получаем заголовок записи в %Headers
		$Data=~/^[$CRLF]*(.*?)$CRLF$CRLF/s;		# выделяем строки заголовка
		_PutBack($');	 								# Остаток запихиваем обратно
		my @Lines=split(/$CRLF/,$1);				# строки заголовка
		my %Headers=();
		foreach my $st (@Lines) {
			my ($k,$v)=split(/: */,$st,2);
			$Headers{lc($k)}=$v;
		}
		if(!%Headers) { die "Malformed multipart POST (no header)\n"; }

	# Выделяем имя тэга и имя файла (если задано)
		my ($name)=$Headers{'content-disposition'}=~/ name="?([^\";]*)"?/;
		my ($filename) = $Headers{'content-disposition'}=~/ filename="?([^\";]*)"?/;

	# Если это не закачка, то читаем данные и продолжаем
		if(!defined $filename || $filename eq "")  {
			my ($body,$i);
			for($body=""; ($i=index($body,$bound))<0 && !_IsEof(); ) { $body.=_ReadInput(); }
			if($i<0) { die "Malformed multipart POST (no boundary after body)\n"; }
			_PutBack(substr($body,$i+length($bound)));	# запихиваем остаток назад
			ProcessPar($name,substr($body,0,$i-length($CRLF)),"post");
			next;
		}

	# Иначе это закачка. Записываем временный файл.
		my $temp=defined $UploadProp{dir}? tempnam($UploadProp{dir}):tempnam();
		local *F; open(F,">$temp") or die("Cannot open temporary file $temp"); binmode(F);
		my $Written=0;
		while(1) {
			my $body1=_ReadInput();
			my $body2=_ReadInput();
			my $body=$body1.$body2;
			# Нашли конец файла (разделитель)?
			if(($i=index($body,$bound))>=0) {
				$Written+=$i-length($CRLF);
				print F substr($body,0,$i-length($CRLF));
				_PutBack(substr($body,$i+length($bound)));
				last;
			}
			# Не нашли. Тогда смотрим, не слишком ли велик файл и разрешена ли закачка
			#print "$Written $UploadProp{maxsize}<br>";
			if(%UploadProp && (!defined $UploadProp{maxsize} || $Written<=$UploadProp{maxsize})) {
				# Если не слишком, то дописываем ему в конец
				$Written+=length($body1);
				print F $body1;
			}
			# Иначе не дописываем.
			_PutBack($body2);			
		}
		close(F);
		ProcessPar($name."{filename}",$filename,"post");
		if(!%UploadProp || (defined $UploadProp{maxsize} && $Written>$UploadProp{maxsize})) {
			# Файл слишком большой, либо upload запрещен
			close(F); 
			unlink($temp);
		} else {
			# Иначе все в порядке
			ProcessPar($name."{headers}",\%Headers,"post");
			ProcessPar($name."{file}",$temp,"post");
			ProcessPar($name."{size}",(-s $temp),"post");
			if($Headers{'content-type'}) { ProcessPar($name."{type}",$Headers{'content-type'},"post"); }
		}
	}
}


# Разбирает пришедшие cookies
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
# Устанавливает cookie с именем $name и значение $value ($value может быть сложным объектом
# - в частности, ссылкой на массив или хэш).
# Если $value не задан (undef), cookie удаляется.
# Если $expire не задан, время жизни становится бесконечным. Если задан, но равен 
# нулю - создается one-session cookie.
# Параметр $expire можно задавать в виде, который "понимает" функция ExpireCalc().
sub SetCookie($;$;$;$;$;$)
{	my ($name,$value,$expires,$path,$domain,$secure)=@_;
	my $NeedDel=0;

#	[12.03.2002] Можно и без этого.
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
		# Если не вышло загрузить CGI::WebOut, то просто печатаем.
		print $cook;
	} else {
		CGI::WebOut::Header($cook);
	}
	if(!$NeedDel) { ProcessPar($name,$value,"cookie"); } 
		else { ProcessPar($name,undef,"cookie"); }
	# Экспортируем Cookie во все нужные модули
	export_vars();
}


# void DropCookie(string $name [,$path] [,$domain])
# Удаляет cookie с именем $name. Параметры $path и $domain 
# должны точно совпадать с теми, которые были заданы при 
# установке Cookie.
sub DropCookie($;$;$)
{	my ($name,$path,$domain)=@_;
	SetCookie($name,undef,undef,$path,$domain);
}


# string Serialize(mixed @args)
# Упаковывает в строку любой (практически) объект. Так что не обязательно передавать
# этой функции ссылку - можно прямо объект целиком.
# Нельзя упаковывать объекты, содержащие ссылки на функции и дескрипторы файлов.
# В случае ошибки возвращает undef и выводит warning.
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
# Распаковывает строку, созданную ранее при помощи Serialize(). Возвращает то, что
# было когда-то передано в параметрах Serialize.
# В случае ошибки возвращает undef и выдает warning.
sub _Unserialize($;$)
{	my ($st,$TotalLen)=@_;
	# Считаем число ссылок
	my $RefCount; 
	for($RefCount=0; substr($st,$RefCount,1) eq "r"; $RefCount++) {;}
	$$TotalLen+=$RefCount; $st=substr($st,$RefCount);
	# Определяем тип
	my $Type="S";   # Может быть еще: "HL" (да, 2 символа!!!) или "L" 
	if(substr($st,0,1) eq "H") { $Type="H"; $st=substr($st,2); $$TotalLen+=2; }
		elsif(substr($st,0,1) eq "L") { $Type="L"; $$TotalLen++; $st=substr($st,1); }
	# Выполняем действия в зваисимости от типа	
	my $PResult;
	if($Type eq "S") {
		# Это - обычная строка.
		my $len=substr($st,0,my $p=index($st,$Div1));  # 0123.aaabbb
		$st=substr($st,$p+1); $$TotalLen+=$p+1+$len;   # ^   ^p
		# Распаковываем исходную строку
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
