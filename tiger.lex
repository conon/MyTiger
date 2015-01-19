type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult  = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentsClosed = ref true
val stringClosed = ref 0
val str = ref ""

fun esq_to_string esq =
	let val e =
			case esq of
				"n" => #"\n"
			  | "t" => #"\t"
			  | "\\" => #"\\"
			  | "\"" => #"\""
	in String.str(e) end

fun eof() =
    if not (!commentsClosed)
    then (ErrorMsg.error (!lineNum) ("unmatch comment"); Tokens.EOF(!lineNum,!lineNum))
    else if !stringClosed <> 0
    then (ErrorMsg.error (!lineNum) ("unmatch string"); Tokens.EOF(!lineNum,!lineNum))
    else Tokens.EOF(!lineNum,!lineNum)

%%

%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));
%s COMMENT STRING ESC FORMAT;
ID = [A-Za-z][_A-Za-z0-9]*;
digit = [0-9];
oct = {digit}{3};
newline = [\n\r];
ws = [ \t];
esqchar = ([nt\\\"] | "^c");
formatchar = ({newline} | {ws});

%%


<INITIAL, COMMENT>{newline}  => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>{ws}        => (continue());
<INITIAL>"type"      => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>"var"       => (Tokens.VAR(yypos,yypos+3));
<INITIAL>"function"  => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>"break"     => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>"of"        => (Tokens.OF(yypos,yypos+2));
<INITIAL>"end"       => (Tokens.END(yypos,yypos+3));
<INITIAL>"in"        => (Tokens.IN(yypos,yypos+2));
<INITIAL>"nil"       => (Tokens.NIL(yypos,yypos+3));
<INITIAL>"let"       => (Tokens.LET(yypos,yypos+3));
<INITIAL>"do"        => (Tokens.DO(yypos,yypos+2));
<INITIAL>"to"        => (Tokens.TO(yypos,yypos+2));
<INITIAL>"for"       => (Tokens.FOR(yypos,yypos+3));
<INITIAL>"while"     => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>"else"      => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>"then"      => (Tokens.THEN(yypos,yypos+4));
<INITIAL>"if"        => (Tokens.IF(yypos,yypos+2));
<INITIAL>"array"     => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>"/*"        => (YYBEGIN COMMENT; commentsClosed := false; continue());
<COMMENT>"*/"        => (YYBEGIN INITIAL; commentsClosed := true; continue());
<COMMENT>.           => (continue());
                      
                      
<STRING>"\""         => (YYBEGIN INITIAL; 
                         stringClosed := !stringClosed - 1; 
                         let val temp = !str
                             val _ = str := ""
                         in
                             Tokens.STRING(temp,yypos,yypos)
                         end);
                         
<STRING>"\\"         => (YYBEGIN ESC; continue());
<STRING>{newline}    => (ErrorMsg.error yypos ("strings can not be splited to separate lines " ^ 
		         yytext ^ " [use backslash for splitting string]"); continue());
<STRING>.            => (str := !str ^ yytext; continue());
                      
<ESC>{esqchar}       => (YYBEGIN STRING; str := !str ^ "\\" ^ yytext; continue());
<ESC>{formatchar}    => (YYBEGIN FORMAT; continue());
<ESC>{oct}           => (YYBEGIN STRING; str := !str ^ String.str(Char.chr(valOf(Int.fromString yytext))); continue());
<ESC>.               => (ErrorMsg.error yypos ("illegal escape character " ^ yytext); continue());
                      
<FORMAT>{formatchar} => (continue());
<FORMAT>"\\"         => (YYBEGIN STRING; continue());
<FORMAT>.            => (ErrorMsg.error yypos ("illegal character " ^ yytext ^ 
		         " [use backslash for splitting string]"); continue());
                      
<INITIAL>"\""        => (YYBEGIN STRING; 
                         stringClosed := !stringClosed + 1; 
                         continue());
<INITIAL>":="        => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>"|"         => (Tokens.OR(yypos,yypos+1));
<INITIAL>"&"         => (Tokens.AND(yypos,yypos+1));
<INITIAL>">="        => (Tokens.GE(yypos,yypos+2));
<INITIAL>">"         => (Tokens.GT(yypos,yypos+1));
<INITIAL>"<="        => (Tokens.LE(yypos,yypos+2));
<INITIAL>"<"         => (Tokens.LT(yypos,yypos+1));
<INITIAL>"<>"        => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"="         => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"/"         => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"*"         => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"-"         => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"+"         => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"."         => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"}"         => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"{"         => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"]"         => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"["         => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>")"         => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"("         => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>";"         => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>":"         => (Tokens.COLON(yypos,yypos+1));
<INITIAL>","	     => (Tokens.COMMA(yypos,yypos+1));
                      
<INITIAL>{ID}        => (Tokens.ID(yytext,yypos,yypos+size(yytext)));
<INITIAL>{digit}+    => (Tokens.INT(valOf(Int.fromString(yytext)),yypos,yypos+size(yytext)));
                      
<INITIAL>.           => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());



