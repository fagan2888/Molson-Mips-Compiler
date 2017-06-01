type svalue = Tokens.svalue
type pos = int
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ref 1
val inComment = ref false
val inString = ref false
val string = ref ""

fun eof() = (if !inComment=true then (inComment:=false; Error.msg 0 ("eof inside comment")) else (); if !inString=true then (inString:=false; Error.msg 0 ("eof inside string")) else (); Tokens.EOF(0,0))

%%
%header (functor MolsonLexFun (structure Tokens : Molson_TOKENS));
%s COMMENT STRING;
%%

<INITIAL>[ \t\n\b] => (continue());
<INITIAL>type      => (Tokens.TYPE (yypos, yypos));
<INITIAL>nil       => (Tokens.NIL (yypos, yypos));
<INITIAL>of        => (Tokens.OF (yypos, yypos));
<INITIAL>var       => (Tokens.VAR (yypos, yypos));
<INITIAL>function  => (Tokens.FUNCTION (yypos, yypos));
<INITIAL>break     => (Tokens.BREAK (yypos, yypos));
<INITIAL>while     => (Tokens.WHILE (yypos, yypos));
<INITIAL>else      => (Tokens.ELSE (yypos, yypos));
<INITIAL>if        => (Tokens.IF (yypos, yypos));
<INITIAL>return    => (Tokens.RETURN (yypos, yypos));
<INITIAL>true      => (Tokens.BOOL (true, yypos, yypos));
<INITIAL>false     => (Tokens.BOOL (false, yypos, yypos));
<INITIAL>"="       => (Tokens.ASSIGN (yypos, yypos));
<INITIAL>"|"       => (Tokens.OR (yypos, yypos));
<INITIAL>"&"       => (Tokens.AND (yypos, yypos));
<INITIAL>">="      => (Tokens.GE (yypos, yypos));
<INITIAL>">"       => (Tokens.GT (yypos, yypos));
<INITIAL>"<="      => (Tokens.LE (yypos, yypos));
<INITIAL>"<"       => (Tokens.LT (yypos, yypos));
<INITIAL>"!="      => (Tokens.NEQ (yypos, yypos));
<INITIAL>"=="      => (Tokens.EQ (yypos, yypos));
<INITIAL>"/"       => (Tokens.DIVIDE (yypos, yypos));
<INITIAL>"*"       => (Tokens.TIMES (yypos, yypos));
<INITIAL>"-"       => (Tokens.MINUS (yypos, yypos));
<INITIAL>"+"       => (Tokens.PLUS (yypos, yypos));
<INITIAL>"."       => (Tokens.DOT (yypos, yypos));
<INITIAL>"}"       => (Tokens.RBRACE (yypos, yypos));
<INITIAL>"{"       => (Tokens.LBRACE (yypos, yypos));
<INITIAL>")"       => (Tokens.RPAREN (yypos, yypos));
<INITIAL>"("       => (Tokens.LPAREN (yypos, yypos));
<INITIAL>":"       => (Tokens.COLON (yypos, yypos));
<INITIAL>","       => (Tokens.COMMA (yypos, yypos));

<INITIAL>[0-9]+                => (Tokens.INT ((Option.valOf (Int.fromString yytext)), yypos, yypos));
<INITIAL>[a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID (yytext, yypos, yypos));

<INITIAL>"/*" => (inComment:=true; YYBEGIN COMMENT; continue());
<INITIAL>"*/" => (Error.msg yypos ("end comment with no start comment"); continue());
<COMMENT>"*/" => (inComment:=false; YYBEGIN INITIAL; continue());
<COMMENT>.    => (continue());

<INITIAL>\" => (inString:=true; string:=""; YYBEGIN STRING; continue());
<STRING>\"  => (inString:=false; YYBEGIN INITIAL; Tokens.STRING(!string, yypos, yypos));
<STRING>\\. => (case Char.fromString yytext of NONE => Error.msg yypos ("illegal escape sequence " ^ yytext) | SOME c => string:=(!string)^(String.str c); continue());
<STRING>.   => (string:=(!string)^yytext; continue());

. => (Error.msg yypos ("illegal character " ^ yytext); continue());
