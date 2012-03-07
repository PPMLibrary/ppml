grammar CG ;

options {
language = Ruby;
output = AST;
}

tokens {
FMACRO;
FLINE;
TEXT;
ARGS;
LINE;
INDENT;
COMMENT;
NAMEDARGS;
}

@header {
# require 'something'
}

@members{
attr_accessor :preprocessor

def fmacro_call?
  if input.peek(2) == TokenData::ASSIGN
    @preprocessor.macros.has_key?(input.look(3).text)
  else
    @preprocessor.macros.has_key?(input.look(1).text)
  end
end
}

prog : wrapper* ;

wrapper : {
previous_token = @input.look(-1).to_i
next_token = @input.look(1).to_i
hidden_tokens = []
if !(previous_token == 0 and next_token == 0)
  previous_token = previous_token + 1
  next_token = next_token - 1
  hidden_tokens = @input.tokens(previous_token, next_token)
end
#STDERR.puts "hidden_tokens : #{hidden_tokens}"
indent_string = hidden_tokens.map(&:text).join('')
#STDERR.puts "indent string : '#{indent_string}' (#{indent_string.length})"
}
        l=line
{
previous_token = @input.look(-2)
next_token = @input.look(-1)
# STDERR.puts "prev : #{previous_token.inspect}"
# STDERR.puts "next : #{next_token.inspect}"
if previous_token.nil?
  previous_token = 0
else
  previous_token = previous_token.to_i + 1
end
next_token = next_token.to_i - 1
# STDERR.puts "prev : #{previous_token}"
# STDERR.puts "next : #{next_token}"
hidden_tokens = @input.tokens(previous_token, next_token)
# STDERR.puts "hidd : #{hidden_tokens}"
comment_string = hidden_tokens.map(&:text).join('')
# STDERR.puts "comm : '#{comment_string}'"
}
        -> ^(LINE $l { @adaptor.create_from_type(INDENT, indent_string) }
                     { @adaptor.create_from_type(COMMENT, comment_string) });

line
    : ({fmacro_call?}?=> fcmacro
    | fline)
    ;

fcmacro
    : (result=ID ASSIGN)?  name=ID  LPAREN (args+=value (COMMA args+=value)* (names+=ID ASSIGN values+=value)* )? RPAREN NEWLINE
      -> ^(FMACRO $name $result? ^(ARGS $args*) ^(NAMEDARGS $names*) ^(NAMEDARGS $values*))
    ;

fline
    : fline_contents NEWLINE
      -> ^(FLINE TEXT[$NEWLINE,$fline_contents.text])
    ;

fline_contents : allowed* ;

allowed	: ID | ANY_CHAR | NUMBER | LPAREN | RPAREN | COMMA | ASSIGN | STRING ;

value : ID | NUMBER | STRING ;

ID	: (ALPHA | '_') (ALNUM | '_' | '%')* ;

STRING
    : '"' ('\\"'|~'"')* '"' 
    | '\'' ('\\\''|~'\'')* '\''
    ;

NUMBER : DIGIT+ ;

WS	: SPORT* { $channel=:hidden } ;
NEWLINE	: '\n';

COMMENT : '!' (~NEWLINE)* { $channel=:hidden } ;

//MULTILINE : '&' WS NEWLINE { skip } ;

fragment
COMPARISSON : '<' | '>' ;

ASSIGN       : '=' ;
LPAREN       : '(' ;
RPAREN       : ')' ;
COMMA        : ',' ;

fragment
SPORT	: ' ' | '\t';
fragment
ALNUM	: ( ALPHA | DIGIT ) ;
fragment
ALPHA	: 'a'..'z' | 'A'..'Z' ;
fragment
DIGIT	: '0'..'9' ;

ANY_CHAR : ~( '\r' | '\n' ) ;
