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
NAMEDARGS;
}

@header {
require_relative 'scope'
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

prog : line* ;

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
      -> ^(FLINE TEXT[$fline_contents.start,$fline_contents.text])
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
