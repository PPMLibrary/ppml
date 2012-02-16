grammar CG ;

options {
language = Ruby;
output = AST;
}

tokens{
FMACRO;
FLINE;
ARGS;
}

@header{
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

prog : line* ;

line
    : ({fmacro_call?}?=> fcmacro
    | fline)
    ;

fcmacro
    : (result=ID ASSIGN)?  name=ID  '(' (args+=ID (',' args+=ID)* )? ')' NEWLINE
      -> ^(FMACRO $name $result? ^(ARGS $args*))
    ;

fline
    : fline_contents NEWLINE
      -> FLINE[$NEWLINE,$fline_contents.text]
    ;

fline_contents : allowed* ;

fragment
ASSIGN: '=' ;

ID	: (ALPHA | '_') (ALNUM | '_')* ;
allowed	: ID | '(' | ')' | NUMBER ;

NUMBER : DIGIT+ ;

WS	: SPORT* { $channel=HIDDEN } ;
NEWLINE	: '\n';

COMMENT : '!' .* NEWLINE { $channel=HIDDEN } ;

fragment
SPORT	: ' '|'\t';
fragment
ALNUM	: (ALPHA | DIGIT) ;
fragment
ALPHA	: 'a'..'z' | 'A'..'Z' ;
fragment
DIGIT	: '0'..'9' ;
