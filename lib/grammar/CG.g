grammar CG ;

options {
language = Ruby;
output = AST;
}

tokens{
FMACRO;
FLINE;
}

@header{
# require 'something'
}

@init{
# goes into initialize
}

prog	: (fcmacro | fline)* ;

fcmacro	: (result=ID '=')? name=ID '()' NEWLINE -> ^(FMACRO $name $result?) ;
fline	: allowed* NEWLINE -> FLINE ;

ID	: (ALPHA | '_') (ALNUM | '_')* ;
allowed	: ID ;

WS	: SPORT* { $channel=HIDDEN } ;
NEWLINE	: '\n';

fragment
SPORT	: ' '|'\t';
fragment
ALNUM	: (ALPHA | DIGIT) ;
fragment
ALPHA	: 'a'..'z' | 'A'..'Z' ;
fragment
DIGIT	: '0'..'9' ;
