grammar CG ;

options {
language = Ruby;
output =  template;
}

@header{
# require 'something'
}

@init{
# goes into initialize
}

prog : f+=fline (f+=fline)+ { puts $f } ;

fline : ANYTOK* NEWLINE { puts $fline.text } ;

//fcmacro : (ID '=')? ID '()' NEWLINE { $fcmacro.text } ;

ID : (ALPHA | '_') (ALNUM | '_')* ;

fragment
ALNUM : (ALPHA | DIGIT) ;
fragment
ALPHA : 'a'..'z' | 'A'..'Z' ;
fragment
DIGIT : '0'..'9' ;

ANYTOK : ANYCHAR+ ;

fragment
ANYCHAR : ~(SPORT | NEWLINE) ;

WS : SPORT { skip } ;

fragment
SPORT : ' '|'\t';

NEWLINE : '\n';
