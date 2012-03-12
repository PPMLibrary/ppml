grammar CG ;

options {
language = Ruby;
output = AST;
}

tokens {
PROGRAM;
SCOPE_START;
SCOPE_END;
FMACRO;
FLINE;
TEXT;
ARGS;
NAMEDARGS;
}

@header {
#require_relative 'scope'
}

@members {
attr_accessor :preprocessor

def fmacro_call?
  if input.peek(2) == TokenData::EQUALS_T
    @preprocessor.macros.has_key?(input.look(3).text)
  else
    @preprocessor.macros.has_key?(input.look(1).text)
  end
end
}

prog
    : program_statement
    | naked_code
    ;

program_statement
    : open=program_start
        ({@input.peek(2) != PROGRAM_T}? body+=line)*
      close=program_end
      -> ^(PROGRAM $open $close $body*)
    ;

program_start : PROGRAM_T progname=ID NEWLINE
        -> ^(SCOPE_START $progname TEXT[$program_start.start,$program_start.text]) ;
program_end   : ( ENDPROGRAM_T | END_T PROGRAM_T ) NEWLINE
        -> ^(SCOPE_END TEXT[$program_end.start,$program_end.text]) ;

naked_code : line* ;

line
    : ({fmacro_call?}?=> fcmacro
    | fline)
    ;

fcmacro
    : (result=ID EQUALS_T)? 
       name=ID  LEFT_PAREN_T
                (args+=value (COMMA args+=value)*
                (names+=ID EQUALS_T values+=value)* )?
                RIGHT_PAREN_T
       NEWLINE
      -> ^(FMACRO $name $result? ^(ARGS $args*) ^(NAMEDARGS $names*) ^(NAMEDARGS $values*))
    ;

fline
    : fline_contents NEWLINE
      -> ^(FLINE TEXT[$fline_contents.start,$fline_contents.text])
    ;

fline_contents : allowed* ;

allowed	: ID | ANY_CHAR | NUMBER | LEFT_PAREN_T | RIGHT_PAREN_T | COMMA | EQUALS_T | STRING | END_T ;

value : ID | NUMBER | STRING ;

// Fortran Keywords

PROGRAM_T    : 'PROGRAM'    | 'program' ;
ENDPROGRAM_T : 'ENDPROGRAM' | 'endprogram' ;
END_T        : 'END'        | 'end';

// Identifiers

ID	: (ALPHA | '_') (ALNUM | '_' | '%')* ;

// Constants

STRING
    : '"' ('\\"'|~'"')* '"' 
    | '\'' ('\\\''|~'\'')* '\''
    ;

NUMBER : DIGIT+ ;

// Whitespace

NEWLINE	: '\r'? '\n' ;
WS	: (' '|'\r'|'\t'|'\u000C')* { $channel=:hidden } ;

COMMENT : '!' ~('\n'|'\r')* { $channel=:hidden } ;

//MULTILINE : '&' WS NEWLINE { skip } ;

// Fragments

fragment
COMPARISSON : '<' | '>' ;

EQUALS_T      : '=' ;
LEFT_PAREN_T  : '(' ;
RIGHT_PAREN_T : ')' ;
COMMA         : ',' ;

fragment
ALNUM	: ( ALPHA | DIGIT ) ;
fragment
ALPHA	: 'a'..'z' | 'A'..'Z' ;
fragment
DIGIT	: '0'..'9' ;

ANY_CHAR : ~( '\r' | '\n' ) ;
