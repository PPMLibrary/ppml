grammar CG ;

options {
language = Ruby;
output = AST;
}

tokens {
PROGRAM;
MODULE;
SUBROUTINE;
SCOPE_START;
SCOPE_END;
INNER_STUFF;
USE;
IMPLICIT;
CONTAINS;
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
def fmacro_call?
  m = Preprocessor.instance.macros
  if input.peek(2) == TokenData::EQUALS_T
    m.has_key?(input.look(3).text)
  else
    m.has_key?(input.look(1).text)
  end
end
}

prog
    : (naked_code)=>naked_code
    | (
            ((module_statement)=>module_statement
            |(subroutine_statement)=>subroutine_statement
            )*
            program_statement?
            (module_statement
            |subroutine_statement
            )*
      )
    ;

// Scope detection - top level statements

program_statement
    : ((NEWLINE)=>NEWLINE)*
      open=program_start
        i=inner_stuff
      close=program_end
      ((NEWLINE)=>NEWLINE)*
      -> ^(PROGRAM $open $close
                   $i)
    ;

subroutine_statement
    : ((NEWLINE)=>NEWLINE)*
      open=subroutine_start
        i=inner_stuff
      close=subroutine_end
      ((NEWLINE)=>NEWLINE)*
      -> ^(SUBROUTINE $open $close
                      $i)
    ;

module_statement
    : ((NEWLINE)=>NEWLINE)*
      open=module_start
        i=inner_stuff
      close=module_end
      ((NEWLINE)=>NEWLINE)*
      -> ^(MODULE $open $close
                  $i)
    ;

naked_code : line* ;

// Scope detecion - start and end lines

program_start : PROGRAM_T name=ID NEWLINE
        -> ^(SCOPE_START $name TEXT[$program_start.start,$program_start.text]) ;
program_end   : ( ENDPROGRAM_T | END_T PROGRAM_T ) ID? NEWLINE
        -> ^(SCOPE_END TEXT[$program_end.start,$program_end.text]) ;

module_start : MODULE_T name=ID NEWLINE
        -> ^(SCOPE_START $name TEXT[$module_start.start,$module_start.text]) ;
module_end   : ( ENDMODULE_T | END_T MODULE_T ) ID? NEWLINE
        -> ^(SCOPE_END TEXT[$module_end.start,$module_end.text]) ;

subroutine_start : SUBROUTINE_T name=ID allowed* NEWLINE
        -> ^(SCOPE_START $name TEXT[$subroutine_start.start,$subroutine_start.text]) ;
subroutine_end   : ( ENDSUBROUTINE_T | END_T SUBROUTINE_T ) ID? NEWLINE
        -> ^(SCOPE_END TEXT[$subroutine_end.start,$subroutine_end.text]) ;

// Scope detection - body

inner_stuff
    : ((NEWLINE)=>NEWLINE)*
      use+=use_statement*
      ((NEWLINE)=>NEWLINE)*
      (imp=implicit_none)?
      ({@input.peek(2) != PROGRAM_T}? body+=line)*
      (con=contains
       sub+=subroutine_statement+)?
       -> ^(INNER_STUFF
            ^(USE $use*)
            $imp?
            $con?
            $sub*
            $body*)
    ;

implicit_none
    : IMPLICIT_T NONE_T NEWLINE
      -> ^(IMPLICIT TEXT[$implicit_none.start,$implicit_none.text])
    ;
contains
    : CONTAINS_T NEWLINE 
      -> ^(CONTAINS TEXT[$contains.start,$contains.text])
    ;
use_statement
    : USE_T allowed* NEWLINE
      -> ^(FLINE TEXT[$use_statement.start,$use_statement.text])
    ;

// Actual code

line
    : ({fmacro_call?}?=> fcmacro
    | fline)
    ;

// foreach
//     : FOREACH_T it=ID IN_T name=ID a=arglist?
//         (DOT_T modifiers+=ID arglists+=arglist)*
//       NEWLINE
//         (bodies+=foreach_body
//         |   ((bname+=ID | bname+=DEFAULT_T) COLON_T
//             bodies+=foreach_body*
//             )+
//         )
//       (ENDFOREACH_T | END_T FOREACH_T) NEWLINE
//         -> ^(FOREACH $name $it $a? ^(MODIFIERS $modifiers $arglists))
//     ;

// foreach_body
//     : ({@input.peek(2) != FOREACH_T}? body+=line)*
//       -> ^(BODY $body*)
//     ;

fcmacro
    : (result=ID EQUALS_T)?
       name=ID args=arglist NEWLINE
      -> ^(FMACRO $name $result? $args)
    ;

arglist
    : LEFT_PAREN_T
       ( args+=value (COMMA args+=value)*
        (names+=ID EQUALS_T values+=value)*
       )?
      RIGHT_PAREN_T
      -> ^(ARGS $args* ^(NAMEDARGS $names*) ^(NAMEDARGS $values*))
    ;

fline
    : allowed* NEWLINE
      -> ^(FLINE TEXT[$fline.start,$fline.text])
    ;

allowed	: ID | ANY_CHAR | NUMBER | LEFT_PAREN_T | RIGHT_PAREN_T | COMMA | EQUALS_T | STRING | END_T ;

value : ID | NUMBER | STRING ;

// Fortran Keywords

PROGRAM_T       : 'PROGRAM'       | 'program'       ;
ENDPROGRAM_T    : 'ENDPROGRAM'    | 'endprogram'    ;
MODULE_T        : 'MODULE'        | 'module'        ;
ENDMODULE_T     : 'ENDMODULE'     | 'endmodule'     ;
SUBROUTINE_T    : 'SUBROUTINE'    | 'subroutine'    ;
ENDSUBROUTINE_T : 'ENDSUBROUTINE' | 'endsubroutine' ;
END_T           : 'END'           | 'end'           ;
USE_T           : 'USE'           | 'use'           ;
IMPLICIT_T      : 'IMPLICIT'      | 'implicit'      ;
NONE_T          : 'NONE'          | 'none'          ;
CONTAINS_T      : 'CONTAINS'      | 'contains'      ;

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
