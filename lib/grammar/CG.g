grammar CG ;

options {
language = Ruby;
output = AST;
}

tokens {
PROGRAM;
MODULE;
SUBROUTINE;
FUNCTION;
SCOPE_START;
SCOPE_END;
INNER_STUFF;
USE;
IMPLICIT;
CONTAINS;
FMACRO;
FOREACH;
MODIFIERS;
BODY;
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
    if input.peek(4) == TokenData::DOT_T
        m.has_key?(input.look(5).text)
    else
        m.has_key?(input.look(3).text)
    end
  else
    if input.peek(2) == TokenData::DOT_T
        m.has_key?(input.look(3).text)
    else
        m.has_key?(input.look(1).text)
    end
  end
end
}

prog
    : (naked_code)=>naked_code
    | (
            ((module_statement)=>module_statement
            |(subroutine_statement)=>subroutine_statement
            |(function_statement)=>function_statement
            )*
            program_statement?
            (module_statement
            |subroutine_statement
            )*
      )
    ;

// Scope detection - top level statements

program_statement
    : open=program_start
        i=inner_stuff
      close=program_end
      -> ^(PROGRAM $open $i $close)
    ;

subroutine_statement
    : open=subroutine_start
        i=inner_stuff
      close=subroutine_end
      -> ^(SUBROUTINE $open $i $close)
    ;

module_statement
    : open=module_start
        i=inner_stuff
      close=module_end
      -> ^(MODULE $open $i $close)
    ;

function_statement
    : open=function_start
        i=inner_stuff
      close=function_end
      -> ^(FUNCTION $open $i $close)
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

subroutine_start : RECURSIVE_T? SUBROUTINE_T name=ID arglist? NEWLINE
        -> ^(SCOPE_START $name TEXT[$subroutine_start.start,$subroutine_start.text]) ;
subroutine_end   : ( ENDSUBROUTINE_T | END_T SUBROUTINE_T ) ID? NEWLINE
        -> ^(SCOPE_END TEXT[$subroutine_end.start,$subroutine_end.text]) ;

function_start : (ID FUNCTION_T name=ID arglist? NEWLINE
               | FUNCTION_T name=ID arglist?
                 RESULT_T LEFT_PAREN_T ID RIGHT_PAREN_T NEWLINE)
        -> ^(SCOPE_START $name TEXT[$function_start.start,$function_start.text]) ;
function_end   : ( ENDFUNCTION_T | END_T FUNCTION_T ) ID? NEWLINE
        -> ^(SCOPE_END TEXT[$function_end.start,$function_end.text]) ;

// Scope detection - body

inner_stuff
    : use+=use_statement*
      (imp=implicit_none)?
      ({@input.peek(2) != PROGRAM_T}? body+=line)*
      (con=contains
            (sub+=subroutine_statement
            |sub+=function_statement
            )+ )?
       -> ^(INNER_STUFF
            ^(USE $use*)
            $imp?
            ^(CONTAINS $con?
                $sub*)
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
    : {fmacro_call?}?=> fcmacro
    | subroutine_statement
    | function_statement
    | foreach
    | fline
    ;

foreach
    : FOREACH_T it=ID IN_T name=ID a=arglist?
        (DOT_T modifiers+=ID arglists+=arglist?)*
      NEWLINE
        bodies+=foreach_body
        // ((foreach_body)=>bodies+=foreach_body
        // |   {@input.peek(2) == COLON_T}?=>((bname+=ID | bname+=DEFAULT_T) COLON_T NEWLINE
        //       (foreach_body)=>bodies+=foreach_body*
        //     )+
        // )
      (ENDFOREACH_T | END_T FOREACH_T) NEWLINE
        -> ^(FOREACH $name $it $a? ^(MODIFIERS $modifiers* $arglists*) $bodies*)
    ;

foreach_body
    : ({@input.peek(2) != FOREACH_T}? body+=line)*
      -> ^(BODY $body*)
    ;

fcmacro
    : (result=ID EQUALS_T)? ((name=ID) | (dotarg=ID DOT_T name=ID)) args=arglist NEWLINE
      -> ^(FMACRO $name $result? $args $dotarg?)
    ;

arglist
    : LEFT_PAREN_T
       ( (args+=value | names+=ID EQUALS_T values+=value)
         (COMMA_T (args+=value | names+=ID EQUALS_T values+=value))*
       )?
      RIGHT_PAREN_T
      -> ^(ARGS $args* ^(NAMEDARGS $names*) ^(NAMEDARGS $values*))
    ;

// Catchall

fline
    : (allowed*
      | MODULE_T PROCEDURE_T ID
      | PROCEDURE_T allowed*
      ) NEWLINE
      -> ^(FLINE TEXT[$fline.start,$fline.text])
    ;

allowed
    : ID
    | ANY_CHAR
    | NUMBER | STRING
    | LEFT_PAREN_T | RIGHT_PAREN_T
    | COMMA_T | EQUALS_T | DOUBLE_COLON_T | COLON_T | AMPERSAND_T
    | END_T | IN_T
    | boolean | logical | comparison
    ;

value : ID | NUMBER | STRING ;


////////////////////////////////////////////////////////////////////////////////
// Lexer Rules
////////////////////////////////////////////////////////////////////////////////

// PPM Keywords

FOREACH_T       : 'FOREACH'       | 'foreach'       ;
ENDFOREACH_T    : 'ENDFOREACH'    | 'endforeach'    ;
IN_T            : 'IN'            | 'in'            ;

// Fortran Keywords

PROGRAM_T       : 'PROGRAM'       | 'program'       ;
ENDPROGRAM_T    : 'ENDPROGRAM'    | 'endprogram'    ;
MODULE_T        : 'MODULE'        | 'module'        ;
ENDMODULE_T     : 'ENDMODULE'     | 'endmodule'     ;
SUBROUTINE_T    : 'SUBROUTINE'    | 'subroutine'    ;
ENDSUBROUTINE_T : 'ENDSUBROUTINE' | 'endsubroutine' ;
FUNCTION_T      : 'FUNCTION'      | 'function'      ;
ENDFUNCTION_T   : 'ENDFUNCTION'   | 'endfunction'   ;
END_T           : 'END'           | 'end'           ;
USE_T           : 'USE'           | 'use'           ;
IMPLICIT_T      : 'IMPLICIT'      | 'implicit'      ;
NONE_T          : 'NONE'          | 'none'          ;
CONTAINS_T      : 'CONTAINS'      | 'contains'      ;
PROCEDURE_T     : 'PROCEDURE'     | 'procedure'     ;
RECURSIVE_T     : 'RECURSIVE'     | 'recursive'     ;
RESULT_T        : 'RESULT'        | 'result'        ;
// DEFAULT_T       : 'DEFAULT'       | 'default'       ;

// Before logical operators to give it precedence
DOT_T          : '.' ;

// True/False

boolean : TRUE_T | FALSE_T ;

TRUE_T  : '.TRUE.'  | '.true.'  ;
FALSE_T : '.FALSE.' | '.false.' ;

// Logical

logical : AND_T | OR_T | NOT_T | EQV_T | NEQV_T ;

AND_T  : '.AND.'  | '.nd.'  ;
OR_T   : '.OR.'   | '.or.'   ;
NOT_T  : '.NOT.'  | '.not.'  ;
EQV_T  : '.EQV.'  | '.eqv.'  ;
NEQV_T : '.NEQV.' | '.neqv.' ;

// Comparison

comparison : GT_T | LT_T | GE_T | LE_T | EQ_T | NE_T ;

GT_T : '.GT.' | '.gt.' | '>'  ;
LT_T : '.LT.' | '.lt.' | '<'  ;
GE_T : '.GE.' | '.ge.' | '>=' ;
LE_T : '.LE.' | '.le.' | '<=' ;
EQ_T : '.EQ.' | '.eq.' | '==' ;
NE_T : '.NE.' | '.ne.' | '/=' ;

// Identifiers

ID	: (ALPHA | '_') (ALNUM | '_' | '%')* ;

// Constants

STRING
    : '"' ('\\"'|~'"')* '"' 
    | '\'' ('\\\''|~'\'')* '\''
    ;

NUMBER : DIGIT+;

// Whitespace

EMPTY_LINE     : {column==0}?=> WS_SPEC COMMENT_SPEC? '\r'? '\n' { $channel=:hidden } ;
CONTINUED_LINE : AMPERSAND_T (WS_SPEC '\r'? '\n')+ WS_SPEC AMPERSAND_T? { $channel=:hidden } ;

NEWLINE	: '\r'? '\n' ;

WS	    : WS_SPEC      { $channel=:hidden } ;
COMMENT : COMMENT_SPEC { $channel=:hidden } ;

fragment
COMMENT_SPEC : '!' ~('\n'|'\r')* ;
fragment
WS_SPEC : (' '|'\r'|'\t'|'\u000C')* ;

//MULTILINE : '&' WS NEWLINE { skip } ;

// Fragments

fragment
COMPARISSON : '<' | '>' ;

EQUALS_T       : '='  ;
LEFT_PAREN_T   : '('  ;
RIGHT_PAREN_T  : ')'  ;
AMPERSAND_T    : '&'  ;
DOUBLE_COLON_T : '::' ;
COLON_T        : ':'  ;
COMMA_T        : ','  ;

fragment
ALNUM	: ( ALPHA | DIGIT ) ;
fragment
ALPHA	: 'a'..'z' | 'A'..'Z' ;
fragment
DIGIT	: '0'..'9' ;

ANY_CHAR : ~( '\r' | '\n' | ' ' | '\t' ) ;
