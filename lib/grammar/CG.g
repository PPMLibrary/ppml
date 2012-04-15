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
PROCEDURE;
TYPE;
SCOPE_START;
SCOPE_END;
INNER_STUFF;
TYPE_BODY;
USE;
IMPORT;
GENERIC;
IMPLICIT;
CONTAINS;
FMACRO;
IMACRO;
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

@rulecatch {
rescue ANTLR3::Error::RecognitionError => re
  report_error(re)
  raise re
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

def imacro_call?
  m = Preprocessor.instance.macros
  if input.peek(3) == TokenData::EQUALS_T
        m.has_key?(input.look(4).text)
  else
        m.has_key?(input.look(2).text)
  end
end

def error_message( e = $! )
  s = ""
  case e
  when UnwantedToken
    token_name = token_name( e.expecting )
    s = "extraneous input #{ token_error_display( e.unexpected_token ) } expecting #{ token_name }"
  when MissingToken
    token_name = token_name( e.expecting )
    s = "missing #{ token_name } at #{ token_error_display( e.symbol ) }"
  when MismatchedToken
    token_name = token_name( e.expecting )
    s = "mismatched input #{ token_error_display( e.symbol ) } expecting #{ token_name }"
  when MismatchedTreeNode
    token_name = token_name( e.expecting )
    s = "mismatched tree node: #{ e.symbol } expecting #{ token_name }"
  when NoViableAlternative
    s = "no viable alternative at input " << token_error_display( e.symbol )
  when MismatchedSet
    s = "mismatched input #{token_error_display( e.symbol )} expecting set #{e.expecting.inspect}" 
  when MismatchedNotSet
    s = "mismatched input #{token_error_display( e.symbol )} expecting set #{e.expecting.inspect}" 
  when FailedPredicate
    s = "rule #{e.rule_name} failed predicate: [ #{e.predicate_text} ]?" 
  else e.message
  end
  s
end

}

prog
    : (naked_code)=>naked_code
    | (
            ((module_statement)=>module_statement
            |(subroutine_statement)=>subroutine_statement
            |(function_statement)=>function_statement
            |(imacro)=>imacro
            )*
            program_statement?
            (module_statement
            |subroutine_statement
            |imacro
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

type_statement
    : open=type_start
        i=type_body
      close=type_end
      -> ^(TYPE $open $i $close)
    ;


naked_code : line* ;

// Scope detecion - start and end lines

program_start : PROGRAM_T name=ID_T NEWLINE_T
        -> ^(SCOPE_START $name TEXT[$program_start.start,$program_start.text]) ;
program_end   : ( ENDPROGRAM_T | END_T PROGRAM_T ) ID_T? NEWLINE_T
        -> ^(SCOPE_END TEXT[$program_end.start,$program_end.text]) ;

module_start : MODULE_T name=ID_T NEWLINE_T
        -> ^(SCOPE_START $name TEXT[$module_start.start,$module_start.text]) ;
module_end   : ( ENDMODULE_T | END_T MODULE_T ) ID_T? NEWLINE_T
        -> ^(SCOPE_END TEXT[$module_end.start,$module_end.text]) ;

subroutine_start : RECURSIVE_T? SUBROUTINE_T name=ID_T arglist? NEWLINE_T
        -> ^(SCOPE_START $name TEXT[$subroutine_start.start,$subroutine_start.text]) ;
subroutine_end   : ( ENDSUBROUTINE_T | END_T SUBROUTINE_T ) ID_T? NEWLINE_T
        -> ^(SCOPE_END TEXT[$subroutine_end.start,$subroutine_end.text]) ;

function_start : (ID_T)? FUNCTION_T name=ID_T arglist? 
            (RESULT_T LEFT_PAREN_T ID_T RIGHT_PAREN_T)? NEWLINE_T
        -> ^(SCOPE_START $name TEXT[$function_start.start,$function_start.text]) ;
function_end   : ( ENDFUNCTION_T | END_T FUNCTION_T ) ID_T? NEWLINE_T
        -> ^(SCOPE_END TEXT[$function_end.start,$function_end.text]) ;

type_start : TYPE_T 
            ( (COMMA_T EXTENDS_T LEFT_PAREN_T ID_T RIGHT_PAREN_T)
            | (COMMA_T ABSTRACT_T) )*
           (DOUBLE_COLON_T)? name=ID_T NEWLINE_T
        -> ^(SCOPE_START $name TEXT[$type_start.start,$type_start.text]) ;
type_end   : ( ENDTYPE_T | END_T TYPE_T ) ID_T? NEWLINE_T
        -> ^(SCOPE_END TEXT[$type_end.start,$type_end.text]) ;

// Scope detection - body

inner_stuff
    : (use+=use_statement)*
      (imp+=import_statement)*
      (implicit=implicit_none)?
      ({@input.peek(2) != PROGRAM_T}? body+=line)*
      (con=contains
            (sub+=subroutine_statement
            |sub+=function_statement
            |sub+=imacro
            )+ )?
       -> ^(INNER_STUFF
            ^(USE $use*)
            ^(IMPORT $imp*)
            $implicit?
            ^(CONTAINS $con?
                $sub*)
            $body*)
    ;

type_body
    : ({@input.peek(2) != TYPE_T}? body+=line)*
      (con=contains
        ((procedure_statement)=> sub+=procedure_statement
        |sub+=generic_statement
        |{@input.peek(2) != TYPE_T}? body+=line)+ )?
        //|sub+=imacro)+ )?
      -> ^(TYPE_BODY
          ^(CONTAINS $con?
                     $sub*)
           $body*)
    ;


implicit_none
    : IMPLICIT_T NONE_T NEWLINE_T
      -> ^(IMPLICIT TEXT[$implicit_none.start,$implicit_none.text])
    ;
contains
    : CONTAINS_T NEWLINE_T 
      -> ^(CONTAINS TEXT[$contains.start,$contains.text])
    ;
use_statement
    : USE_T allowed* NEWLINE_T
      -> ^(FLINE TEXT[$use_statement.start,$use_statement.text])
    ;

import_statement
    : IMPORT_T allowed* NEWLINE_T
      -> ^(FLINE TEXT[$import_statement.start,$import_statement.text])
    ;

procedure_statement
    : PROCEDURE_T allowed* NEWLINE_T
      -> ^(PROCEDURE TEXT[$procedure_statement.start,$procedure_statement.text])
    ;

generic_statement
    : GENERIC_T allowed* NEWLINE_T
      -> ^(GENERIC TEXT[$generic_statement.start,$generic_statement.text])
    ;

// Actual code

line
    : {fmacro_call?}?=> fcmacro
    | {imacro_call?}?=> imacro
    | subroutine_statement
    | function_statement
    | (type_statement)=>type_statement
    | foreach
    | fline
    ;

foreach
    : FOREACH_T it=ID_T IN_T name=ID_T a=arglist?
        (DOT_T modifiers+=ID_T arglists+=arglist?)*
      NEWLINE_T
        bodies+=foreach_body
        // ((foreach_body)=>bodies+=foreach_body
        // |   {@input.peek(2) == COLON_T}?=>((bname+=ID_T | bname+=DEFAULT_T) COLON_T NEWLINE_T
        //       (foreach_body)=>bodies+=foreach_body*
        //     )+
        // )
      (ENDFOREACH_T | END_T FOREACH_T) NEWLINE_T
        -> ^(FOREACH $name $it $a? ^(MODIFIERS $modifiers* $arglists*) $bodies*)
    ;

foreach_body
    : ({@input.peek(2) != FOREACH_T}? body+=line)*
      -> ^(BODY $body*)
    ;

fcmacro
    : (result=ID_T EQUALS_T)? ((name=ID_T) | (dotarg=ID_T DOT_T name=ID_T)) args=arglist NEWLINE_T
      -> ^(FMACRO $name $result? $args $dotarg?)
    ;

imacro
    : MINCLUDE_T (name=ID_T)  args=arglist NEWLINE_T
      -> ^(IMACRO $name $args)
    ;

arglist
    : LEFT_PAREN_T
       ( (args+=value | names+=ID_T EQUALS_T values+=value)
         (COMMA_T (args+=value | names+=ID_T EQUALS_T values+=value))*
       )?
      RIGHT_PAREN_T
      -> ^(ARGS $args* ^(NAMEDARGS $names*) ^(NAMEDARGS $values*))
    ;

// Catchall

fline
    : (allowed*
      | MODULE_T PROCEDURE_T ID_T
      | PROCEDURE_T allowed*
      ) NEWLINE_T
      -> ^(FLINE TEXT[$fline.start,$fline.text])
    ;

allowed
    : ID_T
    | ANY_CHAR_T | DOT_T
    | NUMBER_T | STRING_T
    | LEFT_PAREN_T | RIGHT_PAREN_T | ARROW_T
    | COMMA_T | EQUALS_T | DOUBLE_COLON_T | COLON_T | AMPERSAND_T
    | boolean | logical | comparison
    | END_T | IN_T | TYPE_T | ABSTRACT_T
    ;

value : ID_T | NUMBER_T | STRING_T ;


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
TYPE_T          : 'TYPE'          | 'type'          ;
ENDTYPE_T       : 'ENDTYPE'       | 'endtype'       ;
EXTENDS_T       : 'EXTENDS'       | 'extends'       ;
MINCLUDE_T      : 'MINCLUDE'      | 'minclude'      ;
ABSTRACT_T      : 'ABSTRACT'      | 'abstract'      ;
GENERIC_T       : 'GENERIC'       | 'generic'       ;
IMPORT_T        : 'IMPORT'        | 'import'        ;
// DEFAULT_T       : 'DEFAULT'       | 'default'       ;

DOT_T
    : '.'
        (   (TRUE_T)=>  TRUE_T  {$type=TRUE_T}
        |   (FALSE_T)=> FALSE_T {$type=FALSE_T}
        |   (AND_T)=>   AND_T   {$type=AND_T}
        |   (OR_T)=>    OR_T    {$type=OR_T}
        |   (NOT_T)=>   NOT_T   {$type=NOT_T}
        |   (EQV_T)=>   EQV_T   {$type=EQV_T}
        |   (NEQV_T)=>  NEQV_T  {$type=NEQV_T}
        |   (GT)=>      GT      {$type=GT_T}
        |   (LT)=>      LT      {$type=LT_T}
        |   (GE)=>      GE      {$type=GE_T}
        |   (LE)=>      LE      {$type=LE_T}
        |   (EQ)=>      EQ      {$type=EQ_T}
        |   (NE)=>      NE      {$type=NE_T}
        )?
    ;

// True/False

boolean : TRUE_T | FALSE_T ;

fragment
TRUE_T  : 'TRUE.'  | 'true.'  ;
fragment
FALSE_T : 'FALSE.' | 'false.' ;

// Logical

logical : AND_T | OR_T | NOT_T | EQV_T | NEQV_T ;

fragment
AND_T  : 'AND.'  | 'and.'  ;
fragment
OR_T   : 'OR.'   | 'or.'   ;
fragment
NOT_T  : 'NOT.'  | 'not.'  ;
fragment
EQV_T  : 'EQV.'  | 'eqv.'  ;
fragment
NEQV_T : 'NEQV.' | 'neqv.' ;


// Comparison

comparison : GT_T | LT_T | GE_T | LE_T | EQ_T | NE_T ;

fragment
GT   : 'GT.' | 'gt.' ;
GT_T :  '>'  ;
fragment
LT   : 'LT.' | 'lt.' ;
LT_T :  '<'  ;
fragment
GE   : 'GE.' | 'ge.' ;
GE_T :  '>=' ;
fragment
LE   : 'LE.' | 'le.' ;
LE_T :  '<=' ;
fragment
EQ   : 'EQ.' | 'eq.' ;
EQ_T :  '==' ;
fragment
NE   : 'NE.' | 'ne.' ;
NE_T :  '/=' ;

// Identifiers

ID_T : (ALPHA | '_') (ALNUM | '_' | '%')* ;

// Constants

STRING_T
    : '"' ('\\"'|~'"')* '"' 
    | '\'' ('\\\''|~'\'')* '\''
    ;

NUMBER_T
    : DIGIT+
        ((DECIMAL)=> DECIMAL ((KIND)=> KIND)?)?
    ;

fragment
DECIMAL : '.' DIGIT+ ;
fragment
KIND : '_' (ID_T | DIGIT+) ;

// Whitespace

EMPTY_LINE_T     : {column==0}?=> WS COMMENT? '\r'? '\n' { $channel=:hidden } ;
CONTINUED_LINE_T : AMPERSAND_T (WS '\r'? '\n')+ WS AMPERSAND_T? { $channel=:hidden } ;

NEWLINE_T : '\r'? '\n' ;

WS_T      : WS      { $channel=:hidden } ;
COMMENT_T : COMMENT { $channel=:hidden } ;

EQUALS_T       : '='  ;
LEFT_PAREN_T   : '('  ;
RIGHT_PAREN_T  : ')'  ;
AMPERSAND_T    : '&'  ;
DOUBLE_COLON_T : '::' ;
COLON_T        : ':'  ;
COMMA_T        : ','  ;
ARROW_T        : '=>' ;

// Fragments

fragment
COMMENT : '!' ~('\n'|'\r')* ;
fragment
WS      : (' '|'\r'|'\t'|'\u000C')* ;

fragment
ALNUM	: ( ALPHA | DIGIT ) ;
fragment
ALPHA	: 'a'..'z' | 'A'..'Z' ;
fragment
DIGIT	: '0'..'9' ;

ANY_CHAR_T : ~( '\r' | '\n' | ' ' | '\t' ) ;
