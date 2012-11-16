grammar CG ;

options {
language = Ruby;
output = AST;
}

tokens {
PROCEDURE;
TYPE;
SCOPE;
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
RETARGS;
NAMEDARGS;
FDARG;
RHS_SCOPE;
RHS_START;
RHS_INNER;
TIMELOOP;
TEMPLATE;
VLIST;
VPAIR;
BIND;
}

@header {
#require_relative 'scope'
}

@rulecatch {
rescue ANTLR3::Error::RecognitionError => re
  report_error(re)
  raise re
}

@init {
    @context = []
}

@members {
def fmacro_call?
  m = Preprocessor.instance.macros
  i = 2
  while (input.peek(i) != TokenData::EQUALS_T) \
    and (input.peek(i) != TokenData::NEWLINE_T) \
    and (input.peek(i) != TokenData::LEFT_PAREN_T) \
    and (input.peek(i) != -1) do
    i += 1
  end
  if input.peek(i) == TokenData::EQUALS_T
    m.has_key?(input.look(i+1).text)
  else
    m.has_key?(input.look(1).text)
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
    | scope_statement*
    ;

// Scope detection - top level statements

scope_statement
    : t=template?
      open=scope_start
        i=inner_stuff
      close=scope_end
      -> ^(SCOPE $t? $open $i $close)
    | rhs_statement
    ;

template
    : TEMPLATE_T s=STAR_T? LT_T v+=template_var (COMMA_T v+=template_var)* GT_T
      n=NOINTERFACE_T? (SUFFIXES_T f=value_list)? NEWLINE_T
      -> ^(TEMPLATE $v+ $s? $n? $f?)
    ;

template_var
    : n=ID_T COLON_T LEFT_SQUARE_T t+=template_type (COMMA_T t+=template_type)* RIGHT_SQUARE_T
        -> ^(ARGS $n $t+)
    ;

template_type
    : (ID_T|TYPE_T) (LEFT_PAREN_T (ID_T|NUMBER_T) RIGHT_PAREN_T)?
      -> ID_T[$template_type.start,$template_type.text]
    ;

scope_start
    :
    (              kind=PROGRAM_T    name=ID_T           NEWLINE_T { @context << :program    }
    |              kind=CLIENT_T     name=ID_T           NEWLINE_T { @context << :client     }
    |              kind=MODULE_T     name=ID_T           NEWLINE_T { @context << :module     }
    | ABSTRACT_T?  kind=INTERFACE_T  name=ID_T?          NEWLINE_T { @context << :interface  }
    | RECURSIVE_T? kind=SUBROUTINE_T name=ID_T  arglist? bind? NEWLINE_T { @context << :subroutine }
    | (ID_T (LEFT_PAREN_T (ID_T | NUMBER_T) RIGHT_PAREN_T)?)?
                   kind=FUNCTION_T   name=ID_T  arglist?
            (RESULT_T LEFT_PAREN_T ID_T RIGHT_PAREN_T)? bind?  NEWLINE_T { @context << :function }
    ) -> ^(SCOPE_START TEXT[$kind.text] $name? TEXT[$scope_start.start,$scope_start.text])
    ;

scope_end
    :
    ( {@context.last==:program   }?=> ( ENDPROGRAM_T    | END_T PROGRAM_T    ) ID_T? NEWLINE_T
    | {@context.last==:client    }?=> ( ENDPROGRAM_T    | END_T CLIENT_T     ) ID_T? NEWLINE_T
    | {@context.last==:module    }?=> ( ENDMODULE_T     | END_T MODULE_T     ) ID_T? NEWLINE_T
    | {@context.last==:interface }?=> ( ENDINTERFACE_T  | END_T INTERFACE_T  ) ID_T? NEWLINE_T
    | {@context.last==:subroutine}?=> ( ENDSUBROUTINE_T | END_T SUBROUTINE_T ) ID_T? NEWLINE_T
    | {@context.last==:function  }?=> ( ENDFUNCTION_T   | END_T FUNCTION_T   ) ID_T? NEWLINE_T
    )
    { @context.pop }
    -> ^(SCOPE_END TEXT[$scope_end.start,$scope_end.text])
    ;

rhs_statement
    : s=rhs_start
        i=rhs_inner_stuff
      e=rhs_end
      -> ^(RHS_SCOPE $s $i $e)
    ;

rhs_start
    : RHS_T name=ID_T args=rhs_arglist NEWLINE_T
        -> ^(RHS_START $name $args)
    ;

rhs_end
    : (END_T RHS_T | ENDRHS_T) NEWLINE_T
        -> ^(SCOPE_END TEXT[$rhs_end.start,$rhs_end.text])
    ;

rhs_inner_stuff
    : pre+=line*
      GET_FIELDS_T ret=rhs_arglist NEWLINE_T
      post+=line*
        -> ^(RHS_INNER $pre* $ret $post*) 
    ;

rhs_arglist
    : LEFT_PAREN_T
        ( args+=fd_arg
         (COMMA_T args+=fd_arg)*
        )?
      RIGHT_PAREN_T
      -> ^(ARGS $args*)
    ;

// field and discretization
fd_arg : field=ID_T (ARROW_T disc=ID_T)? -> ^(FDARG $field $disc?) ;

type_statement
    : open=type_start
        i=type_body
      close=type_end
      -> ^(TYPE $open $i $close)
    ;


naked_code : line* ;

// Scope detecion - start and end lines

type_start : kind=TYPE_T 
            ( (COMMA_T EXTENDS_T LEFT_PAREN_T ID_T RIGHT_PAREN_T)
            | (COMMA_T ABSTRACT_T) )*
           (DOUBLE_COLON_T)? name=ID_T NEWLINE_T
        { @context << :type }
        -> ^(SCOPE_START TEXT[$kind.text] $name TEXT[$type_start.start,$type_start.text]) ;
type_end   : ( ENDTYPE_T | END_T TYPE_T ) ID_T? NEWLINE_T
        { @context.pop }
        -> ^(SCOPE_END TEXT[$type_end.start,$type_end.text]) ;

// Scope detection - body

inner_stuff
    : (use+=use_statement)*
      (imp+=import_statement)*
      (implicit=implicit_none)?
      ({@input.peek(2) != PROGRAM_T}? body+=line)*
      (con=contains
            (sub+=scope_statement
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
    | scope_statement
    | (type_statement)=>type_statement
    | foreach
    | timeloop
    | fline
    ;

foreach
    : FOREACH_T it=ID_T IN_T name=ID_T a=arglist?
        (WITH_T (modifiers+=ID_T arglists+=arglist?)*)?
      NEWLINE_T
        ((loop_body)=>bodies+=loop_body
        |(bodies+=qualified_body)*
        )
      e=foreach_end
        -> ^(FOREACH $name $it $a? ^(MODIFIERS $modifiers* $arglists*) $bodies* $e)
    ;

foreach_end: (ENDFOREACH_T | END_T FOREACH_T) NEWLINE_T
   -> ^(SCOPE_END TEXT[$foreach_end.start,$foreach_end.text]);


loop_body
    : ({@input.peek(2) != FOREACH_T and @input.peek(2) != TIMELOOP_T}?
       body+=line)*
      -> ^(BODY ID_T[$loop_body.start,"default"] $body*)
    ;

qualified_body
    : FOR_T name=ID_T NEWLINE_T
      ({@input.peek(2) != FOREACH_T}?
       body+=line)*
      -> ^(BODY $name $body*)
    ;


timeloop
    : t=ID_T EQUALS_T TIMELOOP_T tp=arglist NEWLINE_T
        body=loop_body
      e=timeloop_end
      -> ^(TIMELOOP $t $tp $body $e)
    ;

timeloop_end : (ENDTIMELOOP_T | END_T TIMELOOP_T) NEWLINE_T
   -> ^(SCOPE_END TEXT[$timeloop_end.start,$timeloop_end.text]);

fcmacro
    :  results=return_args name=ID_T args=arglist NEWLINE_T
      -> ^(FMACRO $name $results $args)
    ;

return_args
    : (results+=ID_T (COMMA_T results+=ID_T)* EQUALS_T)?
      -> ^(RETARGS $results*)
    ;

imacro
    : MINCLUDE_T (name=ID_T)  args=arglist NEWLINE_T
      -> ^(IMACRO $name $args)
    ;

bind
   : BIND_T LEFT_PAREN_T allowed* RIGHT_PAREN_T
//      -> ^(BIND TEXT[$bind.start,$bind.text])
    ;

arglist
    : LEFT_PAREN_T
       ( (args+=value | args+=value_list | names+=ID_T EQUALS_T 
                                                    (values+=value | values+=value_list))
         (COMMA_T (args+=value | args+=value_list | names+=ID_T EQUALS_T 
                                                    (values+=value | values+=value_list)))*
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
    | ANY_CHAR_T | DOT_T | STAR_T
    | NUMBER_T | STRING_T
    | LEFT_PAREN_T | RIGHT_PAREN_T | ARROW_T
    | LEFT_SQUARE_T | RIGHT_SQUARE_T
    | COMMA_T | EQUALS_T | DOUBLE_COLON_T | COLON_T | AMPERSAND_T
    | boolean | logical | comparison
    | END_T | IN_T | TYPE_T | ABSTRACT_T
    ;

value_list
    : LEFT_SQUARE_T (vals+=value | vals+=value_pair) 
    (COMMA_T (vals+=value | vals+=value_pair))* RIGHT_SQUARE_T
      -> ^(VLIST $vals*)
    ;

value_pair : v1=ID_T ARROW_T v2=value -> ^(VPAIR $v1 $v2) ;

value : ID_T | NUMBER_T | STRING_T | CODE_T;


////////////////////////////////////////////////////////////////////////////////
// Lexer Rules
////////////////////////////////////////////////////////////////////////////////

// PPM Keywords

FOREACH_T       : 'FOREACH'       | 'foreach'       ;
ENDFOREACH_T    : 'ENDFOREACH'    | 'endforeach'    ;
FOR_T           : 'FOR'           | 'for'           ;
IN_T            : 'IN'            | 'in'            ;
RHS_T           : 'RHS'           | 'rhs'           ;
ENDRHS_T        : 'ENDRHS'        | 'endrhs'        ;
GET_FIELDS_T    : 'GET_FIELDS'    | 'get_fields'    ;
TIMELOOP_T      : 'TIMELOOP'      | 'timeloop'      ;
ENDTIMELOOP_T   : 'ENDTIMELOOP'   | 'endtimeloop'   ;
WITH_T          : 'WITH'          | 'with'          ;
TEMPLATE_T      : 'TEMPLATE'      | 'template'      ;
CLIENT_T        : 'CLIENT'        | 'client'        ;
ENDCLIENT_T     : 'ENDCLIENT'     | 'endclient'     ;
NOINTERFACE_T   : 'NOINTERFACE'   | 'nointerface'   ;
SUFFIXES_T      : 'SUFFIXES'      | 'suffixes'      ;

// Fortran Keywords

PROGRAM_T       : 'PROGRAM'       | 'program'       ;
ENDPROGRAM_T    : 'ENDPROGRAM'    | 'endprogram'    ;
MODULE_T        : 'MODULE'        | 'module'        ;
ENDMODULE_T     : 'ENDMODULE'     | 'endmodule'     ;
INTERFACE_T     : 'INTERFACE'     | 'interface'     ;
ENDINTERFACE_T  : 'ENDINTERFACE'  | 'endinterface'  ;
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
BIND_T          : 'BIND'          | 'bind'          ;
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

CODE_T
    : START_CODE .* STOP_CODE
    ;

fragment
START_CODE : '<#' ;

fragment
STOP_CODE : '#>' ;


STRING_T
    : '"' ('\\"'|~'"')* '"' 
    | '\'' ('\\\''|~'\'')* '\''
    ;

NUMBER_T
    : ('-')? DIGIT+
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
LEFT_SQUARE_T  : '['  ;
RIGHT_SQUARE_T : ']'  ;
AMPERSAND_T    : '&'  ;
DOUBLE_COLON_T : '::' ;
COLON_T        : ':'  ;
COMMA_T        : ','  ;
ARROW_T        : '=>' ;
STAR_T         : '*'  ;

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
