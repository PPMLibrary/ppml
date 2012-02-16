tree grammar CG;

options {
language = Ruby;
tokenVocab = CG;
ASTLabelType = CommonTree;
output = template;
//rewrite = true;
}

@members{
attr_accessor :preprocessor
}

prog 	: (l+=line)* -> prog(lines={$l}) ;

line
    : macro=fcmacro -> line(in={$macro.st})
    | fortran=fline -> line(in={$fortran.st})
    ;

fcmacro	: ^(FMACRO n=ID r=ID? ^(ARGS a+=ID*) ) -> fcall_macro(p={@preprocessor},name={$n},result={$r},args={$a}) ;
fline	: FLINE -> fortran(in={$FLINE.text}) ;
