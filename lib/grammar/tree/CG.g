tree grammar CG;

options {
language = Ruby;
tokenVocab = CG;
ASTLabelType = CommonTree;
output = template;
//rewrite = true;
}

//prog	: all+=.*  { $all.each { |t| puts t.text }  } ;
prog 	: (fcmacro | fline)* ;

fcmacro	: ^(FMACRO name=ID result=ID? ) -> fcall_macro(name={$name},res={$result}) ;
fline	: FLINE -> template() "some test text";

