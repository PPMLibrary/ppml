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

def wrap(input, indent, comment)
  if input.respond_to?(:to_s)
    string = input.to_s
  elsif input.respond_to?(:text)
    string = input.text
  end
#  STDERR.puts "received indent : '#{indent.text}' (#{indent.text.length})"
  if !string.empty?
    lines = string.split("\n")
    lines[0] += comment.text if lines[0] and comment
    lines.map! { |l| indent.text+l } if indent
    lines.join("\n")
  else
    comment
  end
end

}

prog 	: (l+=wrapper)* -> prog(lines={$l}) ;

wrapper : ^(LINE l=line i=INDENT c=COMMENT) -> template(l={wrap($l.st,$i,$c)}) "<%= l %>" ;

line
    : macro=fcmacro -> line(in={$macro.st})
    | fortran=fline -> line(in={$fortran.st})
    ;

fcmacro	: ^(FMACRO n=ID r=ID? ^(ARGS a+=ID*) ^(NAMEDARGS na+=ID*) ^(NAMEDARGS v+=value*))
          -> fcall_macro(p={@preprocessor},name={$n},result={$r},args={$a},namedargs={$na},namedvalues={$v}) ;
fline	: ^(FLINE c=TEXT) -> fortran(in={$c.text}) ;

value : ID | NUMBER | STRING ;
