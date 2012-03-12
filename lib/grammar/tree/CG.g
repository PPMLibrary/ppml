tree grammar CG;

options {
language = Ruby;
tokenVocab = CG;
ASTLabelType = CommonTree;
output = template;
//rewrite = true;
}

@members {
attr_accessor :preprocessor, :stream

def find_hidden
  tree   = @input.look
  start  = tree.start_index
  stop   = tree.stop_index

  find_leading  start
  find_trailing stop
end

def find_leading i
  i -= 1
  if !@stream[i].nil? and @stream[i].channel == :hidden
    @current_indent = @stream[i].text
    i -= 1
  else
    @current_indent = ''
  end
  hidden = []
end

def find_trailing i
  i -= 1
  hidden = []
  @trailing = ''
  if ! @stream[i].nil? and @stream[i].channel == :hidden
    i -= 1
    if ! @stream[i].nil? and @stream[i].channel == :hidden
      @trailing = @stream[i].text
    end
    @trailing += @stream[i+1].text
  end
end

def wrap input
  if input.respond_to? :to_s
    string = input.to_s
  elsif input.respond_to? :text
    string = input.text
  end
  if !string.empty?
    lines = string.split("\n")
    lines[0] += @trailing
    lines.map! { |l| @current_indent+l }
    lines.join("\n")
  else
    @trailing
  end
end
}

prog 	: (l+=line)* -> prog(lines={$l}) ;

line
    : { find_hidden }
        ( macro=fcmacro -> line(in={wrap($macro.st)})
        | fortran=fline -> line(in={wrap($fortran.st)})
        )
    ;

fcmacro	: ^(FMACRO n=ID r=ID? ^(ARGS a+=value*) ^(NAMEDARGS na+=ID*) ^(NAMEDARGS v+=value*))
          -> fcall_macro(p={@preprocessor},name={$n},result={$r},args={$a},namedargs={$na},namedvalues={$v}) ;

fline	: ^(FLINE c=TEXT) -> fortran(in={$c.text}) ;

value
    : i=ID     -> template(i={$i}) "<%= i %>"
    | n=NUMBER -> template(n={$n}) "<%= n %>"
    | s=STRING -> template(s={$s}) "<%= s %>"
    ;
