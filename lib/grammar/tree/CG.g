tree grammar CG;

options {
language = Ruby;
tokenVocab = CG;
ASTLabelType = CommonTree;
output = template;
//rewrite = true;
}

@header {
require_relative 'scope'
}

@members {
attr_accessor :preprocessor, :stream

def find_hidden
  tree   = @input.look
  start  = tree.start_index
  stop   = tree.stop_index

  find_leading  start
  find_trailing stop

  STDERR.puts "looking before: #{start}"
  STDERR.puts "  found indent: #{@current_indent}"
  STDERR.puts "looking after: #{stop}"
  STDERR.puts "  found comment: #{@trailing}"
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

def indent input
  if input.respond_to? :to_s
    string = input.to_s
  elsif input.respond_to? :text
    string = input.text
  end
  if !string.empty?
    lines = string.split("\n")
    lines.map! { |l| @current_indent+l }
    lines.join("\n")
  else
    @trailing
  end
end

}

prog
    : ( code=program_statement
      | code=naked_code ) -> verbatim(in={$code.st})
    ;

naked_code : (l+=line)* -> join(lines={$l}) ;

program_statement
    : ^(PROGRAM o=scope_start c=scope_end b+=line*) -> scoped(open={$o.st},close={$c.st},body={$b})
    ;

scope_start
    : { find_hidden }
      ^(SCOPE_START name=ID text=TEXT)
      -> verbatim(in={indent($text)})
    ;

scope_end
    : { find_hidden }
      ^(SCOPE_END text=TEXT)
      -> verbatim(in={indent($text)})
    ;

indented
    : { find_hidden }
        t=TEXT -> verbatim(in={wrap($t.text)})
    ;

line
    : { find_hidden }
        ( macro=fcmacro -> verbatim(in={wrap($macro.st)})
        | fortran=fline -> verbatim(in={wrap($fortran.st)})
        )
    ;

fcmacro	: ^(FMACRO n=ID r=ID? ^(ARGS a+=value*) ^(NAMEDARGS na+=ID*) ^(NAMEDARGS v+=value*))
          -> fcall_macro(p={@preprocessor},name={$n},result={$r},args={$a},namedargs={$na},namedvalues={$v}) ;

fline	: ^(FLINE c=TEXT) -> verbatim(in={$c.text}) ;

value
    : i=ID     -> verbatim(in={$i})
    | n=NUMBER -> verbatim(in={$n})
    | s=STRING -> verbatim(in={$s})
    ;
