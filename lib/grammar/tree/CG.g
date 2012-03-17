tree grammar CG;

options {
language = Ruby;
tokenVocab = CG;
ASTLabelType = CommonTree;
output = template;
}

@header {
require_relative 'scope'
}

@members {
def find_hidden(lines=false)
  tree   = @input.look
  start  = tree.start_index
  stop   = tree.stop_index

  i = find_leading start
  find_empty_lines i if lines
  find_trailing stop

#  STDERR.puts "\n\nlooking before: #{start}" if lines
#  STDERR.puts "  found indent: '#{@current_indent}'"
#  STDERR.puts "  found empty lines: '#{@empty_lines}'" if lines
#  STDERR.puts "looking after: #{stop}"
#  STDERR.puts "  found comment: #{@trailing}"
end

def find_leading i
  t = Preprocessor.instance.tokens
  i -= 1
  if !t[i].nil? and t[i].channel == :hidden
    @current_indent = t[i].text
    i -= 1
  else
    @current_indent = ''
  end
  return i
end

def find_trailing i
  t = Preprocessor.instance.tokens
  i -= 1
  @trailing = ''
  if !t[i].nil? and t[i].channel == :hidden
    i -= 1
    if !t[i].nil? and t[i].channel == :hidden
      @trailing = t[i].text
    end
    @trailing += t[i+1].text
  end
end

class Line
  attr_accessor :start, :end, :text
  attr_writer :empty
  def empty?
    @empty
  end
  def initialize e
    @empty, @start, @end, @text = false, e, e, ''
  end
end

def find_empty_lines i
  t = Preprocessor.instance.tokens
  @empty_lines = ''
  l = previous_line i
  while l.empty?
    @empty_lines = l.text + @empty_lines
    l = previous_line(l.start - 1)
  end
end

def previous_line i
  t = Preprocessor.instance.tokens
  l = Line.new i
  return l if i<0 or t[i].nil?
  i -= 1
  l.empty = true
  while !t[i].nil? and t[i].type != NEWLINE
    l.empty = false if t[i].channel != :hidden
    l.start = i
    i -= 1
  end
  l.text = t[l.start..l.end].map { |tok| tok.text }.join('')
  l
end

def indent input
  if input.respond_to? :to_s
    string = input.to_s
  elsif input.respond_to? :text
    string = input.text
  end
  if !string.empty?
    lines = string.split("\n")
    lines = [''] if lines.empty?
    lines.map! { |l| @current_indent+l }
    lines.join("\n")
  end
end

def setup_scope
  find_hidden line=true
end

def cleanup_scope
  find_hidden line=true
end

}

prog
    : (naked_code)=> code=naked_code -> verbatim(in={$code.st})
    | (
            ((module_statement)=>pre+=module_statement
            |(subroutine_statement)=>pre+=subroutine_statement
            )*
            program=program_statement?
            (post+=module_statement
            |post+=subroutine_statement
            )*
            -> prog(pre={$pre},prog={$program.st},post={$post})
      )
    ;

naked_code : (l+=line)* -> join(lines={$l}) ;

program_statement
    : ^(PROGRAM o=scope_start
                c=scope_end
            i=inner_stuff)
            -> scoped(open={$o.st},close={$c.st},inner={$i.st})
    ;

module_statement
    : ^(MODULE o=scope_start
               c=scope_end
            i=inner_stuff)
            -> scoped(open={$o.st},close={$c.st},inner={$i.st})
    ;

subroutine_statement
    : ^(SUBROUTINE o=scope_start
                   c=scope_end
            i=inner_stuff)
            -> scoped(open={$o.st},close={$c.st},inner={$i.st})
    ;

inner_stuff
    : { find_hidden }
        ^(INNER_STUFF
            ^(USE u+=line*)
            ( ^(IMPLICIT i=TEXT) )?
            ( ^(CONTAINS n=TEXT)
              s+=subroutine_statement+ )?
          { @first_line = nil }
            b+=line*)
        -> inner(use={$u},implicit={$i},contains={$n},subroutines={$s},body={$b},indent={@first_line || ''})
    ;

scope_start
    : { setup_scope }
      ^(SCOPE_START name=ID text=TEXT)
      -> verbatim(in={@empty_lines + indent($text)})
    ;

scope_end
    : { cleanup_scope }
      ^(SCOPE_END text=TEXT)
      -> verbatim(in={@empty_lines + indent($text)})
    ;

line
    : { find_hidden; @first_line ||= @current_indent }
        ( macro=fcmacro -> verbatim(in={indent($macro.st)})
        | fortran=fline -> verbatim(in={indent($fortran.st)})
        )
    ;

fcmacro	: ^(FMACRO n=ID r=ID? ^(ARGS a+=value* ^(NAMEDARGS na+=ID*) ^(NAMEDARGS v+=value*)))
          -> fcall_macro(p={@preprocessor},name={$n},result={$r},args={$a},namedargs={$na},namedvalues={$v}) ;

fline	: ^(FLINE c=TEXT) -> verbatim(in={$c.text}) ;

value
    : i=ID     -> verbatim(in={$i})
    | n=NUMBER -> verbatim(in={$n})
    | s=STRING -> verbatim(in={$s})
    ;
