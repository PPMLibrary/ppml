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

def find_hidden
  t      = Preprocessor.instance.tokens
  tree   = @input.look
  i      = tree.start_index

  i -= 1
  if !t[i].nil? and t[i].channel == :hidden
    @current_indent = t[i].text
    i -= 1
  else
    @current_indent = ''
  end

  find_empty_lines i
end

def find_empty_lines i
  t = Preprocessor.instance.tokens
  @empty_lines = ''
  stop = i
  return if stop < 0
  i -= 1 while i >= 0 and !t[i].nil? and t[i].type == EMPTY_LINE
  @empty_lines = t.extract_text(i+1,stop)
end

def trailing_lines
  t = Preprocessor.instance.tokens
  @trailing = ''
  stop = i = t.length - 1
  i -= 1 while i >= 0 and !t[i].nil? and t[i].type == EMPTY_LINE
  @trailing = t.extract_text(i+1,stop) if i < stop
end

def indent string
  if !string.empty?
    lines = string.split("\n")
    lines = [''] if lines.empty?
    lines.map! { |l| @current_indent + l }
    lines.join("\n") + "\n"
  end
end

def setup_scope
  find_hidden
  @scope = CG::Scope.new(@input.look(3).text,@scope)
end

def cleanup_scope
  find_hidden
  @scope = @scope.parent
end

}

// Entry point

prog
    : (naked_code)=> code=naked_code { trailing_lines } -> verbatim(in={$code.st.to_s + @trailing})
    | (
            ((module_statement)=>pre+=module_statement
            |(subroutine_statement)=>pre+=subroutine_statement
            |(function_statement)=>pre+=function_statement
            )*
            program=program_statement?
            (post+=module_statement
            |post+=subroutine_statement
            |post+=function_statement
            )*
            { trailing_lines }
            -> prog(pre={$pre},prog={$program.st},post={$post},trailing={@trailing})
      )
    ;

// Top level constructs

naked_code : (l+=line)* -> join(lines={$l}) ;

program_statement
    : ^(PROGRAM o=scope_start
                i=inner_stuff
                c=scope_end)
            -> scoped(open={$o.st},close={$c.st},inner={$i.st})
    ;

module_statement
    : ^(MODULE o=scope_start
               i=inner_stuff
               c=scope_end)
            -> scoped(open={$o.st},close={$c.st},inner={$i.st})
    ;

subroutine_statement
    : ^(SUBROUTINE o=scope_start
                   i=inner_stuff
                   c=scope_end)
            -> scoped(open={$o.st},close={$c.st},inner={$i.st})
    ;

function_statement
    : ^(FUNCTION o=scope_start
                 i=inner_stuff
                 c=scope_end)
            -> scoped(open={$o.st},close={$c.st},inner={$i.st})
    ;

// Scope handling

scope_start
    : { setup_scope }
      ^(SCOPE_START name=ID text=TEXT)
      -> verbatim(in={@empty_lines + indent($text.text)})
    ;

inner_stuff
    : { find_hidden }
        ^(INNER_STUFF
            ^(USE u+=line*)
            ( i=implicit_line )?
            ^(CONTAINS c=contains_line?
                (s+=subroutine_statement
                |s+=function_statement
                )*)
          { @first_line = nil }
            b+=line*)
        -> inner(context={@scope},use={$u},implicit={$i.st},contains={$c.st},subroutines={$s},body={$b},indent={@first_line || ''})
    ;

implicit_line : { find_hidden } ^(IMPLICIT i=TEXT) -> verbatim(in={@empty_lines + indent($i.text)}) ;
contains_line : { find_hidden } ^(CONTAINS c=TEXT) -> verbatim(in={@empty_lines + indent($c.text)}) ;

scope_end
    : { cleanup_scope }
      ^(SCOPE_END text=TEXT)
      -> verbatim(in={@empty_lines + indent($text.text)})
    ;

// Actual code

line
    : { find_hidden
       @first_line ||= @current_indent }
        ( macro=fcmacro -> verbatim(in={@empty_lines + indent($macro.st.to_s)})
        | loop=foreach  -> verbatim(in={@empty_lines + indent($foreach.st.to_s)})
        | fortran=fline -> verbatim(in={@empty_lines + indent($fortran.st.to_s)})
        | func=function_statement  -> verbatim(in={@empty_lines + $func.st.to_s})
        | sub=subroutine_statement -> verbatim(in={@empty_lines + $sub.st.to_s})
        )
    ;

fcmacro
    : ^(FMACRO n=ID r=ID?
            a=arglist)
          -> fcall_macro(name={$n},context={@scope},result={$r},args={a})
    ;

foreach
    : ^(FOREACH n=ID it=ID a=arglist?
            ^(MODIFIERS m+=ID* ma+=arglist*)
            b+=foreach_body*)
      -> foreach(context={@scope},name={$n},iter={$it},args={a},ms={$m},ma={$ma},bodies={$b})
    ;

foreach_body : ^(BODY b+=line*) ;

arglist returns [pos,named]
    :  ^(ARGS a+=value*
            ^(NAMEDARGS na+=ID*)
            ^(NAMEDARGS v+=value*))
      { $pos = $a
        $named = Hash[*$na.map(&:to_s).zip($v).flatten] }
    ;

fline : ^(FLINE c=TEXT) -> verbatim(in={$c.text}) ;

value
    : i=ID     -> verbatim(in={$i})
    | n=NUMBER -> verbatim(in={$n})
    | s=STRING -> verbatim(in={$s})
    ;
