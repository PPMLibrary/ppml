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

  return @current_indent, @empty_lines
end

def find_empty_lines i
  t = Preprocessor.instance.tokens
  @empty_lines = ''
  stop = i
  return if stop < 0
  i -= 1 while i >= 0 and !t[i].nil? and t[i].type == EMPTY_LINE_T
  @empty_lines = t.extract_text(i+1,stop)
end

def trailing_lines
  t = Preprocessor.instance.tokens
  @trailing = ''
  stop = i = t.length - 1
  i -= 1 while i >= 0 and !t[i].nil? and t[i].type == EMPTY_LINE_T
  @trailing = t.extract_text(i+1,stop) if i < stop
end

def indent string, prefix=nil
  prefix ||= @current_indent
  if !string.empty?
    lines = string.split("\n")
    lines = [''] if lines.empty?
    lines.map! { |l| prefix + l }
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
            |(imacro)=>pre+=imacro
            )*
            program=program_statement?
            (post+=module_statement
            |post+=subroutine_statement
            |post+=function_statement
            |post+=imacro
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

type_statement
    : ^(TYPE o=scope_start
               i=type_body
               c=scope_end)
            -> scoped(open={$o.st},close={$c.st},inner={$i.st})
    ;

// Scope handling

scope_start
    : { setup_scope }
      ^(SCOPE_START name=ID_T text=TEXT)
      -> verbatim(in={@empty_lines + @current_indent + $text.text})
    ;

inner_stuff
    : { find_hidden }
        ^(INNER_STUFF
            ^(USE u+=line*)
            ^(IMPORT u+=line*)
            ( i=implicit_line )?
            ^(CONTAINS c=contains_line?
                (s+=subroutine_statement
                |s+=function_statement
                |s+=procedure_statement
                |s+=imacro
                )*)
          { @first_line = nil }
            (b+=inner_line)*)
        -> inner(context={@scope},use={$u},implicit={$i.st},contains={$c.st},subroutines={$s},body={$b},indent={@first_line || ''})
    ;

type_body
    : { find_hidden }
        ^(TYPE_BODY
            ^(CONTAINS (c=contains_line
             (s+=procedure_statement
             |s+=generic_statement
             |s+=inner_line)+)?)
             //|s+=imacro)+)?)
          { @first_line = nil }
            (b+=inner_line)*)
        -> type_inner(context={@scope},contains={$c.st},procedures={$s},body={$b},indent={@first_line || ''})
    ;

implicit_line : { find_hidden } ^(IMPLICIT i=TEXT) -> verbatim(in={@empty_lines + @current_indent + $i.text}) ;
contains_line : { find_hidden } ^(CONTAINS c=TEXT) -> verbatim(in={@empty_lines + @current_indent + $c.text}) ;
procedure_statement : { find_hidden } ^(PROCEDURE c=TEXT) -> verbatim(in={@empty_lines + @current_indent + $c.text}) ;
generic_statement : { find_hidden } ^(GENERIC c=TEXT) -> verbatim(in={@empty_lines + @current_indent + $c.text}) ;

scope_end
    : { cleanup_scope }
      ^(SCOPE_END text=TEXT)
      {@first_line = @current_indent}
      -> verbatim(in={@empty_lines + @current_indent + $text.text})
    ;

// Actual code

inner_line
    : { find_hidden
       @first_line ||= @current_indent }
        ( l=line -> verbatim(in={$l.st.to_s})
        | tdef=type_statement -> verbatim(in={@empty_lines + $tdef.st.to_s})
        )
    ;


line
    : { my_indent, my_empty = find_hidden
       @first_line ||= my_indent }
        ( fmac=fcmacro -> verbatim(in={@empty_lines + indent($fmac.st.to_s)})
        | loop=foreach -> verbatim(in={my_empty + indent($foreach.st.to_s, my_indent)})
        | imac=imacro  -> verbatim(in={@empty_lines + indent($imac.st.to_s)})
        | fortran=fline -> verbatim(in={@empty_lines + @current_indent + $fortran.st.to_s})
        | func=function_statement  -> verbatim(in={@empty_lines + $func.st.to_s})
        | sub=subroutine_statement -> verbatim(in={@empty_lines + $sub.st.to_s})
        )
    ;

fcmacro
    : ^(FMACRO n=ID_T r=ID_T?
            a=arglist d=ID_T?)
          -> fcall_macro(name={$n},context={@scope},result={$r},args={a},dotarg={$d})
    ;

imacro
    : ^(IMACRO n=ID_T
            a=arglist)
          -> include_macro(name={$n},context={@scope},args={a})
    ;

foreach
    : ^(FOREACH n=ID_T it=ID_T a=arglist?
            ^(MODIFIERS m+=ID_T* ma+=arglist*)
            b+=foreach_body*)
      -> foreach(name={$n.text},context={@scope},iter={$it.text},args={a},mods={$m},modargs={$ma},bodies={$b})
    ;

foreach_body : ^(BODY l+=line*) -> join(lines={$l}) ;

arglist returns [pos,named]
    :  ^(ARGS a+=value*
            ^(NAMEDARGS na+=ID_T*)
            ^(NAMEDARGS v+=value*))
      { $pos = $a
        $named = Hash[*$na.map(&:to_s).zip($v).flatten] }
    ;

fline : ^(FLINE c=TEXT) -> verbatim(in={$c.text}) ;

value
    : i=ID_T     -> verbatim(in={$i})
    | n=NUMBER_T -> verbatim(in={$n})
    | s=STRING_T -> verbatim(in={$s})
    ;
