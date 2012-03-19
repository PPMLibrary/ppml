require 'configatron'

module CG
  class Templates
    def self.get_all
      ANTLR3::Template::Group.new do
        define_template( :join,        "<%= @lines.join(\"\n\") %>\n")

        define_template( :prog,        <<-'ENDTEMPLATE')
% _erbout += @pre.join("\n") if !@pre.empty?
% _erbout += @prog.to_s if !@prog.nil?
% _erbout += @post.join("\n") if !@post.empty?
ENDTEMPLATE

        define_template( :scoped,      <<-'ENDTEMPLATE')
<%= @open  %>
% _erbout += @inner.to_s
<%= @close %>
ENDTEMPLATE

        define_template( :inner,       <<-'ENDTEMPLATE')
% _erbout += @use.join("\n") + "\n" if !@use.empty?
% _erbout += "#{@indent}! use statements\n" if configatron.comment_mode
% if ! @implicit.nil?
%   _erbout += @indent + @implicit.text
% else
%   _erbout += "#{@indent}implicit none\n"
% end
% if configatron.comment_mode
%   _erbout += "#{@indent}! interfaces\n"
% end
% if configatron.comment_mode
%   _erbout += "#{@indent}! variable definitions\n"
% end
% _erbout += @indent + @context.variables.values.join("\n#{@indent}") + "\n" if !@context.variables.empty?
% _erbout += @body.join("\n") + "\n" if !@body.empty?
% _erbout += @indent + @contains.text if !@contains.nil?
% _erbout += "#{@indent}! subroutines\n" if configatron.comment_mode
% _erbout += @subroutines.join('') if !@subroutines.empty?
ENDTEMPLATE

        define_template( :verbatim,    "<%= @in %>")

        define_template( :fcall_macro, <<-'ENDTEMPLATE')
<%= CG::Preprocessor.instance.expand(@name, @context, @result, @args,
        Hash[*@namedargs.map(&:to_s).zip(@namedvalues).flatten]) %>
ENDTEMPLATE
      end
    end
  end
end
