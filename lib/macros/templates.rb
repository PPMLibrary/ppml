require 'configatron'

module CG
  class Templates
    def self.get_all
      ANTLR3::Template::Group.new do
        define_template( :join,        "<%= @lines.join(\"\n\") %>\n")

        define_template( :prog,        <<-'ENDTEMPLATE')
% _erbout += @pre.join("\n") unless @pre.empty?
% _erbout += @prog.to_s unless @prog.nil?
% _erbout += @post.join("\n") unless @post.empty?
% _erbout += @trailing unless @trailing.empty?
ENDTEMPLATE

        define_template( :scoped,      <<-'ENDTEMPLATE')
<%= @open  %>
% _erbout += @inner.to_s
<%= @close %>
ENDTEMPLATE

        define_template( :inner,       <<-'ENDTEMPLATE')
% _erbout += @use.join("\n") + "\n" unless @use.empty?
% _erbout += "#{@indent}! use statements\n" if configatron.comment_mode
% _erbout += @indent + @context.use_statements.join("\n#{@indent}") + "\n" if !@context.use_statements.empty?
% unless @implicit.nil?
%   _erbout += @implicit.to_s + "\n"
% else
%   _erbout += "#{@indent}implicit none\n"
% end
% if configatron.comment_mode
%   _erbout += "#{@indent}! interfaces\n"
% end
% if configatron.comment_mode
%   _erbout += "#{@indent}! variable definitions\n"
% end
% _erbout += @indent + @context.variables.values.join("\n#{@indent}") + "\n" unless @context.variables.empty?
% _erbout += @body.join("\n") + "\n"     unless @body.empty?
% _erbout += @contains.to_s + "\n"       unless @contains.nil?
% _erbout += "#{@indent}! subroutines\n" if configatron.comment_mode
% _erbout += @subroutines.join('')       unless @subroutines.empty?
ENDTEMPLATE

        define_template( :verbatim,    "<%= @in %>")

        define_template( :fcall_macro, <<-'ENDTEMPLATE')
<%= CG::Preprocessor.instance.expand(@name, @context, @result, @args.pos, @args.named) %>
ENDTEMPLATE
      end
    end
  end
end
