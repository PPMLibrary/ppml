require 'configatron'

module CG
  class Templates
    def self.get_all
      ANTLR3::Template::Group.new do
        define_template( :join,        "<%= @lines.join('') %>")

        define_template( :prog,        <<-'ENDTEMPLATE')
% _erbout += @pre.join("")  unless @pre.empty?
% _erbout += @prog.to_s     unless @prog.nil?
% _erbout += @post.join("") unless @post.empty?
% _erbout += @trailing      unless @trailing.empty?
ENDTEMPLATE

        define_template( :scoped,      <<-'ENDTEMPLATE')
% _erbout += @open.to_s
% _erbout += @inner.to_s
% _erbout += @close.to_s
ENDTEMPLATE

        define_template( :inner,       <<-'ENDTEMPLATE')
% _erbout += @use.join("")  unless @use.empty?
% _erbout += "#{@indent}! use statements\n" if configatron.comment_mode
% _erbout += @indent + @context.use_statements.join("\n#{@indent}") + "\n" unless @context.use_statements.empty?
% unless @implicit.nil?
%   _erbout += @implicit.to_s
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
% _erbout += @body.join("")              unless @body.empty?
% _erbout += @contains.to_s              unless @contains.nil?
% _erbout += "#{@indent}! subroutines\n" if configatron.comment_mode
% _erbout += @subroutines.join("")       unless @subroutines.empty?
ENDTEMPLATE

        define_template( :verbatim,    "<%= @in %>")

        define_template( :fcall_macro, <<-'ENDTEMPLATE')
<%= CG::Preprocessor.instance.expand(@name, @context, @result, @args.pos, @args.named) %>


ENDTEMPLATE
      end
    end
  end
end
