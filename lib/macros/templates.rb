require 'configatron'

module CG
  class Templates
    def self.get_all
      ANTLR3::Template::Group.new do
        define_template( :join,        "<%= @lines.join('') %>")

        define_template( :prog,        <<-'ENDTEMPLATE')
% _erbout += @scopes.join("") unless @scopes.empty?
% _erbout += @trailing        unless @trailing.empty?
ENDTEMPLATE

        define_template( :scoped,      <<-'ENDTEMPLATE')
% _erbout += @open.to_s
% _erbout += @inner.to_s
% _erbout += @close.to_s
ENDTEMPLATE

        define_template( :rhs,         <<-'ENDTEMPLATE')
% CG::RHSModule.instance.definition @name.to_s, @args, @ret, @inner
ENDTEMPLATE

        define_template( :inner,       <<-'ENDTEMPLATE')
% _erbout += @use.join("")  unless @use.empty?
% _erbout += "#{@indent}! use statements\n" if conf.comment_mode
% _erbout += @indent + @context.use_statements.values.join("\n#{@indent}") + "\n" unless @context.use_statements.empty?
% unless @implicit.nil?
%   _erbout += @implicit.to_s
% else
%   _erbout += "#{@indent}implicit none\n"
% end
% unless @context.includes.empty?
%   _erbout += "#{@indent}include '" + @context.includes.uniq.join("'\n#{indent}include '") + "'\n"
% end
% if conf.comment_mode
%   _erbout += "#{@indent}! interfaces\n"
% end
% if conf.comment_mode
%   _erbout += "#{@indent}! variable definitions\n"
% end
% _erbout += @indent + @context.variables.values.join("\n#{@indent}") + "\n" unless @context.variables.empty?
% _erbout += @body.join("")              unless @body.empty?
% _erbout += "9999 continue\n"           if @context.output_continue
% _erbout += @contains.to_s              unless @contains.nil?
% _erbout += "#{@indent}! subroutines\n" if conf.comment_mode
% _erbout += @subroutines.join("")       unless @subroutines.empty?
ENDTEMPLATE

        define_template( :type_inner,       <<-'ENDTEMPLATE')
% if conf.comment_mode
%   #_erbout += "#{@indent}! type member definitions\n"
% end
% _erbout += @indent + @context.variables.values.join("\n#{@indent}") + "\n" unless @context.variables.empty?
% _erbout += @body.join("")              unless @body.empty?
% _erbout += @contains.to_s              unless @contains.nil?
% #_erbout += "#{@indent}! procedures\n" if conf.comment_mode
% _erbout += @procedures.join("")       unless @procedures.empty?
        ENDTEMPLATE

        define_template( :verbatim,    "<%= @in %>")

        define_template( :fcall_macro, <<-'ENDTEMPLATE')
<%= CG::Preprocessor.instance.expand(@name, @context, @result, @args.pos, @args.named, @dotarg) %>
ENDTEMPLATE

        define_template( :include_macro, <<-'ENDTEMPLATE')
<%= CG::Preprocessor.instance.expand(@name, @context, @args.pos, @args.named) %>
ENDTEMPLATE

        define_template( :foreach, <<-'ENDTEMPLATE')
<%= CG::Preprocessor.instance.expand(@name, @context, @iter, @args.pos, @args.named, @mods && @mods.map(&:to_s), @modargs && @modargs.map(&:pos), @modargs && @modargs.map(&:named), @bodies.map(&:to_s)) %>
ENDTEMPLATE

        define_template( :timeloop, <<-'ENDTEMPLATE')
% _erbout += CG::Preprocessor.instance.expand("timeloop", @context, @body)
ENDTEMPLATE
      end
    end
  end
end
