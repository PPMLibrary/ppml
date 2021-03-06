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
% if @template
%   t = CG::ProcedureTemplate.new @template, @cart, @name, @open.to_s, @inner.to_s, @close.to_s, @suffixes
%   if @iface
%     t.update_scope @context
%   end
%   _erbout += t
% else
%   _erbout += @open.to_s
%   _erbout += @inner.to_s
%   _erbout += @close.to_s
% end
ENDTEMPLATE

        define_template( :rhs,         <<-'ENDTEMPLATE')
% CG::RHSModule.instance.definition @name.to_s, @args, @ret, @pre, @post, @context
ENDTEMPLATE

        define_template( :inner,       <<-'ENDTEMPLATE')
% _temp_subs = @subroutines.join("")       unless @subroutines.empty?
% unless @context.kind == :interface
%   _erbout += @use.join("")  unless @use.empty?
%   _erbout += "#{@indent}! use statements\n" if conf.comment_mode
%   _erbout += @context.use_statements(@indent)
% end
% unless @context.kind == :interface or (!@context.parent.nil? and @context.parent.kind == :interface)
%   unless @implicit.nil?
%     _erbout += @implicit.to_s
%   else
%     _erbout += "#{@indent}IMPLICIT NONE\n"
%   end
% end
% unless @context.includes.empty?
%   _erbout += @context.include_statements(@indent)
% end
% unless @context.kind == :interface or (!@context.parent.nil? and @context.parent.kind == :interface)
%   if conf.comment_mode
%     _erbout += "#{@indent}! variable definitions\n"
%   end
% end
% _erbout += @context.var_statements(@indent)
% if @context.kind == :module
%   _erbout += @context.interfaces @indent
% end
% _erbout += @body.join("")                unless @body.empty?
% unless @context.kind == :interface or (!@context.parent.nil? and @context.parent.kind == :interface)
%   _erbout += "9999 CONTINUE\n"           if @context.output_continue
%   _erbout += @contains.to_s              unless @contains.nil?
%   _erbout += "#{@indent}! SUBROUTINES\n" if conf.comment_mode
%   _erbout += _temp_subs                  unless @subroutines.empty?
% end
ENDTEMPLATE

        define_template( :type_inner,       <<-'ENDTEMPLATE')
% if conf.comment_mode
%   #_erbout += "#{@indent}! type member definitions\n"
% end
% _erbout += @indent + @context.variables.values.join("\n#{@indent}") + "\n" unless @context.variables.empty?
% _erbout += @body.join("")              unless @body.empty?
% _erbout += @contains.to_s              unless @contains.nil?
% #_erbout += "#{@indent}! PROCEDURES\n" if conf.comment_mode
% _erbout += @procedures.join("")       unless @procedures.empty?
        ENDTEMPLATE

        define_template( :verbatim,    "<%= @in %>")

        define_template( :client, <<-'ENDTEMPLATE')
<%= @in.gsub(/client/i, "PROGRAM") %>
ENDTEMPLATE

        define_template( :fcall_macro, <<-'ENDTEMPLATE')
<%= CG::Preprocessor.instance.expand(@name, @context, @results.args, @args.pos, @args.named) %>
ENDTEMPLATE

        define_template( :include_macro, <<-'ENDTEMPLATE')
<%= CG::Preprocessor.instance.expand(@name, @context, @args.pos, @args.named) %>
ENDTEMPLATE

        define_template( :foreach, <<-'ENDTEMPLATE')
<%= CG::Preprocessor.instance.expand(@name, @context, @iter, @args.pos, @args.named, @mods && @mods.map(&:to_s), @modargs && @modargs.map(&:pos), @modargs && @modargs.map(&:named), @bodies) %>
ENDTEMPLATE

        define_template( :timeloop, <<-'ENDTEMPLATE')
% _erbout += CG::Preprocessor.instance.expand("timeloop", @context, @time, @tparams.pos, @tparams.named, @body)
ENDTEMPLATE
      end
    end
  end
end
