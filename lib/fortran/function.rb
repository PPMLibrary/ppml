module CG
  class FortranFunction
    attr_accessor :print_continue

    def initialize name, result_type, result_name=nil, context=nil
      @name = name
      @result_type = result_type
      @result_name = result_name.nil? ? @name : result_name
      @result_arg = result_name.nil? ? "" : " result(#{@result_name})"
      @use = {}
      @vars = {}
      @args = {}
      @code = []
      @print_continue = false
      @context = context
    end

    def use sym, str=nil
      @use[sym] = str || "use #{sym.to_s}"
    end

    def var sym, str
      @vars[sym] = str
    end

    def args hash
      @args = hash
    end

    def add text
      @code << text
    end

    def to_s
      us = ""
      vs = ""
      unless @use.empty?
        us = "\n  " + @use.values.join("\n  ")
      end
      if @context
        cuse = @context.use_statements("  ")
        us = us + "\n" + cuse[0...cuse.length-1].to_s if cuse.length != 0
      end
      unless @vars.empty?
        vs = "\n  " + @vars.values.join("\n  ")
      end
      if @context
        cvars = @context.var_statements("  ")
        vs = vs + "\n" + cvars[0...cvars.length-1].to_s if cvars.length != 0
      end
      args = ""
      unless @args.empty?
        args = "(" + @args.keys.map(&:to_s).join(", ") + ")" 
        vs = "\n  " + @args.values.join("\n  ") + vs
      end
      args = args + @result_arg
      vs = "\n  #{@result_type} :: #{@result_name}" + vs
      cs = ""
      unless @code.empty?
        cs = "\n" + @code.join("\n")
        cs.gsub! /\n/, "\n  "
      end
      <<EOF
function #{@name}#{args}#{us}
  implicit none#{vs}#{cs}#{"\n9999 continue" if @print_continue}
end function #{@name}
EOF
    end

  end
end
