module CG
  class FortranFunction
    def initialize name, result_type, result_name=nil
      @name = name
      @result_type = result_type
      STDERR.puts "resultname: #{result_name}"
      @result_name = result_name.nil? ? @name : result_name
      @result_arg = result_name.nil? ? "" : " result(#{@result_name})"
      STDERR.puts "resultname: #{@result_arg}"
      @use = {}
      @vars = {}
      @args = {}
      @code = []
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
      unless @vars.empty?
        vs = "\n  " + @vars.values.join("\n  ")
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
  implicit none#{vs}#{cs}
end function #{@name}
EOF
    end

  end
end
