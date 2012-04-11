module CG
  class FortranSubroutine
    def initialize name
      @name = name
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
      cs = ""
      unless @code.empty?
        cs = "\n" + @code.join("\n")
        cs.gsub! /\n/, "\n  "
      end
      <<EOF
subroutine #{@name}#{args}#{us}
  implicit none#{vs}#{cs}
end subroutine #{@name}
EOF
    end

  end
end
