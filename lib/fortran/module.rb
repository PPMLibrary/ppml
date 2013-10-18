module CG
  class FortranModule
    def initialize name
      @name = name
      @use  = {}
      @vars = {}
      @subs = []
    end

    def use sym, str=nil
      @use[sym] = str || "USE #{sym.to_s}"
    end

    def var sym, str
      @vars[sym] = str
    end

    def subroutine sub
      @subs << sub
    end

    def function func
      @subs << func
    end

    def to_s
      us = ""
      us = "\n  " + @use.values.join("\n  ") unless @use.empty?
      vs = ""
      vs = "\n  " + @vars.values.join("\n  ") unless @vars.empty?
      ss = ""
      ss = "\ncontains\n" + @subs.map(&:to_s).join("\n") unless @subs.empty?
      ss.gsub! /\n/, "\n  "
      <<EOF
MODULE #{@name}#{us}
  IMPLICIT NONE#{vs}#{ss}
END MODULE #{@name}
EOF
    end
  end
end
