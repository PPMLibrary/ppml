module CG
  class Scope
    attr_reader :name, :use_statements, :variables, :includes, :child, :parent, :output_continue
    attr_accessor :indent, :body_indent

    def initialize name, parent=nil
      @name = name
      @use_statements = {}
      @variables = {}
      @includes = []
      @parent = parent
      @output_continue = false
      parent.set_child self if !parent.nil?
    end

    def use sym, str=nil
      @use_statements[sym] = str || "use #{sym.to_s}"
    end

    def var var
      @variables.merge! var
    end

    def insert_exit_point
      @output_continue = true
    end

    def include inc
      @includes << inc
    end

    def arg h
      use GlobalModule.module_name
      GlobalModule.instance.arg h
    end

    protected
    def set_child child
      @child = child
    end
  end
end
