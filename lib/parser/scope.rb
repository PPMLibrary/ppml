module CG
  class Scope
    attr_reader :kind, :name, :use_statements, :variables, :includes, :child, :parent, :output_continue
    attr_accessor :indent, :body_indent

    def initialize kind, name, parent=nil
      @kind = kind
      @name = name
      @use_statements = {}
      @variables = {}
      @includes = []
      @parent = parent
      @output_continue = false
      @types = Hash.new nil
      parent.set_child self if !parent.nil?
    end

    def use sym, str=nil
      @use_statements[sym] = str || "use #{sym.to_s}"
    end

    def var var, type=nil
      @types[var.keys[0]] = type if type
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

    def type_of sym
      @types[sym]
    end

    protected
    def set_child child
      @child = child
    end
  end
end
