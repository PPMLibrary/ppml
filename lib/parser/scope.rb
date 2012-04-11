module CG
  class Scope
    attr_reader :name, :use_statements, :variables, :includes, :child, :parent
    attr_accessor :indent, :body_indent

    def initialize name, parent=nil
      @name = name
      @use_statements = []
      @variables = {}
      @includes = []
      @parent = parent
      parent.set_child self if !parent.nil?
    end

    def use str
      @use_statements << "use " + str
    end

    def var var
      @variables.merge! var
    end

    def include inc
      @includes << inc
    end

    protected
    def set_child child
      @child = child
    end
  end
end
