module CG
  class Scope
    attr_reader :name, :use_statements, :variables, :child, :parent
    attr_accessor :indent, :body_indent

    def initialize name, parent=nil
      @name = name
      @use_statements = []
      @variables = {}
      @parent = parent
      parent.set_child self if !parent.nil?
    end

    def use str
      @use_statements << "use " + str
    end

    def var var
      @variables.merge! var
    end

    protected
      def set_child child
        @child = child
      end

  end

end
