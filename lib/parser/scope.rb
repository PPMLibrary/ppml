module CG
  class Scope
    attr_reader :name, :use_statements
    attr_accessor :indent, :body_indent

    def initialize name
      @name = name
      @use_statements = []
    end

    def use str
      @use_statements << "use " + str
    end

    def self.program name
      ProgramScope.new name
    end
  end

  class ProgramScope < Scope
  end
end
