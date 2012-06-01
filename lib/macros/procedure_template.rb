require_relative 'transform'

module CG
  class ProcedureTemplate
    def initialize vars, cartesian, name, open, inner, close
      if cartesian
        @combinations = ProcedureTemplate.cartesian_vars vars
      else
        @combinations = ProcedureTemplate.matched_vars vars
      end
      @name  = name.to_str
      @open  = open
      @inner = inner
      @close = close
    end

    # Transform a hashtable storing lists of values into a list
    # of hashtables whose keys are all possible combinations of
    # the input lists.
    #
    # @param vars [Hash] A hashtable storing lists of values for each key
    # @return [Array] A list of hash tables where keys map to all combinations of the supplied values
    def self.cartesian_vars vars
      combinations = [{}]
      vars.each do |var, type_list|
        newc = []
        type_list.each do |type|
          combinations.each do |c|
            c[var] = type
            newc << c.dup
          end
        end
        combinations = newc
      end
      combinations
    end

    def self.matched_vars vars
      tuples = []
      first = true
      vars.each do |var, types|
        if first
          types.each_with_index do |t,i|
            tuples << {var => t}
          end
          first = false
        else
          types.each_with_index do |t,i|
            tuples[i][var] = t
          end
        end
      end
      tuples
    end

    def update_scope scope
      @combinations.each do |comb|
        scope.interface @name, "module procedure #{@name}_#{name_sufix(comb)}"
      end
    end

    def to_str
      result = ""
      @combinations.each do |comb|
        nameupdate = Transform.new @name, "#{@name}_#{name_sufix(comb)}"
        typeupdate = MultiTransform.new comb
        result += "\n"
        result += nameupdate.transform @open
        result += typeupdate.transform @inner
        result += nameupdate.transform @close
      end
      result
    end

    def name_sufix comb
      comb.values.map { |t| t.gsub(/\(|\)/,"_") }.join '_'
    end
  end
end
