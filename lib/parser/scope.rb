module CG

  # Represents the current scope in the prepocessed code. Provides methods for
  # adding various declarations to the scope when generating its code.
  class Scope
    attr_reader :kind, :name, :use_statements, :variables, :includes, :child, :parent
    attr_accessor :indent, :body_indent, :line, :interfaces, :output_continue

    # Creates a new scope.
    #
    # @param [Symbol] kind create a scope of this kind, (e.g. :program, :module, :subroutine, :interface, ...)
    # @param [String] name the scope's if available
    # @param [Scope] parent to be given if this is not a top-level scope
    def initialize kind, name, parent=nil
      @kind = kind
      @name = name
      @use_statements = {}
      @interfaces = Hash.new []
      @variables = {}
      @unmangled = {}
      @includes = []
      @parent = parent
      @output_continue = false
      @types = Hash.new nil
      parent.set_child self if !parent.nil?
    end

    # Add a new use statement
    #
    # @param [Symbol] sym of the module to be used
    # @param str containing the fortran use statement if the default "use module" is not desired
    def use sym, str=nil
      @use_statements[sym] = str || "use #{sym.to_s}"
    end

    def interface name, line
      raise "You can only add interfaces to module scopes" unless @kind == :module
      @interfaces[name] <<= line
    end

    def interfaces indent
      result = ""
      @interfaces.each do |name, lines|
        result += "\n#{indent}  interface #{name}\n"
        result += "#{indent}    " + lines.join("\n#{indent}    ") + "\n"
        result += "#{indent}  end interface #{name}\n"
      end
      result
    end

    # Add a variable declaration directly into the symbol table
    #
    # @param [Hash] var contains a symbol => declaration statement hash
    # @param [Symbol] type argument, useful to create RHS definitions
    def raw_var var, type=nil
      @types[var.keys[0]] = type if type
      @variables.merge! var
    end

    # Add a variable declaration to the intermediate mangle table.
    # This should be used together with {#mangle} to generate the final
    # variable names.
    #
    # @param [Symbol] name of the variable
    # @param [String] typedecl_str to be used in the generated Fortran code at
    #                 the left hand side of the '::'
    # @param [String] init_str to be used in the generated fortran as an
    #                 initializer, such as "=> NULL()"
    def var name, typedecl_str, init_str = nil
      @unmangled[name] = [typedecl_str, init_str]
    end

    # Mangle the variable names in an expanded macro and add the final variable
    # names to the scope's symbol table.
    #
    # @param [String] body of an expanded macro
    def mangle body
      if conf.predictable_mangle_prefix.nil? or
        !conf.predictable_mangle_prefix
        prefix = "gen"+rand(26**6).to_s(26)
      else
        prefix = "mangled"
      end

      @unmangled.each_pair do |k,v|
        unmangled = k.to_s
        mangled = "#{prefix}_#{k.to_s}"
        pattern = "(?<![a-zA-Z_0-9])" + unmangled +  "(?![a-zA-Z_0-9])"
        body.gsub! /#{pattern}/, mangled
        raw_var mangled.to_sym => "#{v[0]} :: #{mangled} #{v[1]}".rstrip()
      end
      @unmangled.clear
      body
    end

    # Add to the current scope a continue statement for clean routine exit
    def insert_exit_point
      @output_continue = true
    end

    # Add to the current scope an include statement
    #
    # @param inc the include statement
    def include inc
      @includes << inc
    end

    # Add a global argument. This will use the global module in the current
    # scope and add a ppm_ctrl argument (generating the code for reading the
    # argument from an ctrl file and the command line).
    # See {CG::GlobalModule#arg}
    def arg h
      use GlobalModule.name
      GlobalModule.instance.arg h
    end

    # Add a module to be used by the global module
    #
    # @param [Symbol] mod name
    def global_use mod
      use GlobalModule.name
      GlobalModule.instance.use mod
    end
  
    # Setup an right hand side call. This will use the RHS module in the current
    # scope and request a right hand side function to be generated that presents
    # fields and discretizations as given here.
    #
    # @param [String] name generic name of the right hand side
    # @param [Array] field_discs passed this right hand side
    # @param [Array] change_discs passed this right hand side
    def rhs_call name, field_discs, change_discs
      use RHSModule.name
      RHSModule.instance.call_to(name, field_discs, change_discs)
    end

    # Return the type of a given symbol if set previously by {#var}
    #
    # @param [Symbol] sym to be resolved
    # @return [Symbol] the type
    def type_of sym
      @types[sym]
    end

    protected
    def set_child child
      @child = child
    end
  end
end
