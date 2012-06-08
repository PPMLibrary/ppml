require 'set'

module CG
  class RHSModule < FortranModule
    include Singleton

    @@module_name = "rhs"

    def self.name
      return "#{conf.name_prefix}#{@@module_name}"
    end

    def initialize
      super "#{conf.name_prefix}#{@@module_name}"
      use GlobalModule.name.to_sym
      @defs = {}
      @calls = Hash.new(Set.new)
    end

    # Add a RHS definition
    #
    # @param [String] name of the RHS
    # @param [Array] args list of the RHS (=rhs_fields)
    # @param [Array] result list of changes
    # @param [String] body of the RHS definition
    def definition name, args, result, pre, post
      @defs[name] = [Hash[args.map(&:name).zip(args.map(&:disc))], Hash[result.map(&:name).zip(result.map(&:disc))], pre, post]
    end

    # Add a call to the RHS. This method must be called whenever a RHS is called. 
    # This allows the rhs module to generate the correct code for accessing the
    # fields and discretizations.
    #
    # @param [String] name of the rhs to which this call should be added
    # @param [Array] arg_types the discretization types of the input arguments 
    # of this RHS. Some elements may be nil
    # @param [Array] resul_types the types of the output arguments of this RHS
    # (the changes)
    def call_to name, arg_types, result_types
      @calls[name].add [arg_types, result_types]
    end

    # Generate the module Fortran code.
    def to_s
      @defs.each do
        |name, rhs|
        @calls[name].each do
          |call_type|
          function RHS.new(name, call_type, rhs)
        end
      end
      super
    end

  end

  class RHS < FortranFunction

    # Create an instance of the RHS definition with a given definition and call
    #
    # @param [String] name of the RHS
    # @param [Array] call_types argument types (Array) and result types (Array)
    # @param [Array] definition arguments and results stored in two Hashes
    def initialize name, call_types, definition
      super name, "integer"
      prec = !conf.ppm.prec.nil? ? conf.ppm.prec : "ppm_kind_double"
      args fields_discr: "class(ppm_v_field_discr_pair), pointer :: fields_discr",
        time: "real(#{prec}) :: time",
        changes: "class(ppm_v_field), pointer :: changes"
      use :ppm_module_interfaces
      var :fd_pair, "class(ppm_t_field_discr_pair), pointer :: fd_pair => null()"
      var :di, "class(ppm_t_discr_info_), pointer :: di => null()"

      add definition[2].map(&:to_s).map(&:strip)

      add "#{name} = 0"
      add_args call_types[0], definition[0]
      add_results call_types[1], definition[1]

      add definition[3].map(&:to_s).map(&:strip)
    end

    # setup all arguments of right hand side. This retrieves the arguments from
    # the fields vector and creates variables with appropriate names and types.
    #
    # @param [Array] call_types argument types (from where the RHS will be used)
    # @param [Hash] defn of arguments (from the RHS declaration)
    def add_args call_types, defn
      # check whether argument lists have same length
      raise <<-ERRMSG if call_types.length != defn.length
Called RHS #{@name} with #{call_types.length} arguments instead of #{defn.length}
  Call arguments : #{call_types.join ', '}
  Definition     : #{defn.keys.join ', '}
ERRMSG
      # create items  of [[def_field, def_disc], call_arg_type]
      defn.zip(call_types).each_with_index do
        |dc, i|
        field_disc, type = dc
        var field_disc[0].to_sym, "class(ppm_t_field_), pointer :: #{field_disc[0]}"
        add "fd_pair => fields_discr%at(#{i+1})"
        add "#{field_disc[0]} => fd_pair%field"
        unless field_disc[1].nil?
          if type.nil?
            raise "RHS #{@name} requires for argument #{field_disc[0]} "+
              "a discretization to be passed when calling"
          end
          var field_disc[1].to_sym, "class(#{type}), pointer :: #{field_disc[1]}"
          add "select type(fd_pair%discretization)"
          add "class is (#{type})"
          add "  #{field_disc[1]} => fd_pair%discretization"
          add "end select"
        end
      end
    end

    # setup all output arguments of right hand side. This retrieves the arguments from
    # the changes vector and creates variables with appropriate names and types.
    #
    # @param [Array] result_types argument types (from where the RHS will be used)
    # @param [Hash] defn of arguments (from the RHS declaration)
    def add_results call, defn
      raise <<-ERRMSG if call.length != defn.length
Called RHS #{@name} with #{call.length} results instead of #{defn.length}
  Call results : #{call.join ', '}
  Definition   : #{defn.keys.join ', '}
ERRMSG
      # create items  of [[def_change, def_disc], result_type]
      defn.zip(call).each_with_index do |dc, i|
        res_disc, type = dc
        var res_disc[0].to_sym, "class(ppm_t_field_), pointer :: #{res_disc[0]}"
        add "#{res_disc[0]} => changes%at(#{i+1})"
        #var names[1].to_sym, "class(#{type}), pointer :: #{names[1]}" unless names[1].nil?
        #add "#{names[1]} => discretizations%at(#{i+1})" unless names[1].nil?
        unless res_disc[1].nil?
          if type.nil?
            raise "RHS #{@name} requires for output argument #{res_disc[0]} "+
              "a discretization to be passed when calling"
          end
          var res_disc[1].to_sym, "class(#{type}), pointer :: #{res_disc[1]}"
          add "di => #{res_disc[0]}%discr_info%begin()"
          add "select type(disc => di%discr_ptr)"
          add "class is (#{type})"
          add "  #{res_disc[1]} => disc"
          add "end select"

        end
      end
    end
  end

end
