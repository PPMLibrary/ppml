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

    def definition name, args, result, body
      @defs[name] = [Hash[args.map(&:name).zip(args.map(&:disc))], Hash[result.map(&:name).zip(result.map(&:disc))], body]
    end

    def call_to name, args, result
      @calls[name].add [args, result]
    end

    def to_s
      @defs.each do
        |name, rhs|
        @calls[name].each do
          |call|
          function RHS.new(name, call, rhs)
        end
      end
      super
    end
  end

  class RHS < FortranFunction
    def initialize name, call, definition
      super name, "integer"
      args fields_discr: "class(ppm_v_field_discr_pair), pointer :: fields_discr", 
        changes: "class(ppm_v_field), pointer :: changes"
      use :ppm_module_interfaces
      var :fd_pair, "class(ppm_t_field_discr_pair), pointer :: fd_pair"
      add_args call[0], definition[0]
      add_results call[1], definition[1]
      add definition[2].map(&:to_s).map(&:strip)
    end

    def add_args call, defn
      raise <<-ERRMSG if call.length != defn.length
Called RHS #{@name} with #{call.length} arguments instead of #{defn.length}
  Call arguments : #{call.join ', '}
  Definition     : #{defn.join ', '}
ERRMSG
      defn.zip(call).each_with_index do
        |dc, i|
        names, type = dc
        var names[0].to_sym, "class(ppm_t_field), pointer :: #{names[0]}"
        add "fd_pair => fields_discr%vec(#{i+1})"
        add "#{names[0]} => fd_pair%field"
        unless names[1].nil?
          var names[1].to_sym, "class(#{type}), pointer :: #{names[1]}"
          add "select type(fd_pair%discretization)"
          add "class is (#{type})"
          add "  #{names[1]} => fd_pair%discretization"
          add "end select"
        end
      end
    end

    def add_results call, defn
      raise <<-ERRMSG if call.length != defn.length
Called RHS #{@name} with #{call.length} results instead of #{defn.length}
  Call results : #{call.join ', '}
  Definition   : #{defn.join ', '}
ERRMSG
      defn.zip(call).each_with_index do
        |dc, i|
        names, type = dc
        var names[0].to_sym, "class(ppm_t_field), pointer :: #{names[0]}"
        add "#{names[0]} => changes%vec(#{i+1})"
        var names[1].to_sym, "class(#{type}), pointer :: #{names[1]}" unless names[1].nil?
        add "#{names[1]} => discretizations%vec(#{i+1})" unless names[1].nil?
      end
    end
  end

end
