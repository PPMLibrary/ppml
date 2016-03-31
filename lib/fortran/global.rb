module CG
  class GlobalModule < FortranModule
    include Singleton

    @@module_name = "global"

    def self.name
      "#{conf.name_prefix}#{@@module_name}"
    end

    def initialize
      super "#{conf.name_prefix}#{@@module_name}"
      @da = FortranSubroutine.new "define_args"
      subroutine @da
      use :ppm_module_core
      use :ppm_module_user_numerics
      if (!conf.ppm.prec.nil?  )
        var :mk, "INTEGER, PARAMETER :: MK = #{conf.ppm.prec}"
      else
        var :mk, "INTEGER, PARAMETER :: MK = ppm_kind_single"
      end
    end

    def arg o
      var o[:name], "#{o[:type]} :: #{o[:name]}"
      @da.add o[:init] unless o[:init].nil?
      txt = "CALL arg(#{o[:name]}, '#{o[:name]}'"
      o.delete :name
      o.delete :type
      o.delete :init
      o.each do |k,v|
        txt += ", &\n         #{k} = #{v}"
      end
      txt += ")"
      @da.add txt
    end

    def arg_group g
      @da.add "CALL arg_group('#{g}')"
    end
  end
end
