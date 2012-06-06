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
      use :ppm_module_ctrl
      use :ppm_module_data
    end

    def arg o
      var o[:name], "#{o[:type]} :: #{o[:name]}"
      txt = "call arg(#{o[:name]}, '#{o[:name]}'"
      o.delete :name
      o.delete :type
      o.each do |k,v|
        txt += ", &\n         #{k} = #{v}"
      end
      txt += ")"
      @da.add txt
    end
  end
end
