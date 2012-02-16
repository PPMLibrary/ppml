module CG
  class Macro
    attr_accessor :name, :body

    def initialize(name, body, args=nil)
      @name, @body, @args = name, body, args
    end

    def expand(result=nil, args=nil)
      result = @body
      @args.zip(args).each do |a,r|
        result = result.sub("$#{a}",r.to_s)
      end if args and @args
      result
    end
  end
end
