
require_relative "../fortran.rb"

module CG
  class Rhs
    attr_accessor :name
    attr_reader :args, :body, :rhs_func

    # this is dictated by the ppm ODE step routine
    TTOPO = "integer, intent(in) :: "
    TDISCR = "real(mk), dimension(:,:), pointer :: "
    TRHS = "real(mk), dimension(:,:), pointer :: "
    TDRHS = "real(mk), dimension(:,:), pointer :: " 
    TIDATA = "integer, dimension(:,:), intent(in), optional :: " 
    TLDATA = "logical, dimension(:,:), intent(in), optional :: " 
    TRDATA = "real(mk), dimension(:,:), intent(in), optional :: " 

    NAME = '(?:[a-z_][a-z_0-9]*)'
    #ARG = "#{NAME}(?: *(?<!%)= *#{NAME})?"
    #ARGS = "(?: *#{ARG} *(?:, *#{ARG} *){6})"
    TOPO_ARG = "(?:topology *(?:= *(?<topo>#{NAME}))?)" 
    DISCR_ARG = "(?:discretization *(?:= *(?<discr>#{NAME}))?)" 
    RHS_ARG = "(?:rhs *(?:= *(?<rhs>#{NAME}))?)" 
    DRHS_ARG = "(?:drhs *(?:= *(?<drhs>#{NAME}))?)" 
    IDATA_ARG = "(?:idata *(?:= *(?<idata>#{NAME}))?)" 
    LDATA_ARG = "(?:ldata *(?:= *(?<ldata>#{NAME}))?)" 
    RDATA_ARG = "(?:rdata *(?:= *(?<rdata>#{NAME}))?)" 
    ARGS = "(?: *#{TOPO_ARG} *, *#{DISCR_ARG} *, *#{RHS_ARG} *, *#{DRHS_ARG} *, *#{IDATA_ARG} *, *#{LDATA_ARG} *, *#{RDATA_ARG})" 
    RHS_START = /^ *rhs +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/i
    RHS_STOP = /^ *end +rhs *$/i
    
    def initialize name, args, body_or_file
      if body_or_file.is_a? StringIO or body_or_file.is_a? File
        body = read_body body_or_file
      else
        body = body_or_file
      end
      @name = name
      @rhs_func = FortranFunction.new name, "integer"
      @rhs_func.args args
      @rhs_func.add body
    end



    protected

    def read_body file
      body = ''
      while l=file.gets
        raise "Nested RHS definitions are not allowed" if l =~ self.class::RHS_START
        return body if l =~ self.class::RHS_STOP
        body << l
      end
      raise "EOF while reading body!"
    end

    class << self
      def load path
        load_file File.open(path)
      end

      def load_file file
        rhs = nil
        args = {}
        while l=file.gets
          if RHS_START =~ l
            name = $~[:name]
            unless $~[:topo].nil?
              args[$~[:topo].to_sym] = TTOPO + $~[:topo]
            else
              args[:topology] = TTOPO + "topology"
            end
            unless $~[:discr].nil?
              args[$~[:discr].to_sym] = TDISCR + $~[:discr]
            else
              args[:discretization] = TDISCR + "discretization"
            end
            unless $~[:rhs].nil?
              args[$~[:rhs].to_sym] = TRHS + $~[:rhs]
            else
              args[:rhs] = TRHS + "rhs"
            end
            unless $~[:drhs].nil?
              args[$~[:drhs].to_sym] = TDRHS + $~[:drhs]
            else
              args[:drhs] = TDRHS + "drhs"
            end
            unless $~[:idata].nil?
              args[$~[:idata].to_sym] = TIDATA + $~[:idata]
            else
              args[:idata] = TIDATA + "idata"
            end
            unless $~[:ldata].nil?
              args[$~[:ldata].to_sym] = TLDATA + $~[:ldata]
            else
              args[:ldata] = TLDATA + "ldata"
            end
            unless $~[:rdata].nil?
              args[$~[:rdata].to_sym] = TRDATA + $~[:rdata]
            else
              args[:rdata] = TRDATA + "rdata"
            end
            
            rhs = Rhs.new name, args, file
          end
        end
        rhs
      end

    end # class << self

  end # Rhs

end
