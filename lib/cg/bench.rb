module CG

  require "csv"

  class Measurement
    attr_accessor :min, :max, :avg

    def initialize min=0.0,max=0.0,avg=0.0
      @min = min
      @max = max
      @avg = avg
    end

    def ==(other)
      (self.min == other.min) and (self.max == other.max) and (self.avg == other.avg)
    end

    def +(other)
      Measurement.new self.min+other.min,self.max+other.max,self.avg+other.avg
    end

    def -(other)
      Measurement.new self.min-other.min,self.max-other.max,self.avg-other.avg
    end

    def /(frac)
      Measurement.new self.min/frac,self.max/frac,self.avg/frac
    end
  end

  class DataSet
    attr_reader :header, :timings

    def initialize header
      @timings = {}
      @header = header
    end

    def add_time_series nproc, times
      if@timings.has_key?(nproc) then
        @timings[nproc] << times
      else
        @timings[nproc] = [times]
      end
    end

    def time_series nproc
      @timings[nproc]
    end

    def avg_time_series nproc
      avg = Array.new(@timings[nproc][0].size) { Measurement.new }
      @timings[nproc].each do |ts|
        (0...ts.size).each do |i|
          avg[i] = avg[i] + ts[i]
        end
      end
      (0...avg.size).each {|i| avg[i] = avg[i] / @timings[nproc].size}
      avg
    end

    def avg_time_per_step nproc
      ts = avg_time_series nproc
      mean = ts.reduce(Measurement.new) {|sum,el| sum=sum+el}/ts.size
    end

    def efficiency
      minproct = avg_time_per_step @timings.keys.min
      eff = []
      nprocs = []
      @timings.keys.sort.each do |key|
        nprocs << key
        t = avg_time_per_step key
        eff << minproct.avg / t.avg
      end
      [nprocs,eff]
    end

  end

  class Benchmark

    attr_reader :data_sets

    def initialize
      @data_sets = {}
    end

    def append csv_string, nproc
      csv_data = CSV.parse csv_string, {:col_sep => "\t", :headers => true}
      headers = csv_data.headers.find_all {|h| !h.nil?}
      headers.each {|h| @data_sets[h] = DataSet.new h if !@data_sets.has_key? h}
      timing = {}
      csv_data.each do |row|
        grouped = row.each_slice(3).to_a
        grouped.each do |el|
          header = el[0][0]
          data = Measurement.new el[0][1].to_f,el[1][1].to_f,el[2][1].to_f
          timing.has_key?(header) ? timing[header] << data : timing[header] = [data]
        end
      end
      timing.each_pair do |key,val|
        @data_sets[key].add_time_series nproc,val
      end
    end


  end

end