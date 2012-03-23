require_relative "../spec_helper"

module CG
  describe Measurement do
    context "Operators" do
      describe "#+" do
        it "Adds two measurement triplets and returns a new instance" do
          m1 = Measurement.new 1.0,2.0,3.0
          m2 = Measurement.new 4.0,5.0,6.0
          m3 = m1 + m2
          m3.min.should == 5.0
          m3.max.should == 7.0
          m3.avg.should == 9.0
        end
      end
      describe "#-" do
        it "Adds two measurement triplets and returns a new instance" do
          m1 = Measurement.new 5.0,7.0,9.0
          m2 = Measurement.new 4.0,5.0,6.0
          m3 = m1 - m2
          m3.min.should == 1.0
          m3.max.should == 2.0
          m3.avg.should == 3.0
        end
      end
      describe "#/" do
        it "Adds two measurement triplets and returns a new instance" do
          m1 = Measurement.new 4.0,8.0,6.0
          m2 = m1 / 2.0
          m2.min.should == 2.0
          m2.max.should == 4.0
          m2.avg.should == 3.0
        end
      end
    end
  end

  describe DataSet do

    before :each do
      @header = "Timing series 1"
      @dataset = DataSet.new @header
      @m1 = Measurement.new 1.0, 3.0, 2.0
      @m2 = Measurement.new 2.0, 4.0, 3.0
      @timings1 = [@m1,@m2]
      @m3 = Measurement.new 10.0, 12.0, 11.0
      @m4 = Measurement.new 12.0, 14.0, 13.0
      @timings2 = [@m3,@m4]
    end

    describe "#add_time_series" do
      it "adds a new time series to the dataset given a number of procs key" do
        @dataset.add_time_series 1, @timings1
        @dataset.timings[1][0].should == @timings1
      end
    end
    describe "#time_series" do
      it "returns all time series for a given nproc" do
        @dataset.add_time_series 1,@timings1
        @dataset.add_time_series 1,@timings2
        @dataset.time_series(1).should == [@timings1,@timings2]
      end
    end

    describe "# " do
      it "returns the average time series over all runs" do
        @dataset.add_time_series 1,@timings1
        @dataset.add_time_series 1,@timings2
        avg = @dataset.avg_time_series 1
        m1e = (@m1 + @m3)/2.0
        m2e = (@m2 + @m4)/2.0
        avg[0].should == m1e
        avg[1].should == m2e
      end
    end

    describe "#avg_time_per_step" do
      it "returns the average over the averaged samples" do
        @dataset.add_time_series 1,@timings1
        @dataset.add_time_series 1,@timings2
        mean = @dataset.avg_time_per_step 1
        m1e = (@m1 + @m3)/2.0
        m2e = (@m2 + @m4)/2.0
        expect = (m1e + m2e)/2.0
        mean.should == expect
      end
    end

    describe "#efficiency" do
      it "returns two (packed) arrays with number of procs and efficiencies" do
        @dataset.add_time_series 1,@timings1
        @dataset.add_time_series 8,@timings2
        eff = @dataset.efficiency
        eff[0].should == [1,8]
        eff[1].should == [1.0,2.5/12]
      end
    end
  end

  describe Benchmark do
    before :all do
      @bench = Benchmark.new
    end
    describe "#append reading in data" do
      it "reads in a timing data file" do
        csv_data = <<-eos
"test1"			"test2"
0.144958E-03	  0.144958E-03	  0.144958E-03	  0.144958E-03	  0.144958E-03	  0.144958E-03
0.124958E-03	  0.144938E-03	  0.144948E-03	  0.143958E-03	  0.142958E-03	  0.164958E-03
        eos
        @bench.append csv_data, 1
        @bench.data_sets["test1"].timings[1][0][1].min.should == "0.124958E-03".to_f
      end

      it "appends more data to the same data set" do
        csv_data = <<-eos
"test1"			"test2"
0.244958E-03	  0.244958E-03	  0.244958E-03	  0.244958E-03	  0.244958E-03	  0.244958E-03
0.224958E-03	  0.244938E-03	  0.244948E-03	  0.243958E-03	  0.242958E-03	  0.264958E-03
        eos
        @bench.append csv_data, 1
        @bench.data_sets["test1"].timings[1].size.should == 2
      end
    end
  end
end