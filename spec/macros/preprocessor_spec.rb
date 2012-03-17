require_relative("../spec_helper")

module CG
  describe Preprocessor do
    before :each  do
      @p = Preprocessor.instance
      @m = mock('test_macro')
      @m.stub(:name => 'test_macro')
    end

    after :each do
      @p.macros.clear
    end

    context "adding macro definitions" do
      it "adds macro definitions with .macros <<" do
        @p.macros << @m
        @p.macros.should have_key('test_macro')
      end

      it "merges existing macro hashes with .macros +" do
        h = {'test_macro' => @m}
        @p.macros += h
        @p.macros.should have_key('test_macro')
      end
    end

    context "expanding macros" do
      it "expands existing macro definitions" do
        arglist = ['some', :args]
        @m.should_receive(:expand).with(*arglist).and_return('body')
        @p.macros << @m
        @p.expand('test_macro', *arglist).should == 'body'
      end

      it "returns nil if macro is not defined" do
        @p.expand('test_macro', nil).should be_nil
      end
    end
  end
end
