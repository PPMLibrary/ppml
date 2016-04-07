module CG
  class Transform
    def initialize pattern, replacement
      @replacement = find_args          replacement
      @pattern     = pattern_to_regexp  pattern
    end

    # Apply do_transform to any number of strings.
    #
    # @param *args [String] any number of Strings
    def transform *args
      res = args.map do
        |arg|
        do_transform arg if arg.is_a? String
      end
      if res.length == 1
        res[0]
      else
        res
      end
    end

    # Replace all occurences of pattern in target with replacement.
    #
    # @param target [String] target string
    def do_transform target
      target.gsub /#{@pattern}/ do |match|
        if @splat or @max > 0
          args = Transform.arg_split $~[:args]
        end
        injects = Transform.injects_split $~[:injects].to_s
        # create replacement string with injects inserted
        repl = @replacement.map do |r|
          if r.is_a? String
            r.gsub /(#\d+)/ do |m|
              idx = m[1..-1].to_i
              if idx <= injects.length
                injects[idx-1]
              else
                ""
              end
            end
          else
            r
          end
        end
        sub = ""
        if @splat or @max > 0
          repl.each do
            |piece|
            if piece.is_a? Integer
              sub = sub + args[piece-1]
            elsif piece == :splat
              sub = sub + args[@max..args.length].join(',') unless @max > args.length
            else
              sub = sub + piece
            end
          end
        else
          sub = repl.join ''
        end
        sub
      end
    end

    # Destructive version of transform.
    #
    # @param *args [String] any number of Strings
    def transform! *args
      res = args.map do
        |arg|
        do_transform! arg if arg.is_a? String
      end
      if res.length == 1
        res[0]
      else
        res
      end
    end

    # Destructive version of do_transform
    #
    # @param target [String] target string
    def do_transform! target
      target.replace(do_transform target)
    end

    # Transforms an input string into an array of pieces
    # where all arg references are replaced by either
    # the integer value of the reference or :splat.
    #
    # @param replacement [String] string with optional arg references
    def find_args replacement
      @max = 0
      @splat = false
      replacement.split(/(\$(?:\d+|\*))/).map do
        |piece|
        if piece == "$*"
          @splat = true
          :splat
        elsif piece =~ /^\$(\d+)$/
          @max = $~[1].to_i if $~[1].to_i > @max
          $~[1].to_i
        else
          piece
        end
      end
    end

    # Transforms a comma separated arg list into an array
    # (like String.split ',') but ignores quoted commas.
    #
    # @param str [String] an argument list
    def self.arg_split str
      args = []
      str.scan /".*?(?<!\\)"|[^,]+/ do
        |arg|
        args << arg
      end
      args
    end

    # similar to the above but also allows empty elements in comma separated
    # list
    def self.injects_split str
      injs = []
      str.scan /".*?(?<!\\)"|[^,]*/ do |inj|
        injs << inj
      end
      injs
    end

    # Creates a propper regexp from pattern by making sure to
    # avoid substrings and optinally to capture arg lists.
    #
    # @param pattern [String] the search pattern
    def pattern_to_regexp pattern
      regexp = "(?<![a-zA-Z_0-9])" + pattern + "(?![a-zA-Z_0-9])"
      regexp << "(\\[(?<injects>([-+]?\\d+)?(,([-+]?\\d+)?)*)\\])?"
      if @splat or @max > 0
        regexp << "\\((?<args>.*?)\\)"
      end
      regexp
    end

  end

  class MultiTransform
    def initialize patterns
      @patterns = find_args          patterns
      @regexp   = patterns_to_regexp patterns
    end

    # Apply do_transform to any number of strings.
    #
    # @param *args [String] any number of Strings
    def transform *args
      res = args.map do
        |arg|
        do_transform arg if arg.is_a? String
      end
      if res.length == 1
        res[0]
      else
        res
      end
    end

    # Replace all occurences of pattern in target with replacement.
    #
    # @param target [String] target string
    def do_transform target
      target.gsub /#{@regexp}/ do
        |match|
        sub = ""
        key = $~[:key]
        if @splat or @nmax > 0
          args = Transform.arg_split $~[:args]
          @patterns[key].each do
            |piece|
            if piece.is_a? Integer
              sub = sub + args[piece-1]
            elsif piece == :splat
              sub = sub + args[@max[key]..args.length].join(',') unless @max[key] > args.length
            else
              sub = sub + piece
            end
          end
        else
          sub = @patterns[key].join ''
        end
        sub
      end
    end

    # Destructive version of transform.
    #
    # @param *args [String] any number of Strings
    def transform! *args
      res = args.map do
        |arg|
        do_transform! arg if arg.is_a? String
      end
      if res.length == 1
        res[0]
      else
        res
      end
    end

    # Destructive version of do_transform
    #
    # @param target [String] target string
    def do_transform! target
      target.replace(do_transform target)
    end


    # Transforms all value strings in input into arrays of pieces
    # where all arg references are replaced by either
    # the integer value of the reference or :splat.
    #
    # @param replacement [Hash] pattern to replacement map
    def find_args patterns
      @max = Hash.new 0
      @splat = false
      patterns.each do |pattern, replacement|
        patterns[pattern] = replacement.split(/(\$(?:\d+|\*))/).map do |piece|
          if piece == "$*"
            @splat = true
            :splat
          elsif piece =~ /^\$(\d+)$/
            @max[pattern] = $~[1].to_i if $~[1].to_i > @max[pattern]
            $~[1].to_i
          else
            piece
          end
        end
      end
      @nmax = 0
      @max.values.each { |v| @nmax = v if v > @nmax }
      patterns
    end

    # Creates a propper regexp that captures any key from input hash.
    #
    # @param pattern [Hash] pattern to replacement map
    def patterns_to_regexp patterns
      regexp = "(?<![a-zA-Z_0-9])(?<key>" + patterns.keys.join("|") + ")(?![a-zA-Z_0-9])"
      if @splat or @nmax > 0
        regexp << "\\((?<args>.*?)\\)"
      end
      regexp
    end
  end
end
