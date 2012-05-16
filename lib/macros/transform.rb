module CG
  class Transform
    def initialize pattern, replacement
      @replacement = find_args          replacement
      @pattern     = pattern_to_regexp  pattern
    end

    # Replace all occurences of pattern in target with replacement.
    #
    # @param target [String] target string
    def transform target
      target.gsub /#{@pattern}/ do
        |match|
        sub = ""
        if @splat or @max > 0
          args = Transform.arg_split $~[:args]
          @replacement.each do
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
          sub = @replacement.join ''
        end
        sub
      end
    end

    # Destructive version of transform
    #
    # @param target [String] target string
    def transform! target
      target.replace(transform target)
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

    # Creates a propper regexp from pattern by making sure to
    # avoid substrings and optinally to capture arg lists.
    #
    # @param pattern [String] the search pattern
    def pattern_to_regexp pattern
      pattern = "(?<![a-z_0-9])" + pattern + "(?![a-z_0-9])"
      if @splat or @max > 0
        pattern << "\\((?<args>.*?)\\)"
      end
      pattern
    end
  end
end
