grammar CG ;

options { language = Ruby; }

@header{
# require 'something'
}

@init{
# goes into initialize
}

prog : .* ;
