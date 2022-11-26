#lang brag

flit-program: ( compiled-phrase | deferred-phrase | @executed-phrase )*

compiled-phrase: @atom*

deferred-phrase: /"{" ( @atom | @executed-phrase )* /"}"

/executed-phrase:
    simple-executed-phrase
    | executed-phrase-compiled-as-word
    | executed-phrase-compiled-as-double-word

simple-executed-phrase: /"[" @atom* /"]"

executed-phrase-compiled-as-word: /"(" @atom* /")"

executed-phrase-compiled-as-double-word: /"((" @atom* /"))"

/atom:
    character
    | number
    | string
    | symbol
    | name

character: CHARACTER
number: NUMBER
string: STRING
symbol: SYMBOL
name: NAME
