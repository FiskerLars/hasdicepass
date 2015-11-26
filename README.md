# hasdicepass

[Diceware passphrases](http://world.std.com/~reinhold/diceware.html) are
a approach to create a complex password that is hard to guess but easy
to remember. It is actually not recommended to use a computer to
generate those passphrases, but “what I cannot create I cannot
understand” (Feynman). If you actually want to use a computer for random
selection of words from a list, there are much simpler algorithms for
much more generic list than diceword lists.

Diceword lists, for example, are size restricted. The number of words
must match a power of six.


## Usage

You'll have to download a diceword list from the above webpage or create
your own. And then provide the name and a number after the -n parameter
to hasdicepass.

A diceword list is a line-oriented file. Each line starts with a
sequence of numbers in the range [1,6] and a word (letters and numbers
without spaces). For this current version you'll have to ensure that all
lines consist the same number of digits and that every combination of
digits is included. Strange errors might occur otherwise.

## Implementation

There are three parts that could be used for computer programming
lessons. I invite you to get your own version build. I would be
interested in improvements or different solutions.

* Parsing of diceword lists 
* Generating a diceword tree
* Locating a word given a sequence of random numbers (of the correct
  length)

After parsing the diceword list the program compiles the list into a
tree, ordered by numbers. This is, for the practical problem at hand,
definitely overengineered and probably increases the runtime for most
practical number of output words. But if you happen to require a million
words, it probably runs a little faster. More important, it is a more
beautiful way to store the decision tree that essentially is the thing a
diceword list resembles.

I chose an implementation that is at all points oblivious to the number
of dices thrown (the length of the diceword list, i.e. the number of
digits in each line in the diceword file). Thus, in theory you could use
it with unbalanced diceword trees, but this would obviously reduce the
randomness of results. 

Word location (implemented in the diceword function) turned out to be
rather complex due to error handling. If a word is not found the error
will provide you with the exact location where your list is lacking a
word.

### Wordlist Parsing

If you take a closer look at the Wordlist parser, you'll find that there
are two versions. After fiddling with generating generic parser DSL and
some (yet unknown) problem with the parsing of sequences, I went for a
very simplistic parser. 
