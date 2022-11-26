## Lexical Structure

There are several categories of tokens:

* line comments
* literate comments
* inline strings
* multiline strings
* mode delimiters
* literal symbols
* count arguments
* numbers
* stack rearrangements
* symbols

Tokens are separated by one or more space or newline characters.
Comment, string, and multiline string tokens may have embedded spaces
or newlines.

### Comments

A comment may take one of two forms.  A single backslash followed by a
space begins a comment that extends until the end of the line.
Alternatively, any line with a non-separator character in the first
four columns is considered a comment, allowing the use of Markdown as
rich comment syntax.

### Inline Strings

Inline strings follow the C convention.  That is, they are delimited by
double quotes, and backslash is used as to escape double quote or
backslash, and to embed control characters or unicode literals.

### Multiline Strings

Multiline strings are inspired by one type of YAML multiline strings.
A multiline string begins with "|+".  While the "|+" must be
immediately followed by a space or newline, if it is followed by a
space, then additional non-space characters may appear.  They will not
be included in the string, and should be ignored or result in a
warning.  The body of the string appears in subsequent lines, and only
includes the columns _after_ the column containing the "|" of the
opening delimiter.  Note that there may be comment lines within the
string, and that non-space characters between column 4 and the first
included column should be ignored or result in a warning. The string
ends with a "|" in the same column as the one in the opening
delimiter. All between the closing delimiter and the end of the line
is ignored or may result in a warning.

### Mode Delimiters

Mode delimiters are single character tokens, each delimited by a space
or newline on each side, one of open/close brace, bracket, or
parenthesis.

### Literal Symbols

Literal symbols are delimited by a space or newline on each side. They
consist of an initial pound/hash character and one or more non-space,
non-newline characters.

### Count Arguments

Count arguments are delimited by a space or newline on each side.
They consist of an initial open parenthesis, one or more decimal
digits, and end with a close parenthesis.

### Numbers

Numbers are delimited by a space or newline on each side. They consist
of optionally a minus followed by one or more decimal digits.

### Stack Rearrangements

Stack rearrangements are delimited by a space or newline on each side.
They consist of an initial vertical bar, one or more lowercase letters
or periods, and end with a vertical bar.

### Symbols

Symbols are delimited by a space or newline on each side. They consist
of sequence of non-space and non-newline characters which match none
of the categories above.

## Syntax

## Semantics
