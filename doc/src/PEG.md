# How to write a parser definition

Traditional LR(1) and GLR grammers are divided into a *lexer* and a *parser*.
PEG parsers do not have a seperate lexer. So, a single definition is enough
to construct your parser.

## Rules

Rules start with an identifier and are followed by `<-`, and then one or
more expressions and finally an optional `;`.

An item can be a terminal like `"foo"` or another rule like `bar`. There are
many more possibilities which will discus later. The parser always starts at
the very first rule, and at the first expression, and follows from there.

A very simple PEG could be:

```
foo <- bar;
bar <- "bar";
```

This would construct a parser that would accept `bar` as input. To be more
exact, this parser would accept any input that starts with `bar`, including
`barfoo`.

If a rule has multiple expressions, then all of them must in-order match for the
entire rule to match.

```
foo <- bar WHITESPACE baz;
bar <- "bar";
baz <- "baz";
```

This parser will match `bar` followed by any amount of whitespace, followed
by `baz`. Again there can be text trailing after `baz`.

In order ensure we parse the entire input text, you need to use special keyword
`EOI` (end of input):

```
foo <- "bar" EOI;
```

This parser will only match `bar` exactly, and will return an error if
given any other input including text after `bar`.

## Builtin expressions and regex

lrpeg does not have a separate lexer like traditional parser generators,
so your PEG definition will have to deal with whitespace, end of input,
etc. There are some builtin rules which for that.

`WHITESPACE`
This matches anything that unicode considers whitespace, including newlines.

`XID_IDENTIFIER`
This matches anything that
[unicode considers an identifier](http://www.unicode.org/reports/tr31/).

`DOT`
This match any single character.

`EOI`
End of input, matches if the next character is the last character. This
could also be written as `!DOT` but this implementation is more efficient.

regex (e.g. `re#\d+#`)
Any regular expression that [regex](https://crates.io/crates/regex) can
handle.

## Prioritized Choice

A central concept in PEG parser is choices. This syntax says first try one
expression before another expression, seperated by `/`. This parser will parse
the input `bar` and `baz`.

```
foo <- bar EOI;
bar <- "bar" / "baz";
```

There can be two or more choices, which are tried in-order. As soon as
a choice matches, none of the others will be tried. For example:

```
foo <- "bar"
    / "baz"
    / "foo"
    / "foof";
```

This parser will match `bar`, `baz`, or `foo`. When presented with the input
`foof`, the third choice `foo` will match and the remaining `f` will
not be consumed by this rule. In fact, the last choice `foof` is
unreachable because of this. lrpeg does not detect unreachable choices yet.

## Matching multiple occurrences

It is possible to match the same expression multiple times using the `+`
syntax. This can parse a field list where there are one or more fields.

```
field_list <- "{" field+ "}" ";";
field <- XID_IDENTIFIER ";"?;
```

The final `;` is optional because it is followed by `?`. This means there can
be 0 or 1 occurences of an expression. By changing the `+` on the first line
to `*`, we can match 0 or more fields.

We can use these rules to create a parser for comma separated lists.

```
fields <- field ("," field)*
field <- XID_IDENTIFIER ":" re#\d+#;
```

This parses the input: `foo:1,bar:102,x:500`

## Must match and must not match

The expressions we've seen so far consume input text if they match; this is
not true for these expressions. They simply look for presence or absence of
input.

```
file <- (foo / newline)*;
foo <- "foo" !"\n";
newline <- "\n";
```

This rule match `foo`, but not if it is end and the end of a line. The end
of line is not consumed; this means that the newline rule is still matched.

If we prefix an expression with `&` then it only match if the expression
does match, but without consuming it.

```
foo <- "foo" &"\n";
```

Say you want to create a parser for a mini language which consistents of
lines like so:

```
fruit=apple peer mango
veg=carrot potato
```

You might start with peg which looks like:

```
input <- group* EOI;
group <- XID_IDENTIFIER WHITESPACE "=" WHITESPACE list;
list <- item*
item <- XID_IDENTIFIER WHITESPACE;
```

However, this does not work. The first time rule list is executed, it will
match `apple peer mango veg`, so it consumes the `veg` from the following
group. When it starts parsing the second group, the next input character is
`=` which does not match.

We may try to fix this by adding `!"="` to the list rule.

```
input <- group* EOI;
group <- XID_IDENTIFIER WHITESPACE "=" WHITESPACE list;
list <- item* &"="
item <- XID_IDENTIFIER WHITESPACE;
```
This does not work since PEG parsers do not backtrack (except for choices,
that is, which we are not using here) so _item\*_ will still match `veg`,
but then fail because it is not followed by `=`.

We can resolve this issue by making sure the rule _item_ is not followed by
`=`:

```
input <- group* EOI;
group <- XID_IDENTIFIER WHITESPACE "=" WHITESPACE list;
list <- item*
item <- XID_IDENTIFIER WHITESPACE !"=";
```

## Left associate and right associate

## Precedence rules

## Create nodes in parse tree