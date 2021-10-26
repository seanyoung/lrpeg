
# Left Recursive Parsing Expression Grammar (PEG)

lrpeg has the following advantages over existing rust parser generators

- Uses intuitive PEG syntax
- Uses efficient ratpack parsing
- Allows left recursion in rules
- No precedence climber required
- Parse tree only contains nodes you require

Here is a simple calculator example with the correct precedence and
association rules for addition, multiply, and power.

```
expr <-
    add:/ <left:expr> "+" <right:term>
    sub:/ <left:expr> "-" <right:term>
    / term

term <-
    mul:/ <left:term> "*" <right:power>
    div:/ <left:term> "/" <right:power>
    / "(" expr ")"
    / power

power <-
    pow:/ <left:num> "^" <right:power>
    num:/ <num>

num <- re#[0-9]+#;
```

