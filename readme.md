# Fromage

Fromage is a new formatter for ocaml. It intends to be opiniated but open 
minded: the best formatting possible should be produced. I believe that 
formatting are not just a matter of taste, but that some objective advantages 
can be discovered and chosen. This means that I intend to document the 
formatting choices I make and the reasoning behind them, and I am open to 
discussing and improve them. 

I want to have as few options as possible. Line length is an option I want to 
support, and a few others but generally if there is a better formatting we 
should just adopt it instead of making an option.

For now there is only one version, but I also intend the formatting to be 
versioned so that we can improve it without forcing other devs to have large 
diffs in their codebase.

The implementation is heavily LLM-assisted. The goal is to ensure quality with 
extensive testing, including fuzzing thanks to 
[ocamlgrammarfuzzer](https://github.com/shogan-ai/ocamlgrammarfuzzer).

This readme is not LLM-generated, its intended as human-to-human communication,
to express the intent of the authors.

## Formatting principle

### Context giving symbols should be at the start of the line

The left of the line is the most visible spot. Things that gives context should
be here. For instance

```ocaml
match x with
| Lonnnng_pattern (a, b, c) when
  Some_cons (d, e, h) ->
    some_function j g k
```
is not great because to understand where the pattern, the when clause and the 
result start and end you have to look at the end of the lines. So there is a 
preference for the start of the line:

```ocaml
match x with
| Lonnnng_pattern (a, b, c) 
  when Some_cons (d, e, h) 
-> some_function j g k
```

This is just an example, and this preference is not a hard rule. For instance,
because its much more usual, if the pattern + when clause is single line we 
allow the `->` to end the line:

```ocaml
match x with
| Shorter_pattern (a, b, c) when Some_cons (d, e, h) -> 
    some_function j g k
```

This is both for the sake of tradition which has some value because a formatting 
that one is used to can be easier to read and because its easy to know where the
pattern end if its one line. Another way to put it is we subscribe to the 
classical view that the `->` ends the pattern instead of the defendable view 
that it start the result expression. 

In fact as explained, there is a stricter version of that rule for multiline 
blocks. 

### No diagonal separators

One the big principle is to avoid diagonal separators:

```ocaml
( f
    arg arg
    arg arg );
print x y z;
```

Here the `)` is at the end of the block, which is bad because you have to rely
only on indentation to grasp the shape of the code. This get especially bad when
you have multiple levels of nested `()`.

```ocaml
( f
    arg arg
    arg arg 
);
print x y z;
```

Is a lot more legible.

This style is quite common, but I actually should be applied in a lot more 
places, for instance pattern in match statements.

```ocaml
match x with
| looongg pattern
    pattern pattern ->
    expression expression expression
```
Is not legible because the `->` is in a diagonal position from the `|`. Its
very easy to confuse the pattern and the expression.

```ocaml
match x with
| looongg pattern
    pattern pattern 
  ->
    expression expression expression
```

Is lot more legible.

This principle should be applied everywhere, even when its surprising, for 
instance sequences:

```ocaml
print
  long arguments 
  veryyy loong
;
f ()
```

### Parent nodes should be less intented than their children

This one sounds obvious but is actually often not respected. Here is a glaring
example of bad formatting that could be produced: 

```ocaml
my_function ( g
    arg_of_g_1 arg_of_g_2)
```

Of course the better one is:

```ocaml
my_function 
  ( g
      arg_of_g_1 arg_of_g_2
  )
```

There are expections to this, notably with `begin`/`end` where `begin` followed
by another keyword should be treated as a single unit:
```ocaml
begin match x with
| C -> y
end
```

Here it could be argued that `|` is less indented than its parent node `match`,
but actually its `begin match` that is to be considered its parent.

As shown above the indentation can be the same, but that has to be considered 
carefully. `| ` is very hard to confuse for something else and is precisely two 
chars that color very few pixels, so its almost like 2 indent already, therefore
its fine. In most cases the indentation should increase by 2 between parent and
child.

Also something I am not sure about is this reasoning for anonymous functions:

```ocaml
List.map li ~f:(fun x ->
    f x
  )
```
is quite reasonable, and arguably better than 
```ocaml
List.map li 
  ~f:
    ( fun x ->
        f x
    )
```
But if everything gets longer, its a less familiar structure than `List.map` it
starts not begin a good formatting, even without reaching the margin:

```ocaml
Mymodule.my_function arg1 arg2 arg3 ~labelled_arg:(fun farg1 farg2 -> 
    long code farg1 code farg1
       long code farg2
       long code farg1
       long code farg2
  )
```
is a bit more difficult to read than 
```ocaml
Mymodule.my_function 
  arg1 arg2 arg3 
  ~labelled_arg:
    ( fun farg1 farg2 -> 
        long code
          long code
    )
```
because the eye have to reach much further to the right to understand the
context for the body of the anonymous function which is close to the left. For
now the more sparse formatting is used but its something I would like brainstorm
a bit more about.

Something interesting to notice with this rule and `()` is that its only 
possible to not have a line break after `(` because its only one char and 
therefore leaves room for a space and results in "punned" indentation of 2:
```ocaml
[ f
    arg1 arg2
]
```

has the same indentation as
```ocaml
[ 
  f
    arg1 arg2
]
```

Therefore its prefered, but for arrays this does not work:

```ocaml
[| f
    arg1 arg2
|]
```
Now the difference of indentation between `f` and its args is only one, 
which is wrong and should not happen. If `[|` was something even longer it could
be even worse.

### Infix operators

Infix operators are a bit difficult, because they share a syntax node even when
their intent is very different. For instance `+` is very symmetrical when `:=` 
or `|>` are not.

Because of this we classify operators into three categories: symmetrical, 
left-to-right and right-to-left. They have different indentation.

#### Symmetrical operators

With symmetrical operators, the operator is less indented than the operands:

```ocaml
  long computation
+
  other long computation
```

#### Directionnal operators

For directionnal operators, the operand that is moved is indented more the the 
one thats not:

```ocaml
myref
:=
  some value
```

```ocaml
  x
|>
( fun x ->
    g x   
)
```
Left-to-right also allows the following providing the left operand fits on a 
single line:

```ocaml
myref :=
  some value
```

Something I am not sure about is `@@`. It flows right to left, but is often used
to apply a function to a block without increasing its indentation like:
```ocaml
Lwt_main.run @@
lwt code
  that is long
```

Following the rules this is formatted as:

```ocaml
Lwt_main.run @@
  lwt code
    that is long
```

That does show better the structure of the code, but it also defeats the purpose
of `@@` which in my opinion is to erase that structure. The above is not that
different from:

```ocaml
Lwt_main.run
  ( lwt code
      that is long
  )
```

You still have 2 less indents and one less `)` but its not much compared to the
first non-indented layout. 

### Similar construct should have the same formatting

This is is really obvious, but its easy to mess up because a lot of syntax nodes
that are completely distinct actually represent the same concept, for instance 
anonymous function have a very similar structure to functors and classes:

```ocaml
let f = fun x -> y

class c = fun x -> object method m = y end

module F = function (X:S) -> struct ... end
```

These CST node for these are completely distinct, but they share formatting code
to ensure the formatting is the same. Same for a few other constructs, notably
nodes that are analoguous to let bindings.

Sharing such code is important for LLM assisted programming because it nudges 
the LLM into making the change everywhere even if its asked only for one of the 
construct.

# ocst

ocst is a concrete syntax tree for ocaml. It provides a parser that has been
implemented from scratch in recursive descent.

A concrete syntax tree is similar to an abstract syntax tree, but preserves a 
lot more from the source. It can mean that there is a bijection with the 
sequence of lexing tokens, but here we take it to mean bijection to the source.

This means all the whitespace and comments are preserved in the CST. This works
by having each CST node store each token, and each token stores the whitespace
and comments that precedes it. For comments and token that are at the end, they
are held by the `EOF` token.

Because we have a recursive descent parser, this means we get to define the type
of token ourselves, and it uses polymorphic variants so that which tokens CST 
node can hold is enforced by the type-checker. This gives more confidence for
LLM-assisted programming and also makes the CST types very legible at a glance:

```ocaml
  | Exp_setfield of
      expr * [ `DOT ] tok * lident longident * [ `LESSMINUS ] tok * expr
```
Its quite obvious that this is `e.x <- f`, even without knowing the vocabulary.
Here "set field" is quite transparent, but its less the case for "send" :
```ocaml
 | Exp_send of expr * [ `HASH ] tok * lident
```
Which is a method call. Send is vocabulary from smalltalk, but not everyone 
knows that, and even if knowing if you are out of context that this is about OO
it could not come to mind. The `` `HASH `` token make that impossible to miss
provided you know OCaml syntax.