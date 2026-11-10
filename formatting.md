# Formatting choices

This document is human written, and explain the formatting decisions made in
fromage. It is meant as both documentation of intent, so that if future
maintainer find inconsistent formatting, they can know which is intended, and as
a basis for discussing improvements. Its not exhaustive yet. Comments welcome !

## Context giving symbols should be at the start of the line

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

## No diagonal separators

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

## Parent nodes should be less intented than their children

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

## Repeated constructs should be packed

This one is a bit more abstract. What we mean by that is that various "lists"
will have the same behavior. By list I mean any construct where you can repeat
an element an arbirary number of times, like a tuple, or function arguments.

First lets describe this behavior with function arguments:

```ocaml
f a b c (looooong arguuuumennnnnnt) d e
```

will be formatted
```ocaml
f
  a b c
  ( looooong
      arguuuumennnnnnt
  )
  d e
```
This idea is the following: we "pack" that is put together multiple args on a
single line when possible, but multiline elements such as the one in `()` are
isolated with breaks before and after them.

This is used for a lot of other construct such as list and array litterals,
tuples.

Some list constructs dont use that and instead are either all one line or each
on its own line. An example of this is sequence (`a ; b`) or match branches.

There is some tradeoff between the two:

```ocaml
{ a;
  b;
  c;
  d;
  e;
  f;
  g;
  h = (fun x -> looooooong bodyyyy; print "hello"; does_not fit )
}
```
is very tedious compared to

```ocaml
{ a; b; c; d; e; f; g;
  h = (fun x -> looooooong bodyyyy; print "hello"; does_not fit )
}
```
but if you have a bunch of moderately complex expression the all-or-nothing
behavior might be more readable:
```ocaml
{ a = (fun x -> reasonably_short body );
  b = (fun x -> reasonably_short body );
  c = (fun x -> reasonably_short body );
  d = (fun x -> reasonably_short body );
}
```
compared to
```ocaml
{ a = (fun x -> reasonably_short body ); b = (fun x -> reasonably_short body );
  c = (fun x -> reasonably_short body ); d = (fun x -> reasonably_short body );
}
```

The solution to this maybe be to sort the elements of the list between "simple"
and "complex" ones. The simple ones would be allowed to be grouped on one line
but not the complex ones.

Still, I am afraid there is no shortcut here, it need to be rewritten for every
piece of the CST is used on. Or maybe a smart trick on the number of tokens,
like more than 3 tokens would be complex ? But arguably `A.B.c` is simple were
`fun x -> y` is complex and the first has more tokens.

So for now there is no such smart decision and we always pack. Ideas welcome !

## Balancing the lines

I said above that when elements of a list construct fit on one line, they should
be packed. But there are multiple ways to do that:

```ocaml
(* 1 *)
{ a = aaaaaaaa aaaaaa aaaaa; b = bbbbbb bbbbbbbbb bbbbbbb;
  c = cccccccccc; d = dddddddd dddddddddd;
}
(* 2 *)
{ a = aaaaaaaa aaaaaa aaaaa; b = bbbbbb bbbbbbbbb bbbbbbb; c = cccccccccc;
  d = dddddddd dddddddddd;
}
(* 3 *)
{ a = aaaaaaaa aaaaaa aaaaa;
  b = bbbbbb bbbbbbbbb bbbbbbb; c = cccccccccc; d = dddddddd dddddddddd;
}
```

Are all valid and fit withing 80 columns, with the same number of line and same
indentation. Number (3.) is pretty bad so its not considered, but there is an
argument to be made between (1.) and (2.). The choice made in fromage is (1.),
because I believe short lines are more readable and it produces the shortest
lines. It works by picking the formatting that minimize the square of line
lengths.

However it has the disadvantage that adding an extra element might change
line above it to re-balance:
```ocaml
{ a = aaaaaaaa aaaaaa aaaaa; b = bbbbbb bbbbbbbbb bbbbbbb;
  c = cccccccccc; d = dddddddd dddddddddd; e = eeeeeeeee eeeeeeeeeeeeeeeeeeeeeee
}
(* sum of squares of line length is 58^2 + 80^2 + 1^2 = 10_089*)

{ a = aaaaaaaa aaaaaa aaaaa; b = bbbbbb bbbbbbbbb bbbbbbb; c = cccccccccc;
  d = dddddddd dddddddddd; e = eeeeeeeee eeeeeeeeeeeeeeeeeeeeeee
}
(* sum of squares of line length is 75^2 + 64^2 + 1^2 = 9_722 *)
```
This choice is not set in stone, I am open to arguments about it. Also, given
its implemented by a top-level declaration of what is prefered (pretty
expressive's cost factory), it would be very easy to make into a user option
without disturbing the implementation, similar to the margin.

## Infix operators

Infix operators are a bit difficult, because they share a syntax node even when
their intent is very different. For instance `+` is very symmetrical when `:=`
or `|>` are not.

Because of this we classify operators into three categories: symmetrical,
left-to-right and right-to-left. They have different indentation.

## Symmetrical operators

With symmetrical operators, the operator is less indented than the operands:

```ocaml
  long computation
+
  other long computation
```

## Directionnal operators

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

## Similar construct should have the same formatting

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
