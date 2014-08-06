## Adding builtin functions

If you would like to add a builtin function to rcalc, it's relatively simple.

First, open src/lib/types/operator.rs in your text editor of choice.

For a single builtin function, if it's related to one of the existing groups 
(Gate, Arith, ListOps, OrderEq, Introspect, XForms, RoundId, MatrixOps,
Transcendental), just go to that enumeration, and do the following:
add the name of the builtin to that enumeration. Let's say it's FooQ for
Introspect (in keeping with the naming scheme). 
Next, you would go to the implementation of FromStr for Introspect, and add 
`"foo?" => Some(Foo),` 
above 
`_ => None`
Next, you would add in the fmt::Show implementation for Introspect
`Foo => "foo?",`
somewhere in the existing matching statement.
Finally, you would add to the Help implementation for Introspect
`Foo => "Determine if the term satisfies foo.",`

The process is the same for single builtins for adding to OperatorType.

If instead you want to add another enumeration of functions, you
would define that enumeration, implement Show, FromStr and Help for it,
and add it to OperatorType as another member.

The next step in this process is a bit more complicated.
If you added to Arith, go to src/calc/operator/arithmetic.rs, and
figure it out from there. I'm not sure what crazy arithmetic procedure you're
adding, but go nuts if you have it I guess.
If you're adding to ListOps or XForms, go to src/calc/operator/listops.rs.
If you're adding to ListOps, modify the list_ops function; otherwise
you'll modify transform_ops. Transcend is located in operator/trig.rs,
Ordering, RoundIdent, Logic and Query are in operator/logic.rs.
MatrixOps is in src/calc/matrice.rs, and the rest are in
operator/special.rs. If you've added to OperatorTypes, you'll
just modify eval in operator/mod.rs.

After modifying the appropriate matching function, you just need to write
the function for the builtin you've added (or functions, if you've added
another enumeration of them). Happy hacking.

## Modifying types and parse

Got another type you want to add? Go to src/lib/types/literal.rs and add it 
to LiteralTypes there. Modify Show and WithEnv to accomodate your changes.
Additionally consider if there are appropriate methods you should add
to LiteralTypes implementation. Having a method like is_foo(&self) -> bool
and to_foo(&self) -> Result<Foo> are good ideas.

If you have some method of uniquely identifying your type when inputting
a string, consider going to src/lib/parse and adding to the Token enum.
If you do this, you'll also need to add a function which returns
a MaybeToken<Token, ErrorKind> which matches against a static string,
then adding it to the rules in the parse function.