# Example

What is important is seldom urgent.

A Juvix Markdown file name ends with `.juvix.md`. This kind of file must contain
a module declaration at the top, as shown below ---in the first code block.

```juvix
module Test;
```

Certain blocks can be hidden from the output by adding the `hide` attribute, as shown below.

```juvix hide
import Lib open;
```

```juvix
axiom X : L;
```

The `extract-module-statements` attribute can be used to display only the statements contained in a module in the output.

```juvix extract-module-statements
module Foo1;
  type T := t;
end;
```

You can pass a number to the `extract-module-statements` attribute to drop that number of statements from the start of the module.

```juvix extract-module-statements 1
module Foo2;
  type T := t;
  open T using {t} public;
  a : T := t;
end;
```

Commands like `typecheck` and `compile` can be used with Juvix Markdown files.

```juvix
main : Nat := zero;
```

Other code blocks are not touched, e.g:

```text
This is a text block
```


```haskell
module Test where
```

Blocks indented.

  ```haskell
    module Test where
  ```

Empty blocks:

```
```

We also use other markup for documentation such as:

!!! note

    We use this kind of markup for notes, solutions, and other stuff

    1. More text

        ```text
        f {n : Nat := 0} {m : Nat := n + 1} ....
        ```

    2. Second text


??? info "Solution"

    Initial function arguments that match variables or wildcards in all clauses can
    be moved to the left of the colon in the function definition. For example,

    ```juvix
    module move-to-left;
      import Lib open;

      add (n : Nat) : Nat -> Nat
        | zero := n
        | (suc m) := suc (add n m);
    end;
    ```

    is equivalent to

    ```juvix
      module example-add;
        import Lib open;
        add : Nat -> Nat -> Nat
          | n zero := n
          | n (suc m) := suc (add n m);
      end;
    ```
