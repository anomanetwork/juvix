module Test;

import Lib open;

axiom X : L;

module Foo1;
  type T := t;

  open T using {t} public;
end;

module Foo2;
  type T := t;

  open T using {t} public;

  a : T := t;
end;

main : Nat := zero;

module move-to-left;
  import Lib open;

  add (n : Nat) : Nat -> Nat
    | zero := n
    | (suc m) := suc (add n m);
end;

module example-add;
  import Lib open;

  add : Nat -> Nat -> Nat
    | n zero := n
    | n (suc m) := suc (add n m);
end;
