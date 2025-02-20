module Lib;

axiom L : Type;

type Nat :=
  | zero
  | suc Nat;

open Nat using {zero; suc} public;
