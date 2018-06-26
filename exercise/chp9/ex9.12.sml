structure SegmentedRedundantBinaryNumber
          : sig
            datatype Digit = ZERO | ONES of int | TWO | THREES of int | FOUR
            type Nat = Digit list
            val zero : Nat
            val inc : Nat -> Nat
            val dec : Nat -> Nat
          end
 =
struct
  datatype Digit = ZERO | ONES of int | TWO | THREES of int | FOUR
  type Nat = Digit list

  val zero = nil

  fun ones (0, ds) = ds
    | ones (i, ONES j :: ds) = ONES (i + j) :: ds
    | ones (i, ds) = ONES i :: ds

  fun threes (0, ds) = ds
    | threes (i, THREES j :: ds) = THREES (i + j) :: ds
    | threes (i, ds) = THREES i :: ds

  fun simpleInc nil = [ONES 1]
    | simpleInc (ZERO :: ds) = ones (1, ds)
    | simpleInc (ONES i :: ds) = TWO :: ones (i - 1, ds)
    | simpleInc (TWO :: ds) = threes (1, ds)
    | simpleInc (THREES i :: ds) = FOUR :: threes (i - 1, ds)
    | simpleInc (FOUR :: _) = raise Fail "Bug: simpleInc: called with FOUR"

  fun simpleDec nil = raise Fail "Error: dec with ZERO"
    | simpleDec (ZERO :: ds) = raise Fail "Bug: simpleDec: called with ZERO"
    | simpleDec (ONES i :: ds) = ZERO :: ones (i - 1, ds)
    | simpleDec (TWO :: ds) = ones (1, ds)
    | simpleDec (THREES i :: ds) = TWO :: threes (i - 1, ds)
    | simpleDec (FOUR :: ds) = threes (1, ds)

  (*
   * このデータの普遍条件は、G = {2}, Y = {1, 3}, R = {0, 4}
   * に対して、(G|Y|GY*R)*のような数列を維持することである。
   *
   * fixupは先頭のRやYRの普遍条件に違反した並びを変更して正す。
   * fixupによる主な変更は以下の通りである。
   *   fixup (R :: _) -> (G :: _)
   *   fixup (Y :: R :: _) -> (Y :: G :: _)
   *
   * 今回の実装ではinc/decの両方のO(1)を実現するため、繰り上がり、繰り下がりの
   * 対策で2種類の実装を用意する。
   *)
  fun fixup nil = nil
    | fixup (ZERO :: ds) = TWO :: simpleDec ds
    | fixup (ONES i :: ZERO :: ds) = ONES i :: TWO :: simpleDec ds
    | fixup (THREES i :: FOUR :: ds) = THREES i :: TWO :: simpleInc ds
    | fixup (FOUR :: ds) = TWO :: simpleInc ds
    | fixup ds = ds

  fun inc ds = fixup (simpleInc ds)
  fun dec ds = fixup (simpleDec ds)
end
