module Sugar

open Fragment

val applicant: Variant
val child: Variant

val medicalStatement: Documentation
val bankStatement: Documentation
val receipt: Documentation
val probable: Documentation

val s: string -> Variant
val n: float -> Variant

val (%->): Variant -> string -> Variant

val (%=): Variant -> Variant -> Predicate
val (%<>): Variant -> Variant -> Predicate
val (%<): Variant -> Variant -> Predicate
val (%>): Variant -> Variant -> Predicate

val in_: Variant -> Variant list -> Predicate
val notIn: Variant -> Variant list -> Predicate
val exhausted: string list -> Predicate

val requires: Documentation -> Predicate

val (%&): Condition -> Condition -> Condition
val (%|): Condition -> Condition -> Condition
val not_: Condition -> Condition

val assertion: string -> Predicate -> Condition
val judgement: string -> Condition

val all: Condition list -> Condition
val any: Condition list -> Condition

val fragment: string -> string -> Condition -> t
