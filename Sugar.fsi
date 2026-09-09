namespace Solala

module Sugar =
    open Fragment

    /// The person applying for the benefit.
    val applicant: Variant

    /// A child on whose behalf the application is made.
    val child: Variant

    /// Documentation in form of a medical statement.
    val medicalStatement: Documentation
    /// Documentation in form of a bank statement.
    val bankStatement: Documentation
    /// Documentation in form of payment receipts.
    val receipt: Documentation
    /// Documented probable cause.
    val probable: Documentation

    /// Create a variant value from a string.
    val s: string -> Variant
    /// Create a variant value from a float.
    val n: float -> Variant

    /// Get a sub-property of a value.
    /// For example, child %-> "age" reads the age of the child.
    val (%->): Variant -> string -> Variant

    /// Check whether two values are equal.
    val (%=): Variant -> Variant -> Predicate
    /// Check whether two values are not equal
    val (%<>): Variant -> Variant -> Predicate
    /// Check whether a value is less than another.
    val (%<): Variant -> Variant -> Predicate
    /// Check whether a value is greater than another.
    val (%>): Variant -> Variant -> Predicate

    /// Check whether a value is in a list of values.
    val in_: Variant -> Variant list -> Predicate
    /// Check whether a value is not in a list of values.
    val notIn: Variant -> Variant list -> Predicate
    /// Check whether all referenced paragraphs or benefits have been exhausted.
    val exhausted: string list -> Predicate

    /// Require documentation of some form.
    val requires: Documentation -> Predicate

    /// Require that both conditions are met.
    val (%&): Condition -> Condition -> Condition
    /// Require that at least one condition is met.
    val (%|): Condition -> Condition -> Condition
    /// Require that condition is not met.
    val not_: Condition -> Condition
    /// Require that all conditinos are met.
    val all: Condition list -> Condition
    /// Require that at least one condition is met.
    val any: Condition list -> Condition

    /// Assert a fact.
    val assertion: string -> Predicate -> Condition
    /// Perform a discretionary judgment.
    val judgement: string -> Condition

    /// Create a new fragment from text anchor, benefit and conditions.
    val fragment: string -> string -> Condition -> t
