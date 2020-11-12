// Can a module implement this type?
module type FromSome = {let fromSome: option('a) => 'a;};

// This work because the type var 'a is specialized to int
let fromSome = (_x: option('a)): 'a => 42;
// That means call site such as `fromSome (Some("str"))` doesn't type check

// With module type, we can ensure the implementation to respect the types,
// and thus the above fromSome type is impossible:
/*
 module Test: FromSome = {
   let fromSome = (x: option('a)): 'a => 42;
 };
 */
// The `Test` module fails to build with this error:
/*
   Signature mismatch:
   Modules do not match:
     { let fromSome: option(int) => int; }
   is not included in
     FromSome
   Values do not match:
     let fromSome: option(int) => int
   is not included in
     let fromSome: option('a) => 'a
 */
