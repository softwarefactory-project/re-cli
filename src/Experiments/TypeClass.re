// A module to study type classes

open Haskell;

// The typeclasses
module type SEMIGROUP = {
  type t;
  let append: (t, t) => t;
};

module type MONOID = {
  include SEMIGROUP;
  let empty: t;
};

module type EQ = {
  type t;
  let eq: (t, t) => bool;
};

module MConcat = (M: MONOID) => {
  let rec go = (xs, acc) =>
    switch (xs) {
    | [] => acc
    | [x, ...xs] => xs->go(M.append(acc, x))
    };
  let f: list(M.t) => M.t = go->flip(M.empty);
};

module String = {
  type t = string;
  let append = (++);
  module Semigroup: SEMIGROUP with type t = string = {
    type t = string;
    let append = append;
  };
  module Monoid: MONOID with type t = string = {
    include Semigroup;
    let empty = "";
  };
  module MConcat = MConcat(Monoid);
};

module Int = {
  type t = int;
  module Sum = {
    let append = (+);
    module Semigroup: SEMIGROUP with type t = int = {
      type t = int;
      let append = append;
    };
    module Monoid: MONOID with type t = int = {
      include Semigroup;
      let empty = 0;
    };
    module MConcat = MConcat(Monoid);
  };
  module Product = {
    let append = ( * );
    module Semigroup: SEMIGROUP with type t = int = {
      type t = int;
      let append = append;
    };
    module Monoid: MONOID with type t = int = {
      include Semigroup;
      let empty = 0;
    };
    module MConcat = MConcat(Monoid);
  };
};
Js.log(String.MConcat.f(["Hello ", "World", "!"]) == "Hello World!");
Js.log(Int.Sum.MConcat.f([0, 1, 2, 3]) == 6);
Js.log(Int.Product.MConcat.f([0, 1, 2, 3]) == 0);
