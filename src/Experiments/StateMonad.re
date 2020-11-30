// Based on https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html
module type MONAD = {
  type t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
  let return: 'a => t('a);
};

module type MONAD_STATE = {
  type state;
  type t('a);
  module Monad: MONAD with type t('a) = t('a);
  let get: t(state);
  let put: state => t(unit);
  let runState: (t('a), state) => ('a, state);
};

module MkStateMonad = (T: {type t;}) : (MONAD_STATE with type state = T.t) => {
  type state = T.t;
  type t('a) = state => ('a, state);
  module Monad = {
    type t('a) = state => ('a, state);
    let bind = (m: t('a), f: 'a => t('b)): t('b) =>
      s => {
        let (a, s) = s->m;
        f(a, s);
      };
    let return = (a, s) => (a, s);
  };
  let get = s => (s, s);
  let put = (v, _s) => ((), v);
  let runState = (m, s) => m(s);
};

module IntState =
  MkStateMonad({
    type t = int;
  });

let (>>=) = (a, f) => a->IntState.Monad.bind(f);
let (>>) = (a, b) => a->IntState.Monad.bind(_ => b);

let addId: string => IntState.t(string) =
  name =>
    IntState.(
      get
      >>= (
        idx =>
          switch (name) {
          | "'children" =>
            (idx + 1)->put >> Monad.return(name ++ "-" ++ string_of_int(idx))
          | _ => Monad.return(name)
          }
      )
    );

let statefulCalculation: IntState.t(string) = {
  addId("'children") >> addId("'children");
};
Js.log(IntState.runState(statefulCalculation, 0));
