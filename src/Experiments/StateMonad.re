// Based on https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html
module type MONAD = {
  type t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
  let return: 'a => t('a);
};

module type STATE = {
  type state;
  type t('a);
  module Monad: MONAD with type t('a) = t('a);
  let get: t(state);
  let put: state => t(unit);
  let runState: (t('a), state) => ('a, state);
};

module State = (S: {type t;}) : (STATE with type state = S.t) => {
  type state = S.t;
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
  State({
    type t = int;
  });

let (>>) = (a, b) => a->IntState.Monad.bind(_ => b);

let count: IntState.t(unit) =
  IntState.(
    IntState.get->Monad.bind(v => {
      Js.log("Counter called");
      (v + 1)->IntState.put;
    })
  );

Js.log(IntState.runState(count >> count >> count, 0));
