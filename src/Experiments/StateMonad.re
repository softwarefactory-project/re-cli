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
  let runState: (t('a), state) => (state, 'a);
};

module State = (S: {type t;}) : (STATE with type state = S.t) => {
  type t('a) = state => (state, 'a);
  type state = S.t;
  module Monad = {
    type t('a) = state => (state, 'a);
    let bind = (m: t('a), f: 'a => t('b)): t('b) =>
      s =>
        switch (s->m) {
        | (s, a) => f(a, s)
        };
    let return = (a, s) => (s, a);
  };
  let get = s => (s, s);
  let put = (v, s) => (v, ());
  let runState = (m, s) => m(s);
};

module IntState =
  State({
    type t = int;
  });

let count: IntState.t(unit) =
  IntState.(
    IntState.get->Monad.bind(v => {
      Js.log("Counter called");
      (v + 1)->IntState.put;
    })
  );

let (>>) = (a, b) => a->IntState.Monad.bind(_ => b);

Js.log(IntState.runState(count >> count >> count, 0));
