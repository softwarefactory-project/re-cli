// Functions similar to the haskell Prelude

open Tablecloth;

let flip = (f, a, b) => f(b, a);

let note = (value: option('a), message: 'b): Result.t('a, 'b) =>
  switch (value) {
  | Some(v) => v->Ok
  | None => message->Error
  };

let pureResult = v => v->Ok;
