// A module to study monadic parser combinator
// https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf
open Tablecloth;

type char_list = list(char);
type parse_result('a) = ('a, char_list);
type parser('a) = char_list => list(parse_result('a));

let result: 'a => parser('a) = (v, inp) => [(v, inp)];
let zero: parser('a) = _inp => [];
let item: parser(char) =
  inp =>
    switch (inp) {
    | [] => []
    | [x, ...xs] => [(x, xs)]
    };

let andThen: (parser('a), 'a => parser('b)) => parser('b) =
  (ap, f, inp) => ap(inp) |> List.flatMap(~f=((v, inpa)) => f(v, inpa));

let seq: (parser('a), parser('b)) => parser(('a, 'b)) =
  (pa, pb) => pa->andThen(x => pb->andThen(y => result((x, y))));

let sat: (char => bool) => parser(char) =
  p => item->andThen(x => p(x) ? result(x) : zero);

let pchar: char => parser(char) = x => sat(y => x == y);
let digit: parser(char) = sat(x => x >= '0' && x <= '9');
let lower: parser(char) = sat(x => x >= 'a' && x <= 'z');
let upper: parser(char) = sat(x => x >= 'A' && x <= 'Z');

let plus: (parser('a), parser('a)) => parser('a) =
  (p, q, inp) => List.append(p(inp), q(inp));

let letter = plus(lower, upper);
let alphanum = plus(letter, digit);

let word: parser(char_list) =
  inp => {
    let rec go = (acc, inp) =>
      switch (letter(inp)) {
      | [] => [(acc->List.reverse, inp)]
      | [(x, xs)] => go(List.cons(acc, x), xs)
      | _ => []
      };
    go([], inp);
  };

let rec pstring: char_list => parser(char_list) =
  s =>
    switch (s) {
    | [] => result([])
    | [x, ...xs] =>
      pchar(x)
      ->andThen(_ => pstring(xs)->andThen(_ => result(List.cons(xs, x))))
    };

let many: parser('a) => parser(list('a)) =
  (p, inp) => {
    let rec go = (acc, inp) =>
      switch (p(inp)) {
      | [] => [(acc->List.reverse, inp)]
      | [(x, xs)] => go(List.cons(acc, x), xs)
      | _ => []
      };
    go([], inp);
  };

let run: (parser(char_list), string) => (string, string) =
  (p, inp) =>
    switch (p(inp->String.toList)) {
    | [(v, rest)] => (v->String.fromList, rest->String.fromList)
    | _ => ("", "")
    };

let runm: (parser(list(char_list)), string) => (string, string) =
  (p, inp) =>
    switch (p(inp->String.toList)) {
    | [(v, rest)] => (
        v->List.flatten->String.fromList,
        rest->String.fromList,
      )
    | _ => ("", "")
    };

Js.log(run(word, "aa!"));
Js.log(runm(many(pstring("Hello"->String.toList)), "HelloHello World!"));
