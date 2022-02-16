// A simple effect system
module type Effect = {
  // An effect produce an 'a
  type t('a);

  // Functor
  let fmap: ('a => 'b, t('a)) => t('b);

  // Applicative
  let pure: 'a => t('a);
  let ap: (t('a => 'b), t('a)) => t('b);

  // Monad
  let andThen: (t('a), 'a => t('b)) => t('b);

  // Interpret "run" the effect
  let interpret: t('a) => 'a;

  // The available effects:
  let print: string => t(unit);
  let getLine: t(string);
  let getRandomNumber: t(int);

  // debug
  let getLog: unit => list(string);
};

module DerivedEffects = (EFFECT: Effect) => {
  open EFFECT;
  // Some utility functions
  let then_ = (ea: t('a), eb: t('b)): t('b) => ea->andThen(_ => eb);
  let rec sequence: list(t('a)) => t(list('a)) =
    le =>
      switch (le) {
      | [] => pure([])
      | [x, ...xs] =>
        x->andThen(a => fmap(la => Belt.List.add(la, a), sequence(xs)))
      };

  let mapM = (f: 'a => t('b), xs: list('a)): t(list('b)) =>
    xs->Belt.List.map(f)->sequence;

  let joinList: t(list(string)) => t(string) =
    xs => fmap(x => x->Tablecloth.List.join(~sep=" "), xs);

  // Composed effects
  let prompt: string => t(string) = s => print(s ++ ": ")->then_(getLine);
  let andPrint: t(string) => t(unit) = e => e->andThen(print);
};

// An example program using the effect system
module Main = (EFFECT: Effect) => {
  open EFFECT;
  module DE = DerivedEffects(EFFECT);
  include DE;

  let getInfo = mapM(prompt, ["Name?", "Age?"]);

  let getThreeLines =
    getLine->andThen(l1 =>
      getLine->andThen(l2 =>
        getLine->andThen(l3 =>
          print("got three lines: " ++ l1 ++ " " ++ l2 ++ " " ++ l3)
        )
      )
    );
  let getThreeLinesBis =
    [getLine, getLine, getLine]
    ->sequence
    ->andThen(xs =>
        switch (xs) {
        | [l1, l2, l3] =>
          print("got three lines: " ++ l1 ++ " " ++ l2 ++ " " ++ l3)
        | _ => print("got error")
        }
      );
  let playRandom =
    print("Is random even?")
    ->then_(
        getRandomNumber->andThen(n =>
          n mod 2 == 0 ? print("Even") : print("Odd")
        ),
      );

  let run = () =>
    interpret(
      [
        playRandom,
        getThreeLines,
        getThreeLinesBis,
        getInfo->joinList->andPrint,
      ]
      ->sequence,
    );
  let getLog = getLog;
};

// A JS implementation
module JSWorld: Effect = {
  type t('a) = unit => 'a;
  // let interpret = (f: t('a)): 'a => f();
  let interpret = effect => effect();
  let fmap = (f, e, ()) => f(interpret(e));
  let pure = (x, ()) => x;
  let ap = (ff, ea) => {
    let f = interpret(ff);
    let a = interpret(ea);
    let b = f(a);
    pure(b);
  };
  let andThen = (x, f, ()) => interpret(f(interpret(x)));
  let getLog = () => [];

  let print = (s, ()) => Js.log(s);
  let getLine = () => "user-input"; // TODO: check how to read stdin
  let getRandomNumber = () => Js.Math.random_int(0, 100);
};

// Running the program using the two implementation:
Js.log("\nJs effects:");
module JSMain = Main(JSWorld);
Js.log(JSMain.run);
JSMain.run()->ignore;

// A Test implementation
module TestWorld: Effect = {
  type t('a) = 'a;
  let log = ref([]);
  // let interpret = (x: t('a)): 'a => x;
  let interpret = Tablecloth.Fun.identity;
  let fmap = (f, e) => f(interpret(e));
  let pure = x => x;
  let ap = (ff, ea) => {
    let f = interpret(ff);
    let a = interpret(ea);
    let b = f(a);
    pure(b);
  };
  let andThen = (x, f) => f(interpret(x));
  let getLog = () => log.contents;

  let print = s => {
    log.contents = List.cons(s, log.contents);
    ();
  };
  let getRandomNumber = 42;
  let getLine = "user-input";
};

Js.log("\nTest effects:");
module TestMain = Main(TestWorld);
TestMain.run()->ignore;
Js.log(TestMain.getLog()->Tablecloth.List.toArray);
