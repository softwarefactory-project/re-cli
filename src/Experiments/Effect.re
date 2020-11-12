// A simple effect system
module type Effect = {
  // An effect produce an 'a
  type t('a);
  // Interpret "run" the effect
  let interpret: t('a) => 'a;
  // andThen combine two effect
  let andThen: (t('a), 'a => t('b)) => t('b);

  // The available effects:
  let print: string => t(string);
  let getRandomNumber: (int => t('a)) => t('a);
};

// A JS implementation
module JSWorld: Effect = {
  type t('a) =
    | Print(string, 'a)
    | GetRandomNumber(int => t('a));
  let rec interpret = (x: t('a)): 'a =>
    switch (x) {
    | Print(x, v) =>
      Js.log(x);
      v;
    | GetRandomNumber(f) => interpret(f(Js.Math.random_int(0, 100)))
    };
  let andThen = (x, f) => f(interpret(x));

  let print = s => Print(s, s);
  let getRandomNumber = f => GetRandomNumber(f);
};

// A Test implementation
module TestWorld: Effect = {
  type t('a) =
    | Print(string, 'a)
    | GetRandomNumber(int => t('a));

  let rec interpret = (x: t('a)): 'a =>
    switch (x) {
    | Print(_x, v) => v
    | GetRandomNumber(f) => interpret(f(5))
    };
  let andThen = (x, f) => f(interpret(x));

  let print = s => Print(s, s);
  let getRandomNumber = f => GetRandomNumber(f);
};

// An example program using the effect system
module Main = (EFFECT: Effect) => {
  open EFFECT;
  let greet =
    print("Is random even?")
    ->andThen(_ =>
        getRandomNumber(n => n mod 2 == 0 ? print("Even") : print("Odd"))
      );
  let run = () => interpret(greet);
};

// Running the program using the two implementation:
module JSMain = Main(JSWorld);
JSMain.run();
module TestMain = Main(TestWorld);
Js.log(TestMain.run());
