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
  let getRandomNumber: t(int);
};

// A JS implementation
module JSWorld: Effect = {
  type t('a) = unit => 'a;
  let interpret = (f: t('a)): 'a => f();
  let andThen = (x, f) => f(interpret(x));

  let print = (s, _) => {
    Js.log(s);
    s;
  };

  let getRandomNumber = _ => Js.Math.random_int(0, 100);
};

// A Test implementation
module TestWorld: Effect = {
  type t('a) = 'a;
  let interpret = (x: t('a)): 'a => x;
  let andThen = (x, f) => f(interpret(x));

  let print = s => s;
  let getRandomNumber = 42;
};

// An example program using the effect system
module Main = (EFFECT: Effect) => {
  open EFFECT;
  let greet =
    print("Is random even?")
    ->andThen(_ =>
        getRandomNumber->andThen(n =>
          n mod 2 == 0 ? print("Even") : print("Odd")
        )
      );
  let run = () => interpret(greet);
};

// Running the program using the two implementation:
module JSMain = Main(JSWorld);
JSMain.run();
module TestMain = Main(TestWorld);
Js.log(TestMain.run() == "Even");
