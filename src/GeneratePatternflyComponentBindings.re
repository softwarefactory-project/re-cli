// A script to generate patternfly binding

open Tablecloth;
open BsParse.Combinators;
open BsParse.CommonCombinators;

// Definition from bs-parse for documentation purpose:
// type parser('a)
// type parse_result('a) = result(('a, Location.t), parse_error)
// let run: parser('a) => string => parse_result('a)
// let attempt: parser('a) => parser(option('a))
// let surround: (parser('open), parser('body), parser('close)) => parser('body)
// let string: string => parser(string)
// let spaceAround: parser('a) => parser('a)
// let sepBy: parser('b) => parser('a) => parser(array('a))

// Infix operator functions:
let fmap: (parser('a), 'a => 'b) => parser('b) = BsParse.Combinators.DP.map;
let andThen: (parser('a), 'a => parser('b)) => parser('b) = BsParse.Combinators.BasicCombinators.flatMap;

// Haskell equivalent operator types:
// fmap is:    flip fmap :: Functor f => f a -> (a ->   b) -> f b
// andThen is: (>>=)     :: Monad m   => m a -> (a -> m b) -> m b

type property = {
  name: string,
  type_: string,
  comment: option(string),
};

module Parser = {
  // Get the first result of a regexp parser
  let regex1: string => parser(string) = s => regex(s) <$> (r => r[0]);

  let parsePropComment: parser(option(string)) =
    attempt(
      spaceAround(
        surround(
          string("/**"),
          spaceAround(regex1("[a-zA-Z<>\\.\\n ]+")),
          string("*/"),
        ),
      ),
    );

  let parsePropName: parser(string) = spaceAround(regex1("[a-zA-Z?]+"));

  let parsePropType: parser(string) = spaceAround(regex1("[a-zA-Z\\.]+"));

  let parseProp: parser(property) =
    parsePropComment->andThen(comment =>
      parsePropName->andThen(name =>
        string(":")
        ->andThen(_ => parsePropType->fmap(type_ => {name, type_, comment}))
      )
    );

  let parseProps: parser(array(property)) =
    sepBy(string(";\n"), parseProp);

  let parseComponentProps: string => parser(array(property)) =
    name => {
      let def = {j|export interface $(name)Props extends React.HTMLProps<HTMLLIElement> {|j};
      spaceAround(string(def))
      ->andThen(_ =>
          parseProps->andThen(props =>
            spaceAround(string("}"))->fmap(_ => props)
          )
        );
    };
};
