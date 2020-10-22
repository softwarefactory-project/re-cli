// A script to generate patternfly binding

open Tablecloth;
open BsParse.Combinators;
open BsParse.CommonCombinators;

// Definition from bs-parse for documentation purpose:
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
  required: bool,
  type_: string,
  comment: option(string),
};

type componentInterface = {
  name: string,
  properties: list(property),
};

type enum = {
  name: string,
  values: list(string),
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

  let parsePropName: parser(string) = spaceAround(regex1("[a-zA-Z]+"));

  let parsePropType: parser(string) = spaceAround(regex1("[a-zA-Z\\.]+"));

  let parseOptional: parser(bool) =
    attempt(spaceAround(string("?")))->fmap(Option.isNone);

  let parseProp: parser(property) =
    parsePropComment->andThen(comment =>
      parsePropName->andThen(name =>
        parseOptional->andThen(required =>
          string(":")
          ->andThen(_ =>
              parsePropType->fmap(type_ => {name, type_, comment, required})
            )
        )
      )
    );

  let parseProps: parser(array(property)) =
    sepBy(string(";\n"), parseProp);

  let parseWord: parser(string) = regex1("[^' ]+");

  let parseComponentInterface: parser(componentInterface) =
    surround(
      string("export interface"),
      parseWord,
      string("extends React.HTMLProps<HTMLLIElement> "),
    )
    ->andThen(name =>
        surround(string("{"), parseProps, string("}"))
        ->fmap(props =>
            {
              name: name->String.dropRight(~count="Props"->String.length),
              properties: props->List.fromArray,
            }
          )
      );

  let parseEnumValue: parser(string) =
    spaceAround(parseWord)
    ->andThen(_ =>
        spaceAround(string("="))
        ->andThen(_ => surround(string("'"), parseWord, string("'")))
      );

  let parseEnumValues: parser(array(string)) =
    sepBy(string(","), parseEnumValue);

  let parseEnum: parser(enum) =
    string("export enum ")
    ->andThen(_ =>
        parseWord->andThen(name =>
          surround(string(" {"), parseEnumValues, string("}"))
          ->fmap(values => {name, values: values->List.fromArray})
        )
      );

  module Ast = {
    type t = {
      interfaces: list(componentInterface),
      enums: list(enum),
    };
    type expr =
      | Interface(componentInterface)
      | Enum(enum)
      | Unknown(string);
    let empty = {interfaces: [], enums: []};

    let parseExpr: parser(expr) =
      parseComponentInterface
      ->fmap(x => x->Interface)
      ->orElse(lazy(parseEnum->fmap(x => x->Enum)))
      ->orElse(lazy(regex1(".*")->fmap(x => x->Unknown)));
  };
  let parseFile = (content: string): Ast.t => {
    // This is a bit odd, because BsParse doesn't tell how much has been parsed...
    // we try line by line to parse an expression
    let rec go = (lines: list(string), acc: Ast.t) =>
      switch (lines) {
      | [] => acc
      | [x, ...xs] =>
        let v = run(Ast.parseExpr, List.cons(xs, x)->List.join(~sep="\n"));
        let ast =
          switch (v |> get_exn) {
          | Ast.Interface(i) => {
              ...acc,
              interfaces: List.cons(acc.interfaces, i),
            }
          | Ast.Enum(e) => {...acc, enums: List.cons(acc.enums, e)}
          | Ast.Unknown(l) =>
            Js.log("Skipping:" ++ l);
            acc;
          };
        go(xs, ast);
      };
    go(content->String.split(~on="\n"), Ast.empty);
  };
};
