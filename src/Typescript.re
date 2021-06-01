// A Typescript definitions parser

// The expressions are going to be loaded as the following types:
type propertyType =
  | Enum(list(string))
  | Raw(string)
  | Func(string, string)
  | Array(string)
  | Inline(string);

let enum = x => x->Enum;
let raw = x => x->Raw;
let func = (input, output) => Func(input, output);
let arrayP = x => x->Array;
let inline = x => x->Inline;

type property = {
  name: string,
  required: bool,
  type_: propertyType,
  comment: option(string),
};

type componentInterface = {
  name: string,
  properties: list(property),
};

type enumGlobal = {
  name: string,
  values: list(string),
};

type definitions = {
  interfaces: list(componentInterface),
  enums: list(enumGlobal),
};

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

let empty = {interfaces: [], enums: []};

module Parser = {
  // Get the first result of a regexp parser
  let regex1: string => parser(string) = s => regex(s) <$> (r => r[0]);

  let parsePropComment: parser(string) =
    spaceAround(
      surround(
        string("/**"),
        // Parse as many characters or new-lines that are not `*/`
        many(regex1("(.|\n)(?!\\*\\/)"))
        ->andThen(xs =>
            regex1(".")->andThen(x => xs->Belt.Array.concat([|x|])->unit)
          ),
        string("*/"),
      )
      // Assemble the array of characters back to a single string
      ->fmap(xs => xs->Array.join(~sep="")->String.trim),
    );

  let parsePropName: parser(string) =
    spaceAround(regex1("[a-zA-Z'][a-zA-Z0-9'\\-]+"));

  let parsePropTypeEnum: parser(list(string)) =
    spaceAround(
      attempt(spaceAround(string("|")))
      ->andThen(_ =>
          sepBy(
            spaceAround(string("|")),
            surround(
              regex1("['(]*"),
              regex1("[a-zA-Z0-9][a-zA-Z0-9\\.<>\\-]*"),
              regex1("[')\\[\\]]*"),
            ),
          )
        ),
    )
    ->fmap(List.fromArray);

  let parsePropTypeObject: parser(string) =
    spaceAround(regex1("{[^}]+}"))->fmap(_ => "todo");

  let parsePropTypeRaw: parser(string) =
    spaceAround(regex1("[a-zA-Z][a-zA-Z?<>:,'=() |\\[\\]\\.]+"));

  let parsePropTypeInline: parser(string) =
    spaceAround(
      surround(string("("), many(regex1("(.|\n)(?!\\))")), string(")")),
    )
    ->fmap(xs => xs->Array.join(~sep="")->String.trim);

  let parsePropTypeFunc: parser((string, string)) =
    spaceAround(
      surround(string("("), regex1("[^)]*"), string(")"))
      ->andThen(input =>
          spaceAround(string("=>"))
          ->andThen(_ => parsePropTypeRaw)
          ->fmap(output => (input, output))
        ),
    );

  let parsePropTypeArray: parser(string) =
    spaceAround(surround(string("("), regex1("[^)]*"), string(")")))
    ->andThen(input => string("[]")->andThen(_ => input->unit));

  let parsePropType: parser(propertyType) =
    parsePropTypeRaw
    ->fmap(raw)
    ->orElse(lazy(parsePropTypeFunc->fmap(((x, y)) => func(x, y))))
    ->orElse(lazy(parsePropTypeArray->fmap(arrayP)))
    ->orElse(lazy(parsePropTypeInline->fmap(inline)))
    ->orElse(lazy(parsePropTypeObject->fmap(raw)))
    ->orElse(lazy(parsePropTypeEnum->fmap(enum)));

  let parseOptional: parser(bool) =
    attempt(spaceAround(string("?")))->fmap(Option.isNone);

  let parseProp: parser(property) =
    attempt(parsePropComment)
    ->andThen(comment =>
        parsePropName->andThen(name =>
          parseOptional->andThen(required =>
            string(":")
            ->andThen(_ =>
                spaceAround(parsePropType)
                ->fmap(type_ => {name, type_, comment, required})
              )
          )
        )
      );

  let parseProps: parser(array(property)) =
    sepBy(string(";\n"), parseProp);

  let parseWord: parser(string) = regex1("[^', ]+");

  let parseExtend: parser(string) =
    string("extends ")->andThen(_ => regex1("[a-zA-Z'|<>,\\.\\n ]+"));

  let parseComponentName: parser(string) = regex1("[^ ]+Props");

  let parseComponentInterface: parser(componentInterface) =
    surround(
      string("export interface"),
      spaceAround(parseComponentName),
      attempt(parseExtend),
    )
    ->andThen(name =>
        surround(string("{"), parseProps, string("}"))
        ->fmap(props =>
            {
              name:
                name
                ->String.trim
                ->String.dropRight(~count="Props"->String.length),
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

  let parseEnum: parser(enumGlobal) =
    string("export enum ")
    ->andThen(_ =>
        parseWord->andThen(name =>
          surround(string(" {"), parseEnumValues, string("}"))
          ->fmap(values => {name, values: values->List.fromArray})
        )
      );

  module Expr = {
    type t =
      | Interface(componentInterface)
      | Enum(enumGlobal)
      | Unknown(string);

    let parseExpr: parser(t) =
      parseComponentInterface
      ->fmap(x => x->Interface)
      ->orElse(lazy(parseEnum->fmap(x => x->Enum)))
      ->orElse(lazy(regex1(".*")->fmap(x => x->Unknown)));
  };
  let parseFile = (content: string): definitions => {
    // This is a bit odd, we try line by line to parse the expression we care about
    let rec go = (lines: list(string), acc: definitions) =>
      switch (lines) {
      | [] => acc
      | [x, ...xs] =>
        let v = run(Expr.parseExpr, List.cons(xs, x)->List.join(~sep="\n"));
        let defs =
          switch (v |> get_exn) {
          | Expr.Interface(i) => {
              ...acc,
              interfaces: List.cons(acc.interfaces, i),
            }
          | Expr.Enum(e) => {...acc, enums: List.cons(acc.enums, e)}
          | Expr.Unknown(_l) =>
            //            Js.log("Skipping:" ++ l);
            acc
          };
        go(xs, defs);
      };
    go(content->String.split(~on="\n"), empty);
  };
};
