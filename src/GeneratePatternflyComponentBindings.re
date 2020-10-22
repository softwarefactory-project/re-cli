// A script to generate patternfly binding

open Tablecloth;
open BsParse.Combinators;
open BsParse.CommonCombinators;

let andThen = BsParse.Combinators.BasicCombinators.flatMap;
let map = BsParse.Combinators.DP.map;

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
        ->andThen(_ => parsePropType->map(type_ => {name, type_, comment}))
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
            spaceAround(string("}"))->map(_ => props)
          )
        );
    };
};
