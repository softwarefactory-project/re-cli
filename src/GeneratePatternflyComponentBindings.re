// A script to generate patternfly binding

open Tablecloth;
open BsParse.Combinators;
open BsParse.CommonCombinators;

type property = {
  name: string,
  type_: string,
  comment: option(string),
};

module Parser = {
  // Get the first result of a regexp parser
  let regex1 = s => regex(s) <$> r => r[0]

  let parsePropComment: parser(option(string)) =
    attempt(spaceAround(surround(
      string("/**"),
      spaceAround(regex1("[a-zA-Z<>\\.\\n ]+")),
      string("*/"),
    )))

  let parsePropName: parser(string) = spaceAround(regex1("[a-zA-Z?]+"))

  let parsePropType: parser(string) = spaceAround(regex1("[a-zA-Z\\.]+"))

  let parseProp: parser(property) =
    parsePropComment >>= comment =>
    parsePropName >>=    name =>
    string(":") >>=      _ =>
    parsePropType <$>    type_ => {name, type_, comment}


  let parseProps: parser(array(property)) = sepBy(string(";\n"), parseProp)
  let toto = 42;
  let parseComponentProps = (name) => {
    let def = {j|export interface $(name)Props extends React.HTMLProps<HTMLLIElement> {|j};
    spaceAround(string(def)) >>= _ =>
    parseProps >>=               props =>
    spaceAround(string("}")) <$> _ => props
  }
};
