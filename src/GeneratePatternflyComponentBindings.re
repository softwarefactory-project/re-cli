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
  let parsePropComment: parser(option(string)) =
    attempt(
      surround(
        string("/**"),
        spaceAround(regex("[a-zA-Z<>\.\n ]+")),
        string("*/"),
      ),
    )
    <$> (ao => ao->Option.map(~f=a => a[0]));

  let parsePropName: parser(string) =
    spaceAround(regex("[a-zA-Z?]+")) <$> (a => a[0]);

  let parsePropType: parser(string) =
    spaceAround(regex("[a-zA-Z\.]+")) <$> (a => a[0]);

  let parseProp: parser(property) =
    parsePropComment
    >> (lazy(parsePropName))
    >> (lazy(string(":")))
    >> (lazy(parsePropType))
    <$> (((((comment, name), _sep), type_)) => {comment, name, type_});

  let parseProps: parser(list(property)) =
    sepBy(string(";\n"), parseProp) <$> List.fromArray;
};
let one = "/** Sets the */ children?: React.ReactNode;";
let oneV = {
  name: "children?",
  type_: "React.ReactNode",
  comment: Some("Sets the "),
};
Js.log(run(Parser.parseProps, one) |> get_exn == [oneV]);

let two = one ++ "\n" ++ " className?: string; ";
let twoV = [oneV, {name: "className?", type_: "string", comment: None}];
Js.log(run(Parser.parseProps, two) |> get_exn == twoV);

let complete = "
   /** Content rendered inside the breadcrumb title. */
   children?: React.ReactNode;
   /** Additional classes added to the breadcrumb item. */
   className?: string;
   /** HREF for breadcrumb link. */
   to?: string;
   /** Target for breadcrumb link. */
   target?: string;
   /** Sets the base component to render. Defaults to <a> */
   component?: React.ReactNode;
   /** Internal prop set by Breadcrumb on all but the first crumb */
   showDivider?: boolean;
 ";
