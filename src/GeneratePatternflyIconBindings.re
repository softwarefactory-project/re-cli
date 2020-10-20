// A script to generate patternfly binding
open Python;
open Haskell;

let convert = (l: array((string, string)), s: string): string =>
  l->Belt.Map.String.fromArray->Belt.Map.String.getWithDefault(s, s);

let convertPf: string => string =
  // Adapter lifted from `react-icons/scripts/icons/patternflyIcons.js`
  [|
    ("save", "save-alt"),
    ("folder-open", "folder-open-alt"),
    ("edit", "edit-alt"),
    ("print", "print-alt"),
    ("spinner", "spinner-alt"),
    ("home", "home-alt"),
    ("memory", "memory-alt"),
    ("server", "server-alt"),
    ("user", "user-sec"),
    ("users", "users-alt"),
    ("info", "info-alt"),
    ("filter", "filter-alt"),
  |]
  ->convert;

let toCamel: string => string = [%raw
  {|
  (s) => {
// Raw JS Copied from `react-icons/scripts/writeIcons.js`
const removeSnake = s =>
  s
    .toUpperCase()
    .replace('-', '')
    .replace('_', '');
const toCamel = s => `${s[0].toUpperCase()}${s.substr(1).replace(/([-_][\w])/gi, removeSnake)}`;

    return toCamel(s)
  }
|}
];

let getPatternflyIconsList =
    (json: Json.t): Belt.Result.t(list(string), string) => {
  json
  ->Js.Json.decodeObject
  ->Belt.Option.flatMap(decoded =>
      decoded->Js.Dict.keys->Belt.List.fromArray->Some
    )
  ->note("Expected a dictionary");
};

let createReasonBinding =
    (icons: list(string), nameConverter: string => string): list(string) =>
  icons->Belt.List.map(icon => {
    let jsName = toCamel(nameConverter(icon));
    {j|module $(jsName) = {
  [@react.component] [@bs.module "@patternfly/react-icons"]
  external make:
    (
      ~size: [@bs.string] [
               | [@bs.as "sm"] `SM
               | [@bs.as "md"] `MD
               | [@bs.as "lg"] `LG
               | [@bs.as "xl"] `XL
             ]
               =?,
      ~color: string=?,
      ~title: string=?,
      ~noVerticalAlign: bool=?
    ) =>
    React.element = "$(jsName)Icon";
};
|j};
  });

let dest = "~/src/softwarefactory-project.io/software-factory/re-patternfly/src/PFIcons.re";

Js.log(
  switch (
    "~/src/github.com/patternfly/patternfly/src/icons/definitions/pf-icons.json"
    ->Json.load
    ->Belt.Result.flatMap(getPatternflyIconsList)
    ->Belt.Result.flatMap(xs =>
        xs->createReasonBinding(convertPf)->pureResult
      )
    ->Belt.Result.flatMap(xs => xs->Str.join("\n")->write_file(dest))
  ) {
  | Ok(_) => "Written: " ++ dest
  | Error(e) => "Error: " ++ e
  },
);
