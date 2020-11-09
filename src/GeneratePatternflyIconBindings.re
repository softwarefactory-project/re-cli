// A script to generate patternfly binding

open Tablecloth;
open Python;
open Haskell;

let convert = (l: array((string, string)), s: string): string =>
  l->Map.String.fromArray->Map.get(s)->Option.unwrap(~default=s);

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

let getPatternflyIconsList = (json: Json.t): Result.t(list(string), string) => {
  json
  ->Js.Json.decodeObject
  ->Option.andThen(~f=decoded => decoded->Js.Dict.keys->List.fromArray->Some)
  ->note("Expected a dictionary");
};

let createReasonBinding =
    (icons: list(string), nameConverter: string => string): list(string) =>
  icons->List.map(~f=icon => {
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

let addFAList = (pfList: list(string)) =>
  pfList
  ->List.append([
      // TODO: get the list automatically
      "AngleDoubleLeft",
      "AngleDoubleRight",
      "AngleDown",
      "AngleLeft",
      "AngleRight",
      "AngleUp",
      "ArrowCircleDown",
      "ArrowCircleUp",
      "ArrowRight",
      "ArrowsAltV",
      "BalanceScale",
      "Ban",
      "Bars",
      "Bug",
      "CaretDown",
      "CheckCircle",
      "Check",
      "ClipboardCheck",
      "CodeBranch",
      "Code",
      "Cog",
      "Columns",
      "CompressArrowsAlt",
      "Compress",
      "Copy",
      "Cube",
      "Cubes",
      "Database",
      "Desktop",
      "Download",
      "Dropbox",
      "Drupal",
      "EllipsisV",
      "ExclamationCircle",
      "ExclamationTriangle",
      "ExpandArrowsAlt",
      "Expand",
      "ExternalLinkAlt",
      "FacebookSquare",
      "Filter",
      "Flag",
      "Folder",
      "FolderOpen",
      "Github",
      "Gitlab",
      "Google",
      "GripHorizontal",
      "GripVertical",
      "Home",
      "InfoCircle",
      "Linkedin",
      "Linux",
      "Lock",
      "LockOpen",
      "LongArrowAltDown",
      "LongArrowAltUp",
      "MapMarker",
      "Memory",
      "Microchip",
      "MinusCircle",
      "Minus",
      "OutlinedCalendarAlt",
      "OutlinedClock",
      "OutlinedComments",
      "OutlinedHdd",
      "OutlinedQuestionCircle",
      "OutlinedWindowRestore",
      "PauseCircle",
      "Pause",
      "PencilAlt",
      "Play",
      "PlusCircle",
      "Plus",
      "PowerOff",
      "Print",
      "QuestionCircle",
      "Redo",
      "Search",
      "SearchMinus",
      "SearchPlus",
      "ShareSquare",
      "SortAmountDownAlt",
      "SortAmountDown",
      "StackOverflow",
      "SyncAlt",
      "Tag",
      "Thumbtack",
      "TimesCircle",
      "Times",
      "Trash",
      "Twitter",
      "Undo",
      "User",
      "Users",
      "Windows",
      "Wrench",
    ])
  ->pureResult;

let dest = "~/src/softwarefactory-project.io/software-factory/re-patternfly/src/PFIcons.re";

let pfIcons =
  "~/src/github.com/patternfly/patternfly/src/icons/definitions/pf-icons.json"
  ->Json.load
  ->Result.andThen(~f=getPatternflyIconsList)
  ->Result.andThen(~f=addFAList)
  ->Result.andThen(~f=xs =>
      xs
      ->createReasonBinding(convertPf)
      ->List.sort(~compare=String.compare)
      ->pureResult
    );

/*
 let allIcons =
   "~/src/softwarefactory-project.io/software-factory/sf-ui/node_modules/@patternfly/react-icons/dist/js/icons/"
   ->Os.Path.expanduser
   ->Os.listdir
   ->List.filter(~f=n => String.endsWith(n, ~suffix=".js"))
   ->List.map(~f=n => String.dropRight(n, ~count="Icon.js"->String.length))
   ->createReasonBinding(convertPf)
   ->List.join(~sep="\n");
 */

Js.log(
  switch (
    pfIcons->Result.andThen(~f=xs =>
      xs->List.join(~sep="\n")->write_file(dest)
    )
  ) {
  | Ok(_) => "Written: " ++ dest
  | Error(e) => "Error: " ++ e
  },
);
