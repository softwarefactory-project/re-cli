// A python stdlib (Prelude) in reason

module Str = {
  let startswith = (str: string, prefix: string): bool =>
    Js.String.startsWith(prefix, str);

  let join = (xs: list(string), sep: string): string => {
    let rec go = (acc: string, xs: list(string)): string =>
      switch (xs) {
      | [] => acc
      | [x, ...xs] => go((acc != "" ? acc ++ sep : acc) ++ x, xs)
      };
    go("", xs);
  };
};

module Os = {
  let listdir = (path: string): list(string) =>
    Node.Fs.readdirSync(path)->Belt.List.fromArray;

  let environ = Node.Process.process##env;

  module Path = {
    let expanduser = (path: string): string =>
      path->Str.startswith("~/")
        ? environ
          ->Js.Dict.get("HOME")
          ->Belt.Option.flatMap(homePath =>
              (homePath ++ path->Js.String.substr(~from=1))->Some
            )
          ->Belt.Option.getWithDefault(path)
        : path;
  };
};

module Exception = {
  type t = exn;
  let message = (exn: t): string =>
    exn
    ->Js.Exn.asJsExn
    ->Belt.Option.flatMap(Js.Exn.message)
    ->Belt.Option.getWithDefault("no message");
};

// Convert exception throwing call into a Result
let catchToResult = (f, x) =>
  switch (f(x)) {
  | v => v->Ok
  | exception e => e->Exception.message->Error
  };

// open() / read() / close() is not easy to model with nodejs because of string encoding
// instead a more simple read_file function:
let read_file = (path: string): Belt.Result.t(string, string) =>
  catchToResult(path->Os.Path.expanduser->Node.Fs.readFileSync, `utf8);

let write_file = (data: string, path: string): Belt.Result.t(unit, string) =>
  catchToResult(
    path->Os.Path.expanduser->Node.Fs.writeFileSync(data),
    `utf8,
  );

module Json = {
  type t = Js.Json.t;

  let loads = (content: string): Belt.Result.t(t, string) =>
    catchToResult(Js.Json.parseExn, content);

  let load = (file_path: string): Belt.Result.t(t, string) =>
    file_path->read_file->Belt.Result.flatMap(loads);
};
