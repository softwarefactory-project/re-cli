// A python stdlib (Prelude) in reason

open Tablecloth;

module Os = {
  let listdir = (path: string): list(string) =>
    Node.Fs.readdirSync(path)->List.fromArray;

  let environ = Node.Process.process##env;

  module Path = {
    let expanduser = (path: string): string =>
      path->String.startsWith(~prefix="~/")
        ? environ
          ->Js.Dict.get("HOME")
          ->Option.andThen(~f=homePath =>
              (homePath ++ path->Js.String.substr(~from=1))->Some
            )
          ->Option.unwrap(~default=path)
        : path;
  };
};

module Exception = {
  type t = exn;
  let message = (exn: t): string =>
    exn
    ->Js.Exn.asJsExn
    ->Option.andThen(~f=Js.Exn.message)
    ->Option.unwrap(~default="no message");
};

// Convert exception throwing call into a Result
let catchToResult = (f, x) =>
  switch (f(x)) {
  | v => v->Ok
  | exception e => e->Exception.message->Error
  };

// open() / read() / close() is not easy to model with nodejs because of string encoding
// instead a more simple read_file function:
let read_file = (path: string): Result.t(string, string) =>
  catchToResult(path->Os.Path.expanduser->Node.Fs.readFileSync, `utf8);

let write_file = (data: string, path: string): Result.t(unit, string) =>
  catchToResult(
    path->Os.Path.expanduser->Node.Fs.writeFileSync(data),
    `utf8,
  );

module Json = {
  type t = Js.Json.t;

  let loads = (content: string): Result.t(t, string) =>
    catchToResult(Js.Json.parseExn, content);

  let load = (file_path: string): Result.t(t, string) =>
    file_path->read_file->Result.andThen(~f=loads);
};