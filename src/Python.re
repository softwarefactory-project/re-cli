// A python stdlib (Prelude) in reason

module Str = {
  let startswith = (str: string, prefix: string): bool =>
    Js.String.startsWith(prefix, str);
};

module Os = {
  let listdir = (path: string): list(string) => {
    Node.Fs.readdirSync(path)->Belt.List.fromArray;
  };
  module Path = {
    let expanduser = (path: string): string =>
      if (path->Str.startswith("~/")) {
        Node.Process.process##env
        ->Js.Dict.get("HOME")
        ->Belt.Option.flatMap(h =>
            Some(h ++ path->Js.String.substr(~from=1))
          )
        ->Belt.Option.getWithDefault(path);
      } else {
        path;
      };
  };
};

// open() / read() / close() is not easy to model with nodejs because of string encoding
// instead a more simple read_file function:
let read_file = (path: string): string => {
  path->Os.Path.expanduser->Node.Fs.readFileSync(`utf8);
};

module Exception = {
  type t = exn;
  let message = (exn: t): string => {
    Belt.Option.getWithDefault(
      exn->Js.Exn.asJsExn->Belt.Option.flatMap(Js.Exn.message),
      "no message",
    );
  };
};

module Json = {
  type t = Js.Json.t;

  let loads = (content: string): Belt.Result.t(t, string) =>
    switch (content->Js.Json.parseExn) {
    | x => x->Ok
    | exception e => e->Exception.message->Error
    };
  let load = (file_path: string): Belt.Result.t(t, string) =>
    file_path->read_file->loads;
};

// Utility functions
let note = (value: option('a), message: 'b): Belt.Result.t('a, 'b) => {
  switch (value) {
  | Some(v) => v->Ok
  | None => message->Error
  };
};

let ok = (f: 'a => 'b, v: 'a): Belt.Result.t('b, string) => v->f->Ok;
