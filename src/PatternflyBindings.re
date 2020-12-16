// A script to generate patternfly binding

open Typescript;
open Tablecloth;

let nameToList = name =>
  name->String.endsWith(~suffix="[]")
    ? "array(" ++ name->String.dropRight(~count=2) ++ ")" : name;

let createProperty =
    (acc: list((int, Result.t(string, string))), prop: property)
    : list((int, Result.t(string, string))) => {
  let prev_idx =
    switch (acc->List.head) {
    | None => 0
    | Some((idx, _)) => idx
    };
  let uniquify = name =>
    switch (prev_idx) {
    | 0 => (1, name->Ok)
    | n => (n + 1, (name ++ n->string_of_int)->Ok)
    };
  let error = n => (prev_idx, n->Error);
  let ok = n => (prev_idx, n->Ok);
  (
    switch (prop.type_) {
    | Raw("OrderType") =>
      // todo: get from the List.tsx enum, but we need to thread the definitions down here
      "skip-order"->error
    | Raw("Size") =>
      // todo: dito but for Title.tsx
      "skip-size"->error
    | Raw("RefObject<HTMLDivElement>")
    | Raw("RefObject<any>")
    | Raw("HTMLElement")
    | Raw("React.Ref<any>")
    | Raw("React.RefObject<any>") => "skip-ref"->error
    | Raw("React.ReactNode") => "'children"->uniquify
    | Raw("React.ReactNode[]") => "array('children)"->ok
    | Raw("React.ReactElement<any>")
    | Raw("React.ElementType")
    | Raw("React.ComponentType<any>")
    | Raw("React.ElementType<any>")
    | Raw("React.ReactElement") => "React.element"->ok
    | Raw("string | BackgroundImageSrcMap")
    | Raw("number | string")
    | Raw("string | number") => "string"->ok
    | Raw("boolean") => "bool"->ok
    | Raw("number") => "int"->ok
    | Raw("any[]")
    | Raw("any")
    | Raw("object")
    | Raw("PageGroupProps")
    | Raw("todo") => "'any"->uniquify
    | Raw("NavSelectClickHandler")
    | Raw("ReactEvent.Mouse.t => unit") => "ReactEvent.Mouse.t => unit"->ok
    | Raw("gridSpans")
    | Raw("gridItemSpanValueShape") => "PFTypes.Column.t"->ok
    | Raw("ListVariant.inline") =>
      "[@bs.string] [ | [@bs.as \"inline\"] `Inline]"->ok
    | Func(
        "checked: boolean, event: React.FormEvent<HTMLInputElement>",
        "void",
      ) =>
      "(bool, ReactEvent.Mouse.t) => unit"->ok
    | Func("value: string, event: React.FormEvent<HTMLInputElement>", "void") =>
      "(string, ReactEvent.Mouse.t) => unit"->ok
    | Func("event: React.MouseEvent<HTMLButtonElement, MouseEvent>", "void") =>
      "ReactEvent.Mouse.t => unit"->ok
    | Func(
        "checked: boolean, event: React.FormEvent<HTMLInputElement>",
        "void",
      ) =>
      "(bool, ReactEvent.Mouse.t) => unit"->ok
    | Func(input, output) => ("skip-func: " ++ input ++ output)->error
    | Inline(inline) => ("skip-inline: " ++ inline)->error
    | Array(ar) => ("skip-array: " ++ ar)->error
    | Raw(rawName) =>
      let name = rawName->nameToList;
      name->String.includes(~substring=" ") ? name->error : name->ok;
    | Enum(enums) =>
      let indent = "         ";
      let v =
        enums
        ->List.map(~f=enum => {
            let enumCap =
              enum->String.capitalize |> Js.String.replace("-", "");
            {j|| [@bs.as "$(enum)"] `$(enumCap)|j};
          })
        ->List.join(~sep="\n" ++ indent);
      ok({j|[@bs.string][
$(indent)$(v)
$(indent)] |j});
    }
  )
  ->Tuple2.mapSecond(~f=r =>
      r->Result.andThen(~f=type_name => {
        let name =
          switch (prop.name) {
          | "type" => "_type"
          | "to" => "_to"
          | o => o
          };
        let type_ = type_name ++ (prop.required ? "" : "=?");
        name->String.includes(~substring="-")
          ? name->Error : {j|~$(name): $(type_)|j}->Ok;
      })
    )
  |> List.cons(acc);
};

let partitionResult = (xs: list(Result.t('a, 'b))): (list('a), list('b)) => {
  let rec go = (xs, os, es) =>
    switch (xs) {
    | [] => (os, es)
    | [Ok(o), ...rest] => rest->go(List.cons(os, o), es)
    | [Error(e), ...rest] => rest->go(os, List.cons(es, e))
    };
  go(xs, [], []);
};

let extraProps = (name: string): list(property) => {
  let style = {
    name: "style",
    required: false,
    type_: Raw("ReactDOM.Style.t"),
    comment: None,
  };
  let onClick = {
    name: "onClick",
    required: false,
    type_: Raw("ReactEvent.Mouse.t => unit"),
    comment: None,
  };
  let id_ = {
    name: "id",
    required: true,
    type_: Raw("string"),
    comment: None,
  };

  let navcb =
    ["onSelect", "onToggle"]
    ->List.map(~f=name =>
        {name, required: false, type_: Raw("'callback"), comment: None}
      );

  let onClickComponents = ["Card", "Button", "Brand"];
  let styleComponents = ["Page", "ListItem", "NavItem", "NavList"];

  [
    (name->String.startsWith(~prefix="Card"), [style]),
    (styleComponents->List.includes(name, ~equal=String.equal), [style]),
    (
      onClickComponents->List.includes(name, ~equal=String.equal),
      [onClick],
    ),
    (List.includes(["Nav"], name, ~equal=String.equal), navcb),
    (List.includes(["TextInput"], name, ~equal=String.equal), [id_]),
  ]
  ->List.map(~f=((enabled, props)) => enabled ? props : [])
  ->List.flatten;
};

let createComponent = (def: componentInterface): (string, list(string)) => {
  let name = def.name;
  let properties =
    List.append(def.properties, extraProps(name))
    ->List.sort(~compare=(a, b) =>
        String.compare(a.name, b.name)->Int.negate
      );
  let (propertiesOk, propertiesFailure) =
    properties
    ->List.sort(~compare=(a, b) =>
        a.name == "children" ? (-1) : String.compare(a.name, b.name)
      )
    ->List.fold(~f=createProperty, ~initial=[])
    ->List.unzip
    ->Tuple2.second
    ->partitionResult;
  let properties = propertiesOk->List.join(~sep=",\n      ");
  (
    {j|module $(name) = {
  [@react.component] [@bs.module "@patternfly/react-core"]
  external make:
    (
      $(properties)
    ) => React.element = "$(name)";
}|j},
    propertiesFailure,
  );
};

// Returns the list (component, list of unknown properties)
let create = (defs: definitions): list((string, list(string))) => {
  defs.interfaces->List.map(~f=createComponent);
};

let getComponents = d =>
  d->create->List.map(~f=((c, _e)) => c)->List.join(~sep="\n");

// Process a typescript file
let process = (c: string): Result.t(list(string), string) =>
  c
  ->(
      x => {
        Js.log("Processing: " ++ x);
        c;
      }
    )
  ->Python.read_file
  ->Result.andThen(~f=content => {
      let defs = content->Typescript.Parser.parseFile;
      switch (defs.interfaces) {
      | [_x, ..._xs] =>
        defs
        ->create
        ->List.map(~f=((component, unknownProps)) =>
            switch (unknownProps) {
            | [] => component
            | up =>
              Js.log2("Could not create properties for: ", up->List.toArray);
              component;
            }
          )
        ->Ok
      //      | [_x, ..._xs] => ("Multiple interface found " ++ c)->Error
      | [] => ("No interface found " ++ c)->Error
      };
    });
