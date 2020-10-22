// A script to generate patternfly binding

open Typescript;
open Tablecloth;

let nameToList = name =>
  name->String.endsWith(~suffix="[]")
    ? "list(" ++ name->String.dropRight(~count=2) ++ ")" : name;

let createProperty = (prop: property): Result.t(string, string) => {
  (
    switch (prop.type_) {
    | Raw("React.ReactNode") => "'children"->Ok
    | Raw("React.ElementType<any>") => "React.element"->Ok
    | Raw("boolean") => "bool"->Ok
    | Raw("number") => "int"->Ok
    | Raw("ReactEvent.Mouse.t => unit") => "ReactEvent.Mouse.t => unit"->Ok
    | Raw(
        "(checked: boolean, event: React.FormEvent<HTMLInputElement>) => void",
      ) =>
      "(bool, ReactEvent.Mouse.t) => unit"->Ok
    | Raw(rawName) =>
      let name = rawName->nameToList;
      name->String.includes(~substring=" ") ? name->Error : name->Ok;
    | Enum(enums) =>
      let indent = "         ";
      let v =
        enums
        ->List.map(~f=enum => {
            let enumCap = enum->String.capitalize;
            {j|| [@bs.as "$(enum)"] `$(enumCap)|j};
          })
        ->List.join(~sep="\n" ++ indent);
      Ok({j|[@bs.string][
$(indent)$(v)
$(indent)] |j});
    }
  )
  ->Result.andThen(~f=type_name => {
      let name =
        switch (prop.name) {
        | "type" => "type_"
        | o => o
        };
      let type_ = type_name ++ (prop.required ? "" : "=?");
      name->String.includes(~substring="-")
        ? name->Error : {j|~$(name): $(type_)|j}->Ok;
    });
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

  [
    (name->String.startsWith(~prefix="Card"), [style]),
    (name == "Card", [onClick]),
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
    properties->List.map(~f=createProperty)->partitionResult;
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
  d->create->List.map(~f=((c, e)) => c)->List.join(~sep="\n");
