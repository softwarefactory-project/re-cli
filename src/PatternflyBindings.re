// A script to generate patternfly binding

open Typescript;
open Tablecloth;

let createProperty = (prop: property): Result.t(string, string) => {
  (
    switch (prop.type_) {
    | Raw("React.ReactNode") => "'children"->Ok
    | Raw("boolean") => "bool"->Ok
    | Raw("number") => "int"->Ok
    | Raw(name) =>
      name->String.includes(~substring=" ") ? name->Error : name->Ok
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
      let name = prop.name;
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

let createComponent = (def: componentInterface): (string, list(string)) => {
  let name = def.name;
  let (propertiesOk, propertiesFailure) =
    def.properties->List.map(~f=createProperty)->partitionResult;
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

let create = (defs: definitions): list((string, list(string))) => {
  defs.interfaces->List.map(~f=createComponent);
};

let getComponents = d =>
  d->create->List.map(~f=((c, e)) => c)->List.join(~sep="\n");
