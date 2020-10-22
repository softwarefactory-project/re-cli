// A script to generate patternfly binding

open Typescript;
open Tablecloth;

let createProperty = (prop: property): string => {
  let name = prop.name;
  let type_name =
    switch (prop.type_) {
    | Raw(name) => name
    | Enum(enums) =>
      let indent = "         ";
      let v =
        enums
        ->List.map(~f=enum => {
            let enumCap = enum->String.capitalize;
            {j|| [@bs.as "$(enum)"] `$(enumCap)|j};
          })
        ->List.join(~sep="\n" ++ indent);
      {j|[@bs.string][
$(indent)$(v)
$(indent)] |j};
    };

  let type_ = type_name ++ (prop.required ? "" : "=?");
  {j|~$(name): $(type_)|j};
};

let createComponent = (def: componentInterface): string => {
  let name = def.name;
  let properties =
    def.properties->List.map(~f=createProperty)->List.join(~sep=",\n      ");
  {j|module $(name) = {
  [@react.component] [@bs.module "@patternfly/react-core"]
  external make:
    (
      $(properties)
    ) => React.element = "$(name)";
}|j};
};

let create = (defs: definitions): string => {
  defs.interfaces->List.map(~f=createComponent)->List.join(~sep="\n");
};
