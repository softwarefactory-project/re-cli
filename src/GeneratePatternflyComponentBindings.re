open Tablecloth;
let base =
  "~/src/github.com/patternfly/patternfly-react/packages/react-core/src/components/"
  ->Python.Os.Path.expanduser;

let dest =
  "~/src/softwarefactory-project.io/software-factory/re-patternfly/src/PFComponents.re"
  ->Python.Os.Path.expanduser;

let selectedComponents = [
  "Alert",
  "AlertGroup",
  "Avatar",
  "Badge",
  "Banner",
  "Brand",
  "Breadcrumb",
  "Button",
  "DescriptionList",
  "Divider",
  "Drawer",
  "Dropdown",
  "EmptyState",
  "Checkbox",
  "Card",
  "Form",
  "List",
  "LoginPage",
  "Nav",
  "Page",
  "Text",
  "TextArea",
  "TextInput",
];

let components =
  base
  ->Python.Os.listdir
  ->List.filter(~f=List.includes(selectedComponents, ~equal=(==)))
  ->List.map(~f=x => base ++ x ++ "/")
  ->List.filter(~f=x => x->Node.Fs.existsSync)
  ->List.map(~f=x => x->Python.Os.listdir->List.map(~f=f => x ++ f))
  ->List.flatten
  ->List.filter(~f=x => x->String.endsWith(~suffix=".tsx"))
  ->List.sort(~compare=String.compare)
  ->List.reverse;

let create = (c: string): Result.t(list(string), string) =>
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
        ->PatternflyBindings.create
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

let (ok, failed) =
  components->List.map(~f=create)->PatternflyBindings.partitionResult;

let annoteError = (~msg: string) => Result.mapError(~f=e => msg ++ e);

ok
->List.flatten
->List.join(~sep="\n\n")
->Python.pread("bsrefmt", [||])
->annoteError(~msg="BsRefmt failed:")
//->Result.andThen(~f=c => c->Js.log->Ok)
->Result.andThen(~f=Fun.flip(Python.write_file, dest))
->Result.andThen(~f=() => Js.log(dest ++ ": updated")->Ok)
->Result.mapError(~f=e =>
    Js.log3("Failed:", e, ok->List.flatten->List.join(~sep="\n"))
  );

failed->List.join(~sep="\n")->Js.log;
