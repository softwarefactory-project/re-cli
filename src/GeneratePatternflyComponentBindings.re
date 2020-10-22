open Tablecloth;
let base =
  "~/src/github.com/patternfly/patternfly-react/packages/react-core/src/components/"
  ->Python.Os.Path.expanduser;

let dest =
  "~/src/softwarefactory-project.io/software-factory/re-patternfly/src/Patternfly_Generated.re"
  ->Python.Os.Path.expanduser;

let selectedComponents = ["Alert", "AlertGroup"];

let components =
  base
  ->Python.Os.listdir
  ->List.filter(~f=List.includes(selectedComponents, ~equal=(==)))
  ->List.map(~f=x => base ++ x ++ "/")
  ->List.filter(~f=x => x->Node.Fs.existsSync)
  ->List.map(~f=x => x->Python.Os.listdir->List.map(~f=f => x ++ f))
  ->List.flatten
  ->List.filter(~f=x => x->String.endsWith(~suffix=".tsx"));

let create = (c: string): Result.t(string, string) =>
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
      | [_interface] =>
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
        ->List.join(~sep="\n\n")
        ->Ok
      | [_x, ..._xs] => ("Multiple interface found " ++ c)->Error
      | [] => ("No interface found " ++ c)->Error
      };
    });

let (ok, failed) =
  components->List.map(~f=create)->PatternflyBindings.partitionResult;

ok
->List.join(~sep="\n")
->Python.pread("bsrefmt", [||])
->Result.andThen(~f=Fun.flip(Python.write_file, dest))
->Result.andThen(~f=() => Js.log(dest ++ ": updated")->Ok)
->Result.mapError(~f=e => Js.log3("Failed:", e, ok->List.join(~sep="\n")));

failed->List.join(~sep="\n")->Js.log;
