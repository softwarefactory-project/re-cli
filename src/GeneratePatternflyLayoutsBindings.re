open Tablecloth;
let base =
  "~/src/github.com/patternfly/patternfly-react/packages/react-core/src/layouts/"
  ->Python.Os.Path.expanduser;

let dest =
  "~/src/softwarefactory-project.io/software-factory/re-patternfly/src/PFLayouts.re"
  ->Python.Os.Path.expanduser;

let selectedLayouts = [
  "Bullseye",
  "Flex",
  "Gallery",
  "Grid",
  "Level",
  "Split",
  "Stack",
];

let layouts =
  base
  ->Python.Os.listdir
  ->List.filter(~f=List.includes(selectedLayouts, ~equal=(==)))
  ->List.map(~f=x => base ++ x ++ "/")
  ->List.filter(~f=x => x->Node.Fs.existsSync)
  ->List.map(~f=x => x->Python.Os.listdir->List.map(~f=f => x ++ f))
  ->List.flatten
  ->List.filter(~f=x => x->String.endsWith(~suffix=".tsx"))
  ->List.sort(~compare=String.compare)
  ->List.reverse;

Js.log(layouts);

let (ok, failed) =
  layouts
  ->List.map(~f=PatternflyBindings.process)
  ->PatternflyBindings.partitionResult;

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
  )->ignore;
failed->List.join(~sep="\n")->Js.log;
