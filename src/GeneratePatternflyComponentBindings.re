open Tablecloth;
let base =
  "~/src/github.com/patternfly/patternfly-react/packages/react-core/src/components/"
  ->Python.Os.Path.expanduser;

let dest =
  "~/src/softwarefactory-project.io/software-factory/re-patternfly/src/PFComponents.re"
  ->Python.Os.Path.expanduser;

let selectedComponents = [
  "AboutModal",
  "Accordion",
  "Alert",
  "AlertGroup",
  "ApplicationLauncher",
  "Avatar",
  "Backdrop",
  "BackgroundImage",
  "Badge",
  "Banner",
  "Brand",
  "Breadcrumb",
  "Button",
  "Card",
  "Checkbox",
  "ClipboardCopy",
  "DescriptionList",
  "Divider",
  "Drawer",
  "Dropdown",
  "EmptyState",
  "Form",
  "Hint",
  "InputGroup",
  "Label",
  "LabelGroup",
  "List",
  "LoginPage",
  "Menu",
  "Nav",
  "Page",
  "Radio",
  "Spinner",
  "Splitter",
  "Tabs",
  "Text",
  "TextArea",
  "TextInput",
  "Tile",
  "Title",
  "Toolbar",
  "Tooltip",
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

let printList = f =>
  Js.log(
    selectedComponents
    ->List.sort(~compare=String.compare)
    ->List.map(~f)
    ->List.join(~sep="\n"),
  );

/*
 let printReasonList = () => printList(name => "  \"" ++ name ++ "\",");
 printReasonList();
 */
let printReadmeList = () => printList(name => "- " ++ name);

let (ok, failed) =
  components
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
  );
printReadmeList();
failed->List.join(~sep="\n")->Js.log;
