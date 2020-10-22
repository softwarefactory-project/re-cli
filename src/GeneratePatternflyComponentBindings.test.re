include GeneratePatternflyComponentBindings;
open Tablecloth;
open BsParse.Combinators;

let equal = (a, name, b) => {
  a != b
    ? {
      Js.log2("Failed to decode:", name);
      Js.log(a);
      Js.log("Got:");
      Js.log(b);
      true;
    }
    : {
      Js.log2("Success:", name);
      false;
    };
};

let basic = () => {
  let typescriptProperties = "
export interface BreadcrumbHeadingProps extends React.HTMLProps<HTMLLIElement> {
   /** Content rendered inside the breadcrumb title. */
   children?: React.ReactNode;
   /** Additional classes added to the breadcrumb item. */
   className?: string;
   to?: string;
}";

  let res =
    run(
      Parser.parseComponentProps("BreadcrumbHeading"),
      typescriptProperties,
    )
    |> get_exn;
  res->equal(
    "basic",
    [|
      {
        name: "children?",
        type_: "React.ReactNode",
        comment: Some("Content rendered inside the breadcrumb title. "),
      },
      {
        name: "className?",
        type_: "string",
        comment: Some("Additional classes added to the breadcrumb item. "),
      },
      {name: "to?", type_: "string", comment: None},
    |],
  );
};

Node.Process.exit([basic]->List.any(~f=t => t()) ? 1 : 0);
