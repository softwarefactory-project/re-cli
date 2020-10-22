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
      false;
    }
    : {
      Js.log2("Success:", name);
      true;
    };
};

let interface = () => {
  let typescriptExpr = "export interface BreadcrumbHeadingProps extends React.HTMLProps<HTMLLIElement> {
     /** Content rendered inside the breadcrumb title. */
     children?: React.ReactNode;
     /** Additional classes added to the breadcrumb item. */
     className?: string;
     to: string;
  }";

  (run(Parser.parseComponentInterface, typescriptExpr) |> get_exn)
  ->equal(
      "interface",
      {
        name: "BreadcrumbHeading",
        properties: [
          {
            name: "children",
            type_: "React.ReactNode",
            required: false,
            comment: Some("Content rendered inside the breadcrumb title. "),
          },
          {
            name: "className",
            required: false,
            type_: "string",
            comment:
              Some("Additional classes added to the breadcrumb item. "),
          },
          {name: "to", required: true, type_: "string", comment: None},
        ],
      },
    );
};

let enum = () => {
  let typescriptExpr = "export enum ButtonVariant {
  primary = 'primary',
  secondary = 'secondary'
}
";

  (run(Parser.parseEnum, typescriptExpr) |> get_exn)
  ->equal(
      "enum",
      {name: "ButtonVariant", values: ["primary", "secondary"]},
    );
};

let file = () => {
  let typescriptFile = "
// A comment

unknown expr here;

export interface BreadcrumbHeadingProps extends React.HTMLProps<HTMLLIElement> {
   /** Multi line
   comment. */
   className?: string;
   to: string;
}

export enum ButtonVariant {
  primary = 'primary',
  secondary = 'secondary'
}
";

  Parser.parseFile(typescriptFile)
  ->equal(
      "file",
      {
        interfaces: [
          {
            name: "BreadcrumbHeading",
            properties: [
              {
                name: "className",
                required: false,
                type_: "string",
                comment: Some("Multi line\n   comment. "),
              },
              {name: "to", required: true, type_: "string", comment: None},
            ],
          },
        ],
        enums: [{name: "ButtonVariant", values: ["primary", "secondary"]}],
      },
    );
};

Node.Process.exit([interface, enum, file]->List.any(~f=t => !t()) ? 1 : 0);
