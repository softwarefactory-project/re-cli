include Typescript;
open Tablecloth;
open BsParse.Combinators;

let equal = (a, name, b) => {
  a != b
    ? {
      Js.log3("Failure: decode", name, ", got:");
      Js.log(a);
      Js.log("Wanted:");
      Js.log(b);
      false;
    }
    : {
      Js.log2("Success:", name);
      true;
    };
};

let props = [
  () =>
    (run(Parser.parsePropTypeEnum, "'ok''  ") |> get_exn)
    ->equal("enum", ["ok"]),
  () =>
    (run(Parser.parsePropTypeEnum, "'ok' | 'info'  ") |> get_exn)
    ->equal("enums", ["ok", "info"]),
  () =>
    (run(Parser.parsePropTypeRaw, "React.ReactNode") |> get_exn)
    ->equal("raw", "React.ReactNode"),
  () =>
    (run(Parser.parsePropType, " 'ok' | 'warning'") |> get_exn)
    ->equal("combinedEnum", Enum(["ok", "warning"])),
  () =>
    (run(Parser.parsePropType, " React.ReactNode") |> get_exn)
    ->equal("combinedRaw", Raw("React.ReactNode")),
];

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
            type_: Raw("React.ReactNode"),
            required: false,
            comment: Some("Content rendered inside the breadcrumb title. "),
          },
          {
            name: "className",
            required: false,
            type_: Raw("string"),
            comment:
              Some("Additional classes added to the breadcrumb item. "),
          },
          {name: "to", required: true, type_: Raw("string"), comment: None},
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

export interface BreadcrumbHeadingProps extends React.HTMLProps<HTMLLIElement>, OUIAProps {
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
                type_: Raw("string"),
                comment: Some("Multi line\n   comment. "),
              },
              {
                name: "to",
                required: true,
                type_: Raw("string"),
                comment: None,
              },
            ],
          },
        ],
        enums: [{name: "ButtonVariant", values: ["primary", "secondary"]}],
      },
    );
};

let reason = () => {
  let typescriptExpr = "export interface ButtonProps extends React.HTMLProps<HTMLButtonElement>, OUIAProps {
  /** Content rendered inside the button */
  children?: React.ReactNode;
  /** Additional classes added to the button */
  className?: string;
  /** Sets position of the link icon */
  iconPosition?: 'left' | 'right';
}
";
  typescriptExpr
  ->Parser.parseFile
  ->PatternflyBindings.create
  ->equal(
      "reason",
      {|module Button = {
  [@react.component] [@bs.module "@patternfly/react-core"]
  external make:
    (
      ~children: React.ReactNode=?,
      ~className: string=?,
      ~iconPosition: [@bs.string][
         | [@bs.as "left"] `Left
         | [@bs.as "right"] `Right
         ] =?
    ) => React.element = "Button";
}|},
    );
};
let show =
  Fun.(Parser.parseFile >> PatternflyBindings.create >> Js.log >> Result.ok);
let button = "~/src/github.com/patternfly/patternfly-react/packages/react-core/src/components/Button/Button.tsx";
button->Python.read_file->Result.andThen(~f=show);

let tests = List.append(props, [interface, enum, file, reason]);

Node.Process.exit(tests->List.any(~f=t => !t()) ? 1 : 0);
