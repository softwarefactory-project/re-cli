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

let interface = [
  () => {
    (
      run(Parser.parseComponentInterface, "export interface BreadProps { }")
      |> get_exn
    )
    ->equal("simpleInterface", {name: "Bread", properties: []});
  },
  () => {
    (
      run(
        Parser.parseComponentInterface,
        "export interface BreadProps extends React.HTMLProps<HTMLLIElement> { }",
      )
      |> get_exn
    )
    ->equal("basicInterface", {name: "Bread", properties: []});
  },
  () => {
    (
      run(
        Parser.parseComponentInterface,
        "export interface AlertProps extends Omit<React.HTMLProps<HTMLDivElement>, 'action' | 'title'>, OUIAProps { }",
      )
      |> get_exn
    )
    ->equal("alertInterface", {name: "Alert", properties: []});
  },
  () => {
    let typescriptExpr = "export interface AboutModalProps {
  /** Content rendered inside the about modal  */
  children: React.ReactNode;
  /** Additional classes added to the about modal  */
  className?: string;
  /** Flag to show the about modal  */
  isOpen?: boolean;
  /** A callback for when the close button is clicked  */
  onClose?: () => void;
  /** Product name  */
  productName?: string;
  /** Trademark information  */
  trademark?: string;
  /** The URL of the image for the brand  */
  brandImageSrc: string;
  /** The alternate text of the brand image  */
  brandImageAlt: string;
  /** The URL of the image for the background  */
  backgroundImageSrc?: string;
  /** Prevents the about modal from rendering content inside a container; allows for more flexible layouts  */
  noAboutModalBoxContentContainer?: boolean;
  /** The parent container to append the modal to. Defaults to document.body */
  appendTo?: HTMLElement | (() => HTMLElement);
  /** Set aria label to the close button */
  closeButtonAriaLabel?: string;
}";
    (run(Parser.parseComponentInterface, typescriptExpr) |> get_exn).name
    ->equal("AboutModal", "AboutModal");
  },
  () => {
    let typescriptExpr = "export interface AlertProps {
  /** Adds Alert variant styles  */
  variant?: 'success' | 'danger' | 'warning' | 'info' | 'default';
  /** Flag to indicate if the Alert is inline */
  isInline?: boolean;
  /** Title of the Alert  */
  title: React.ReactNode;
  /** Close button; use the AlertActionCloseButton component  */
  actionClose?: React.ReactNode;
  /** Action links; use a single AlertActionLink component or multiple wrapped in an array or React.Fragment */
  actionLinks?: React.ReactNode;
  /** Content rendered inside the Alert */
  children?: React.ReactNode;
  /** Additional classes added to the Alert  */
  className?: string;
  /** Adds accessible text to the Alert */
  'aria-label'?: string;
  /** Variant label text for screen readers */
  variantLabel?: string;
  /** Flag to indicate if the Alert is in a live region */
  isLiveRegion?: boolean;
  /** If set to true, the time out is 8000 milliseconds.  If a number is provided, alert will be dismissed after that amount of time in milliseconds. */
  timeout?: number | boolean;
  /** Function to be executed on alert timeout. Relevant when the timeout prop is set */
  onTimeout?: () => void;
  /** Truncate title to number of lines */
  truncateTitle?: number;
  /** Position of the tooltip which is displayed if text is truncated */
  tooltipPosition?: 'auto' | 'top' | 'bottom' | 'left' | 'right';
}";
    typescriptExpr->String.dropLeft(~count=872)->Js.log;
    (run(Parser.parseComponentInterface, typescriptExpr) |> get_exn).name
    ->equal("Alert", "Alert");
  },
  () => {
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
            {
              name: "to",
              required: true,
              type_: Raw("string"),
              comment: None,
            },
          ],
        },
      );
  },
];

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
  ->PatternflyBindings.getComponents
  ->equal(
      "reason",
      {|module Button = {
  [@react.component] [@bs.module "@patternfly/react-core"]
  external make:
    (
      ~iconPosition: [@bs.string][
         | [@bs.as "left"] `Left
         | [@bs.as "right"] `Right
         ] =?,
      ~className: string=?,
      ~children: 'children=?
    ) => React.element = "Button";
}|},
    );
};
let show =
  Fun.(
    Parser.parseFile >> PatternflyBindings.getComponents >> Js.log >> Result.ok
  );
let button = "~/src/github.com/patternfly/patternfly-react/packages/react-core/src/components/Button/Button.tsx";
button->Python.read_file->Result.andThen(~f=show);

let tests =
  List.append(props, List.append(interface, [enum, file, reason]));

Node.Process.exit(tests->List.any(~f=t => !t()) ? 1 : 0);
