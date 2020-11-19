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
    (
      run(Parser.parsePropTypeEnum, "
       | 'ok'
       | 'info'")
      |> get_exn
    )
    ->equal("indentedEnums", ["ok", "info"]),
  () =>
    (
      run(
        Parser.parsePropTypeObject,
        "{
    default?: 'padding' | 'noPadding';
    sm?: 'padding' | 'noPadding';
    md?: 'padding' | 'noPadding';
    lg?: 'padding' | 'noPadding';
    xl?: 'padding' | 'noPadding';
    '2xl'?: 'padding' | 'noPadding';
}",
      )
      |> get_exn
    )
    ->equal("enumObject", "todo"),
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
    let typescriptExpr = "export interface TextInputProps {
  /** Type that the input accepts. */
  type?: 'text' | 'date' | 'datetime-local';
}";
    // typescriptExpr->String.dropLeft(~count=99)->Js.log;
    (run(Parser.parseComponentInterface, typescriptExpr) |> get_exn).name
    ->equal("TextInput", "TextInput");
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
    (run(Parser.parseComponentInterface, typescriptExpr) |> get_exn).name
    ->equal("Alert", "Alert");
  },
  () => {
    let typescriptExpr = "export interface LoginPageProps extends React.HTMLProps<HTMLDivElement> {
  /** Anything that can be rendered inside of the LoginPage (e.g. <LoginPageForm>) */
  children?: React.ReactNode;
  /** Additional classes added to the LoginPage. */
  className?: string;
  /** Attribute that specifies the URL of the brand image for the LoginPage */
  brandImgSrc?: string;
  /** Attribute that specifies the alt text of the brand image for the LoginPage. */
  brandImgAlt?: string;
  /** Attribute that specifies the URL of the background image for the LoginPage */
  backgroundImgSrc?: string | BackgroundImageSrcMap;
  /** Attribute that specifies the alt text of the background image for the LoginPage. */
  backgroundImgAlt?: string;
  /** Content rendered inside of the Text Component of the LoginPage */
  textContent?: string;
  /** Items rendered inside of the Footer List Component of the LoginPage */
  footerListItems?: React.ReactNode;
  /** Adds list variant styles for the Footer List component of the LoginPage. The only current value is'inline' */
  footerListVariants?: ListVariant.inline;
  /** Title for the Login Main Body Header of the LoginPage */
  loginTitle: string;
  /** Subtitle for the Login Main Body Header of the LoginPage */
  loginSubtitle?: string;
  /** Content rendered inside of Login Main Footer Band to display a sign up for account message */
  signUpForAccountMessage?: React.ReactNode;
  /** Content rendered inside of Login Main Footer Band to display a forgot credentials link* */
  forgotCredentials?: React.ReactNode;
  /** Content rendered inside of Social Media Login footer section . */
  socialMediaLoginContent?: React.ReactNode;
}";
    // typescriptExpr->String.dropLeft(~count=1425)->Js.log;
    (run(Parser.parseComponentInterface, typescriptExpr) |> get_exn).name
    ->equal("LoginPage", "LoginPage");
  },
  () => {
    let typescriptExpr = "export interface LoginFormProps extends React.HTMLProps<HTMLFormElement> {
  /** Flag to indicate if the first dropdown item should not gain initial focus */
  noAutoFocus?: boolean;
  /** Additional classes added to the Login Main Body's Form */
  className?: string;
  /** Flag indicating the Helper Text is visible * */
  showHelperText?: boolean;
  /** Content displayed in the Helper Text component * */
  helperText?: React.ReactNode;
  /** Icon displayed to the left in the Helper Text */
  helperTextIcon?: React.ReactNode;
  /** Label for the Username Input Field */
  usernameLabel?: string;
  /** Value for the Username */
  usernameValue?: string;
  /** Function that handles the onChange event for the Username */
  onChangeUsername?: (value: string, event: React.FormEvent<HTMLInputElement>) => void;
  /** Flag indicating if the Username is valid */
  isValidUsername?: boolean;
  /** Label for the Password Input Field */
  passwordLabel?: string;
  /** Value for the Password */
  passwordValue?: string;
  /** Function that handles the onChange event for the Password */
  onChangePassword?: (value: string, event: React.FormEvent<HTMLInputElement>) => void;
  /** Flag indicating if the Password is valid */
  isValidPassword?: boolean;
  /** Label for the Log in Button Input */
  loginButtonLabel?: string;
  /** Flag indicating if the Login Button is disabled */
  isLoginButtonDisabled?: boolean;
  /** Function that is called when the Login button is clicked */
  onLoginButtonClick?: (event: React.MouseEvent<HTMLButtonElement, MouseEvent>) => void;
  /** Label for the Remember Me Checkbox that indicates the user should be kept logged in.  If the label is not provided, the checkbox will not show. */
  rememberMeLabel?: string;
  /** Flag indicating if the remember me Checkbox is checked. */
  isRememberMeChecked?: boolean;
  /** Function that handles the onChange event for the Remember Me Checkbox */
  onChangeRememberMe?: (checked: boolean, event: React.FormEvent<HTMLInputElement>) => void;
}";
    // typescriptExpr->String.dropLeft(~count=1425)->Js.log;
    (run(Parser.parseComponentInterface, typescriptExpr) |> get_exn).name
    ->equal("LoginForm", "LoginForm");
  },
  () => {
    let typescriptExpr = "export interface PageHeaderProps extends React.HTMLProps<HTMLDivElement> {
  /** Additional classes added to the page header */
  className?: string;
  /** Component to render the logo/brand, use <Brand /> */
  logo?: React.ReactNode;
  /** Additional props passed to the logo anchor container */
  logoProps?: object;
  /** Component to use to wrap the passed <logo> */
  logoComponent?: React.ReactNode;
  /** Component to render the header tools, use <PageHeaderTools />  */
  headerTools?: React.ReactNode;
  /** Component to render navigation within the header, use <Nav /> */
  topNav?: React.ReactNode;
  /** True to show the nav toggle button (toggles side nav) */
  showNavToggle?: boolean;
  /** True if the side nav is shown  */
  isNavOpen?: boolean;
  /** This prop is no longer managed through PageHeader but in the Page component. */
  isManagedSidebar?: boolean;
  /** Sets the value for role on the <main> element */
  role?: string;
  /** Callback function to handle the side nav toggle button, managed by the Page component if the Page isManagedSidebar prop is set to true */
  onNavToggle?: () => void;
  /** Aria Label for the nav toggle button */
  'aria-label'?: string;
}";
    typescriptExpr->String.dropLeft(~count=152)->Js.log;
    (run(Parser.parseComponentInterface, typescriptExpr) |> get_exn).name
    ->equal("PageHeader", "PageHeader");
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
              comment: Some("Content rendered inside the breadcrumb title."),
            },
            {
              name: "className",
              required: false,
              type_: Raw("string"),
              comment:
                Some("Additional classes added to the breadcrumb item."),
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
   /** a multi
    *  line comment.
    */
   className?: string;
   to: string;
}

export enum ButtonVariant {
  primary = 'primary',
  secondary = 'secondary'
}
";

  let cont = Parser.parseFile(typescriptFile);
  // Js.log(cont.interfaces->List.map(~f=x => x.properties->List.toArray));
  cont->equal(
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
              comment: Some("a multi\n    *  line comment."),
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
      ~children: 'children=?,
      ~className: string=?,
      ~iconPosition: [@bs.string][
         | [@bs.as "left"] `Left
         | [@bs.as "right"] `Right
         ] =?,
      ~onClick: ReactEvent.Mouse.t => unit=?
    ) => React.element = "Button";
}|},
    );
};

let (>>>) = Fun.(>>);

let show =
  Parser.parseFile
  >>> PatternflyBindings.getComponents
  >>> Js.log
  >>> Result.ok;

let button = "~/src/github.com/patternfly/patternfly-react/packages/react-core/src/components/Button/Button.tsx";
button->Python.read_file->Result.andThen(~f=show);

let tests =
  List.append(props, List.append(interface, [enum, file, reason]));

Node.Process.exit(tests->List.any(~f=t => !t()) ? 1 : 0);
