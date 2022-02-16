# re-cli

Hello! This project allows you to quickly get started with Reason and BuckleScript.

# Setup environment

Install runtime and package manager:

```bash
sudo dnf install -y nodejs
```

Then install dependencies:

```bash
npm install
```

# Build + Watch

```bash
npm start
```

# Run tool

```bash
node src/Hello.bs.js
```

# Render patternfly bindings

- Clone ~/src/github.com/patternfly/patternfly-react and src/softwarefactory-project.io/software-factory/re-patternfly
- Run: `npm run build && node src/GeneratePatternflyComponentBindings.bs.js`

# Read documentation:

ReScript (Reason) documentation:
* https://rescript-lang.org/docs/manual/v8.0.0/introduction

Standard library:
* https://tableclothml.netlify.app

Javascript binding:
* https://rescript-lang.org/docs/manual/latest/api/js
