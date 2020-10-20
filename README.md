# re-cli

Hello! This project allows you to quickly get started with Reason and BuckleScript.

# Setup environment

With nix:

```bash
nix-shell --pure
```

With fedora:

```bash
sudo dnf install -y nodejs yarnpkg
yarn install
```

# Build + Watch

```bash
npx bsb -make-world -w
```

# Run tool

```bash
node src/Hello.bs.js
```
