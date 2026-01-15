# Spec Test Client

This folder contains the code for the `purescript-marlowe` spec test client. It is a command line program that receives requests via `stdin` and produces responses in `stdout`, and it is called by the [Spec Test Driver](https://github.com/input-output-hk/marlowe/tree/master/marlowe-spec-test) to ensure that `purescript-marlowe` conforms to the [Marlowe Specification](https://github.com/input-output-hk/marlowe/releases/download/SCP-4415/specification-v3-rc1.pdf).

The test is automatically executed on the CI, but you can trigger a manual test by calling

```bash
[nix-shell] $ marlowe-test
```

To update the version of the spec test driver we need to update the input flake

```bash
$ nix flake lock --update-input marloweSpec
```