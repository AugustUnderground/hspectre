# HSpectre

Haskell Module for communicating with Cadence Spectre.

[Documentation](https://augustunderground.github.io/hspectre)

## Build

```sh
$ stack build
```

## Examples

The `./example` directory contains demos on how to access the shared library
from other languages. These only works _after_ the libray has been built, as
they depend on the `./lib` direcotry.

## Interactive

Interactive mode commands

| Command                   | Implemented | Functionality                       |
|---------------------------|-------------|-------------------------------------|
| `sclGetParameter`         | [ ]         |                                     |
| `sclListParameter`        | [ ]         |                                     |
| `sclGetAttribute`         | [X]         | Get Netlist Parameter Value         |
| `sclSetAttribute`         | [ ]         |                                     |
| `sclListAttribute`        | [ ]         |                                     |
| `sclGetAnalysis`          | [ ]         |                                     |
| `sclGetCircuit`           | [ ]         |                                     |
| `sclGetInstance`          | [ ]         |                                     |
| `sclGetModel`             | [ ]         |                                     |
| `sclGetPrimitive`         | [ ]         |                                     |
| `sclListAnalysis`         | [X]         | Get list of all analyses in netlist |
| `sclListCircuit`          | [ ]         |                                     |
| `sclListInstance`         | [ ]         |                                     |
| `sclListModel`            | [ ]         |                                     |
| `sclListNet`              | [ ]         |                                     |
| `sclListPrimitive`        | [ ]         |                                     |
| `sclCreateAnalysis`       | [ ]         |                                     |
| `sclReleaseObject`        | [ ]         |                                     |
| `sclRun`                  | [X]         | Run _all_ analyses                  |
| `sclRunAnalysis`          | [X]         | Run given analysis                  |
| `sclGetError`             | [ ]         |                                     |
| `sclGetResultDir`         | [ ]         |                                     |
| `sclSetResultDir`         | [ ]         |                                     |
| `sclGetPid`               | [ ]         |                                     |
| `sclHelp`                 | [ ]         |                                     |
| `sclQuit`                 | [X]         | Quit current session                |
| `mdlRegMeasurement`       | [ ]         |                                     |
| `mdlListAliasMeasurement` | [ ]         |                                     |
| `mdlRun`                  | [ ]         |                                     |
| `mdlDelMeasurement`       | [ ]         |                                     |
