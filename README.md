# HSpectre

Haskell Module for communicating with Cadence Spectre.

[Documentation](https://augustunderground.github.io/hspectre)

## Build

```sh
$ stack build
```

## Examples

```haskell
import Spectre.Interactive

main :: IO ()
main = do
    session <- initSession ["path/to/pdk/lib"] "path/to/netlist.scs"
    analyses <- listAnalysis session
    print analyses
    results <- runAll session
    print results
    exitSession session
```

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
