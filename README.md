# boxctl
CLI controller for sing-box

```shell
boxctl [OPTION] <COMMAND>
```

- **options**:
  - `--instance <<PATH>|<ADDR[:<PORT>]>>`   Target instance, defaults to `127.0.0.1:9090`
  - `--verbose`                             Verbose output
  - `--json`                                Emit JSON output
  - `--help`                                Show this help text

- **commands**:
  - `version`                               Show version
  - `mode`                                  Show current Clash mode
  - `switch <CLASH_MODE>`                   Switch Clash mode
  - `list`                                  List groups
  - `show [OUTBOUND1] [OUTBOUND2] [...]`    Show details of outbounds
  - `test [OUTBOUND1] [OUTBOUND2] [...]`    Test delays of outbounds
  - `select [SELECTOR] <OPTION>`            Select an option for a selector
