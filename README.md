# boxctl
![](./banner.png)

## Usage

```shell
boxctl [OPTION] <COMMAND>
```

### Options

  - `--instance <<PATH>|<ADDR[:<PORT>]>>`   Target instance, guessed as config path or controller address
  - `--instance-config <PATH>`              Target instance config file
  - `--instance-address <ADDR[:<PORT>]>`    Target controller address
  - `--instance-workdir <PATH>`             sing-box working directory
  - `--verbose`                             Verbose output
  - `--json`                                Emit JSON output
  - `--color <auto|always|never>`           Colorise human output, defaults to `auto`
  - `--help`                                Show this help text

### Commands
  - `version`                               Show version
  - `mode`                                  Show current Clash mode
  - `switch <CLASH_MODE>`                   Switch Clash mode
  - `list`                                  List groups
  - `show [OUTBOUND1] [OUTBOUND2] [...]`    Show details of outbounds
  - `test [OUTBOUND1] [OUTBOUND2] [...]`    Test delays of outbounds
  - `select <OPTION>`                       Select an option when only one selector exists
  - `select <SELECTOR> <OPTION>`            Select an option for a specific selector
  - `ssm ...`                               Manage Shadowsocks users via SSM API
  - `ts ...`                                Inspect Tailscale state (from cache, not realtime)

#### SSM

```shell
boxctl ssm [--tag NAME] [--endpoint PATH] <COMMAND>
```

  - `list`                                  List users
  - `show <USER1> [USER2] [...]`            Show detailed users' information
  - `add <USER>`                            Add a user
  - `remove <USER1> [USER2] [...]`          Remove users
  - `update <USER>`                         Update a user's password
  - `stat`                                  Show traffic statistics

#### Tailscale

```shell
boxctl ts [--tag NAME] <COMMAND>
```

  - `status`                                Show cached status for configured Tailscale endpoints
  - `ip [PEER]`                             Show cached Tailscale IP addresses

### Env Variables
  - `BOXCTL_INSTANCE`                       Default target
  - `BOXCTL_SECRET`                         Default secret
  - `BOXCTL_BACKGROUND=<dark|light>`        Override auto background detection

## Develop

It uses [mise](https://mise.jdx.dev/) to manage toolchain:

```shell
mise install
cabal build

cabal run boxctl # Use -- to pass params
```

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
