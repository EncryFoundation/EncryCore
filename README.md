# EncryCore

[![Build Status](https://travis-ci.org/EncryFoundation/EncryCore.svg?branch=master)](https://travis-ci.org/EncryFoundation/EncryCore)

Encry is a distributed digital asset platform supporting smart contracts in non turing-complete Prism language (https://github.com/EncryFoundation/PrismLang) and custom asset issuing.

## Related projects

[PrismLang](https://github.com/EncryFoundation/PrismLang) - Contract-oriented scripting language designed specially for EncryCore.

## Installation

#### 1) Running Latest release

At first, you should download latest release from [this page](https://github.com/EncryFoundation/EncryCore/releases), your target in `EncryCore.jar`. Make sure that you have a compatible Java Runtime Environment 1.8 or later (64-bit version, you can find official [guide here](https://docs.oracle.com/javase/8/docs/technotes/guides/install/install_overview.html)). Once download is complete, you should navigate to the folder where `EncryCore.jar` is located and execute the following command:
`java -jar EncryCore.jar`

#### 2) From Docker Container
Make sure you have docker installed for documentation and guide check [this page](https://docs.docker.com/install/). Once docker is installed, run in terminal following commands:

`$ docker pull encryfoundation/encry-core`

`$ docker run -i --log-driver=none -a stdin -a stdout -a stderr -p 9001:9001 encry-core`

#### 3) Compiling from source
To run EncryCore node you need JRE 1.8 or later (64-bit version, you can find official [guide here](https://docs.oracle.com/javase/8/docs/technotes/guides/install/install_overview.html)) and sbt(Simple build tool for scala and java applications, [official guide](https://www.scala-sbt.org/1.0/docs/Setup.html)) to be installed. If you running a Windows machine, please make sure, that you have git client installed.

Configuration file is `src/main/resources/application.conf`.

Linux/MacOS/Windows:

`$ git clone https://github.com/EncryFoundation/EncryCore.git`

`$ cd EncryCore`

`$ git checkout master`

`$ sbt run`

### Using node.
* After successful installation using any of the described methods above you will have node CLI(command line interface) in opened terminal, description of the valid CLI commands can be found below.
* Description of node's api can be found [here](https://github.com/EncryFoundation/EncryCore/blob/release/docs/design/NodeRoutes.md)

## Interacting with CLI
You can interact with your node using command line interface. Note that generic command structure is:
`[GROUP_NAME] [COMMAND] -[ARGUMENT_1]=[VAL_1] -[ARGUMENT_2]=[VAL_2]`

If you just have installed node, to check that everything is fine you can run :
`wallet balance`

Here is list of the other valid commands:
Group name | Command | Argument | Description
--- | ---| --- | --- |
node|          shutdown|       None|          Shutdown the node
node|          stopMining|       None|           Node stops mining
node|          startMining|      None|           Node starts mining
wallet|       pubKeys|          None|           Print available public keys
wallet|        addrs|            None|           Print available addresses
wallet|        createKey|           None|           Add key to storage
wallet|        balance|          None|           Show balance of current wallet
wallet|        transfer|         addr, amount|   Transfer `amount` to `addr`ess
app|           help|             None|           Show all supported commands
## Running tests

`$ sbt test`

## License

All contributions are made under the [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.en.html). See [LICENSE](LICENSE).
