# EncryCore

[![Build Status](https://travis-ci.org/EncryFoundation/EncryCore.svg?branch=master)](https://travis-ci.org/EncryFoundation/EncryCore)

Encry is a distributed digital asset platform supporting smart contracts in non turing-complete Prism language (https://github.com/EncryFoundation/PrismLang) and custom asset issuing.

## Related projects

[PrismLang](https://github.com/EncryFoundation/PrismLang) - Contract-oriented scripting language designed specially for EncryCore.

## Installation

#### 1) Running Latest release

At first, you should download latest release from [this page](https://github.com/EncryFoundation/EncryCore/releases), your target in `EncryCore.jar`. Make sure that you have a compatible Java Runtime Environment 1.8 or later (64-bit version, you can find official [guide here](https://docs.oracle.com/javase/8/docs/technotes/guides/install/install_overview.html)). Once download is complete, you should navigate to the folder where `EncryCore.jar` is located and execute the following command:

`$ java -jar EncryCore.jar`

To run this app with additional configurations you should execute the following command:

`$ java -jar EncryCore.jar file_with_your_configurations.conf`

List of default configs you can find in:

`EncryCore/srs/main/resources/configs/`

#### 2) Compiling from source
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

`wallet createKey`

Here is list of the other valid commands:

    | Group name |   Command              |   Argument      |  Meaning
    |------------|------------------------|-----------------|--------------------------------
    | node       |   shutdown             |   None          |  Shutdown the node
    | node       |   stopMining           |   None          |  Node stops mining
    | node       |   startMining          |   None          |  Node starts mining
    | wallet     |   pubKeys              |   None          |  Print available public keys
    | wallet     |   privKeys             |   None          |  Print available private keys
    | wallet     |   addrs                |   None          |  Print available addresses
    | wallet     |   createKey            |   None          |  Add key to storage
    | wallet     |   balance              |   None          |  Show balance of current wallet
    | wallet     |   transfer             |   addr, amount  |  Transfer `amount` to `addr`ess
    | wallet     |   createToken          |   fee, amount   |  Creates new token
    | wallet     |   createKey            |   None          |  Creates new account
    | wallet     |   fromSeed             |   Seed          |  Creates new account from seed
    | peer       |   removeFromBlackList  |   host, port    |  Remove peer from black list
    | peer       |   addPeer              |   host, port    |  Add peer to 'knownPeers'
    | peer       |   all                  |   None          |  Get all peers in the system
    | peer       |   connected            |   None          |  Get all connected peers
    | peer       |   banned               |   None          |  Get all banned peers
    | peer       |   ban                  |   host, port    |  Ban peer
    | app        |   info                 |   None          |  Show info about your node
    | app        |   help                 |   None          |  Show all supported commands
    | history    |   getTxById            |   Id            |  Get transaction by 'Id'
    | history    |   getLastHeaders       |   Number        |  Get last 'Number' headers
    | history    |   getLastHeaderIds     |   Number        |  Get header at height 'Number'
    | history    |   getHeaderById        |   Id            |  Get header by 'Id'
    | history    |   getFullBlock         |   Id            |  Get block by 'Id'
    | history    |   getTxById            |   None          |  Get block candidate
    | app        |   info                 |   None          |  Show info about your node
    | app        |   help                 |   None          |  Show all supported commands



## Running tests

`$ sbt test`

## License

All contributions are made under the [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.en.html). See [LICENSE](LICENSE).

## Acknowledgements

<img src="https://www.yourkit.com/images/yklogo.png" align="right" />

YourKit supports open source projects with its full-featured Java Profiler.
YourKit, LLC is the creator of <a href="https://www.yourkit.com/java/profiler/">YourKit Java Profiler</a>
and <a href="https://www.yourkit.com/.net/profiler/">YourKit .NET Profiler</a>,
innovative and intelligent tools for profiling Java and .NET applications.
