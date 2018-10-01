# EncryCore

[![Build Status](https://travis-ci.org/EncryFoundation/EncryCore.svg?branch=master)](https://travis-ci.org/EncryFoundation/EncryCore)

Encry is a distributed digital asset platform supporting smart contracts in non turing-complete Prism language (https://github.com/EncryFoundation/PrismLang) and custom asset issuing.

## Related projects

* Prism language (https://github.com/EncryFoundation/PrismLang) - Contract-oriented scripting language designed specially for EncryCore.

## Installation

#### Compiling from source
To run EncryCore node you need JRE 1.8+ (64-bit version) and sbt(Scala build tool) to be installed. 
Configuration file is `src/main/resources/application.conf`.

Linux/MacOS/Windows:

`$ git clone https://github.com/EncryFoundation/EncryCore.git`

`$ cd EncryCore`

`$ git checkout master`

`$ sbt run`

Description of node's api can be found here: https://github.com/EncryFoundation/EncryCore/blob/release/docs/design/NodeRoutes.md
    
## Running tests

`$ sbt test`

## License

All contributions are made under the [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.en.html). See [LICENSE](LICENSE).
