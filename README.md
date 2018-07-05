# EncryCore

[![Build Status](https://travis-ci.org/EncryFoundation/EncryCore.svg?branch=master)](https://travis-ci.org/EncryFoundation/EncryCore)

Reference implementation of the EncryCore node. Encry is a distributed digital asset platform supporting smart contracts (non turing-complete)
and custom asset issuing.

* [Protocol specification](https://github.com/EncryFoundation/EncryCore/blob/master/docs/design/ProtocolSpec.md) (in progress)

## Related projects

* [EncryScript](https://github.com/EncryFoundation/EncryScript) - Contract-oriented scripting language designed specially for EncryCore.
* [EncryTypeLang](https://github.com/EncryFoundation/EncryTypeLang) - Language for arbitrary data structures description (At early development stage, currently).

## Installation

#### Compiling from source
To run EncryCore node you need JRE 1.8 (64-bit version) and SBT to be installed. 
Default configuration file and templates could be found in `src/main/resources/`.

MacOS and Linux:

`$ git clone https://github.com/EncryFoundation/EncryCore.git`

`$ cd EncryCore`

`$ chmod +x startup.sh`

`$ ./startup.sh <optional_path_to_your_application.conf>`
    
## Running tests

`$ sbt test`

## License

All contributions are made under the [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.en.html). See [LICENSE](LICENSE).