## DotNetLightning: The utility to work with the Bitcoin Lightning Network (LN) in .NET

The main API is in `DotNetLightning.Core` project/assembly.

## Installation

The package is compiled and published in nuget:

* [`DotNetLightning.Kiss`](https://www.nuget.org/packages/DotNetLightning.Kiss/)

It does not use native bindings for cryptographic operations.

Currently it is in alpha, so you probably want to install a latest version by specifying it with `--version`.
The version is prefixed with git commit hash and date. Please take a look at the nuget page.

## Features

### Features in `DotNetLightning.*` sub-namespaces.

#### `DotNetLightning.Utils`

Contains a low-level primitives for LN.
Mostly it is for internal usage but some are useful for consumer's point of view.
(e.g. LNMoney to represent milli-satoshis value)

#### `DotNetLightning.Serialization`

Contains items for wire-protocol. FeatureBits, TLV, and P2P messages.

#### `DotNetLightning.Crypto`

Contains modules and types for working with Cryptographic operations.
For example LN-onion network encoding, [aezeed](https://github.com/lightningnetwork/lnd/tree/master/aezeed) for seed
backups.

#### `DotNetLightning.Chain`

Interface to inject I/O (e.g. signing keys and Blockchain-interaction).

#### `DotNetLightning.Transactions`

This is a module for creating LN-specific transactions. Mostly for internal usage.

#### `DotNetLightning.Peer`

Handles handshake and encryption against other peers.

####  `DotNetLightning.Channel`

Handles channel state.
This module is pretty much WIP (not sure when it will be finished, as this is the most complex part in the LN protocol).

#### `DotNetLightning.Payment`

Contains primitives for Payment-related operations. The most important class is `PaymentRequest`,
a.k.a. bolt11-invoice, LN-invoice.

It also contains primitives for [LSAT](https://github.com/lightninglabs/LSATI), the LN based http authentication mechanism.
See [here](https://github.com/joemphilips/LSATAuthentication) for PoC of AspNetCore middleware for LSAT.

#### `DotNetLightning.Routing`

Module for calculating payment routes. (Very WIP.)

### Other features

Some sibling assemblies come together when you install `DotNetLightning` or `DotNetLightning.Core`. These are mostly for internal usages but some might be useful for you:

#### `AEZ`

Which contains managed code for aez cipher scheme.

It may be useful if you want to secure your data before saving it to disk.
See official page for more details: https://www.cs.ucdavis.edu/~rogaway/aez/index.html

#### `Macaroon`

Which contains macaroon authentication token.

The API is mostly the same as [libmacaroon](https://github.com/rescrv/libmacaroons) (see libmacaroon's readme for the
usage).

(Currently it is only supported in BouncyCastle build (which means not in `DotNetLightning.Core`),
see https://github.com/joemphilips/DotNetLightning/issues/153 for more info.)

