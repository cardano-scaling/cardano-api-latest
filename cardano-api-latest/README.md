# cardano-api-latest

Additional `ViewPatterns` and functions for [cardano-api](https://github.com/IntersectMBO/cardano-api) that assume the latest `Era`
and `PlutusScript` (Currently `Conway` and `PlutusScriptV3`).

This library replaces types from `Cardano.Api` that take an `era` type parameter or a plutus `script` type parameter, and
functions that require witnesses of those. It does not re-export other things from `Cardano.Api`.
