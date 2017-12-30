# dbgen

This is a simple program to generate valid `wallet-db` databases, starting from a specification
stored in a `Dhall` config file.

In order for the program to work correctly, it must be provided with a non-empty RocksDB generated
with the same configuration this program is invoked with, or the DB-content deserialisation will
fail with the "wrong magic" error.

## Usage

First compile `dbgen` against the same revision of Cardano you want it to generate the `wallet-db` for.
At the moment the program has been tested with `release/1.0.4`
(SHA: `6e53bf599097aa0b55738a454115f76f69c9489e`). Then edit the `config.dhall` file to specify the number
of wallets, accounts and addresses to generate. For example, the following config will generate 10 wallets,
each wallet with 1 account and 100 addresses underneath:

```
{ wallet_spec = { account_spec = { addresses = 100 }, accounts = 1 }
, wallets     = 10
}
```

Once you are ready, you can invoke `dbgen`:

```
Usage: dbgen [--config CONFIG.DHALL] [--dbPath rocksdb-path]
```

If all is well, the program will generate a new `wallet-db` filled with synthetic but valid data,
together with stats about how much time it took to generate those:

```
dbgen --dbPath ../cardano-sl/run/node-db0
Faking StateLock syncing...
Generating 1 wallets...
Action took 0.13194 seconds.
Generating 1 accounts for Wallet 1...
Action took 0.083039 seconds.
Generating 10 addresses for Account CAccountId "Ae2tdPwUPEZGwUVR8meJ6mgbHeiC16TNyM3wTDYRYwDBMeCHUXdMVLHKupC@793406314"...
Action took 0.739177 seconds.
OK.
```

## Pitfalls

- Not passing an external RocksDB path would result in a segmentation fault (due to invalid FFI code
  usage in the `rocksdb-haskell` library)

- Faking `StakeLock` sync to avoid the relevant `MVar` from deadlocking.
