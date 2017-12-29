# dbgen

This is a simple program to generate valid `wallet-db` databases, starting from a specification
stored in a `Dhall` config file.

In order for the program to work correctly, it must be provided with a non-empty RocksDB generated
with the same configuration this program is invoked with, or the DB-content deserialisation will
fail with the "wrong magic" error.

## Pitfalls

- Not passing an external RocksDB path would result in a segmentation fault (due to invalid FFI code
  usage in the `rocksdb-haskell` library)

- Faking `StakeLock` sync to avoid the relevant `MVar` from deadlocking.
