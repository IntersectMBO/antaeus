<div align="center">
  <img src="antaeus.jpg" style="border-radius: 20px;" alt="antaeus" width="25%"/>
  <h2>Antaeus</h2>
</div>


Antaeus is an end-to-end testing framework designed for Cardano Haskell APIs. It leverages [cardano-testnet](https://github.com/input-output-hk/cardano-node/tree/master/cardano-testnet) to configure and launch a local Cardano testnet, and [cardano-api](https://github.com/input-output-hk/cardano-node/tree/master/cardano-api) to build transactions and query the ledger state. The primary focus of these tests is on functionality involving Plutus scripts.


Tests can be executed on either private or public testnets. They are grouped by target protocol version (e.g. Babbage PV8) and share each instance of testnet during execution.

#### How to run on private testnet

By default, tests are configured to run in a local environment for their supported protocol version.
1. Optionally, enter `nix develop` shell for dependencies.
2. Run `cabal build all`
3. Run `cabal test e2e-tests`


#### How to run on public testnet

Check these preconditions before following the private testnet steps above.
1. Create a directory containing at least the following two subdirectories:
   - **utxo-keys**, which includes two files: "test.skey" and "test.vkey" in text envelope PaymentKey format (no support for PaymentExtendedKey at the moment).
   - **ipc**, which contains the active node socket "node.socket"
2. Ensure your Cardano node is fully synced on a public network (e.g. preview testnet).
3. Have at least one ADA-only UTxO in the test account. Each test will spend a few ADA, so ensure sufficient funds are available.
4. Edit [Testnet.hs](e2e-tests/test/Helpers/Testnet.hs) so that `LocalNodeOption`'s `localEnvDir` points to the directory containing your keys ("utxo-keys") and node socket ("ipc").
5. Edit [Spec.hs](e2e-tests/test/Spec.hs) to include `localNodeTests` in the `TestTree`. You can configure to run with or without the existing private testnet tests (e.g. `pv8Tests`).

---

### Status

Antaeus is currently under development, with a growing suite of tests covering:

- Plutus built-in functions `verifySchnorrSecp256k1Signature` and `verifyEcdsaSecp256k1Signature` across different protocol versions and expected outcomes (success or specific errors).
- Spending locked funds using reference scripts, reference inputs, and providing datum as witness in the transaction body.
- Minting tokens using reference scripts and providing script witness in the transaction body.
- Examining each field in Plutus V1 and V2 `TxInfo`.

---

### Planned Features

We're working on adding the following features to Antaeus:

- CI execution with private testnet on commit (nix configuration coming soon).
- Test reporting with [Allure](https://github.com/allure-framework).
- Nightly CI test execution would be useful in public environments.

---

### Licensing

You are free to copy, modify, and distribute Antaeus under the terms of the Apache 2.0 license. See the [LICENSE](./LICENSE) and [NOTICE](./NOTICE) files for details.