# Contributing to Antaeus

Thank you for your interest in contributing to the Antaeus project! In this guide you will get an overview of the contribution workflow. Please read these contribution guidelines before starting your work.

## Getting Started
### Issues
1. Search Existing Issues: Whether you plan to extend test coverage, highlight a problem with the test framework, or wish to request an improvement, a new issue should be created for tracking. Search the [existing issues](https://github.com/input-output-hk/antaeus/issues) to see if your issue has already been reported. If you find a similar issue, add any relevant information to help clarify it.
2. Open New Issues: If a related issue doesn't exist, you can open a [new issue](https://github.com/input-output-hk/antaeus/issues/new).

### Pull Requests
1. Clone the [repository](https://github.com/input-output-hk/antaeus) and create a new branch from `main`.
2. Write your code, ensuring that it follows the [styleguide](https://github.com/input-output-hk/antaeus/blob/main/STYLEGUIDE.adoc) and includes appropriate tests.
3. Write a descriptive commit message with an informative title followed by a brief explanation of the changes made.
4. Push your changes to your branch on GitHub.
5. Open a new pull request against the `main` branch of the main repository, providing detailed information about the changes and any additional context that may be helpful. Don't forget to [link PR to issue](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue) if you are solving one.

### Test environments
Antaeus can execute tests on any combination of network environments in a single test run: local private testnets, emulators, or public testnets. Each environment uses a constant protocol version and suitible tests are pre-defined in [Spec.hs](https://github.com/input-output-hk/antaeus/blob/main/e2e-tests/test/Spec.hs). Tests are designed to run against any type of environment but some are incompatible certain protocol versions. All testnet environments and configurations are defined in [Testnet.hs](https://github.com/input-output-hk/antaeus/blob/main/e2e-tests/test/Helpers/Testnet.hs).

### Adding new tests
1. Where to add tests: All tests are defined in [Spec/](https://github.com/input-output-hk/antaeus/tree/main/e2e-tests/test/Spec) grouped by module relating to the functionality under test. [Spec.hs](https://github.com/input-output-hk/antaeus/blob/main/e2e-tests/test/Spec.hs defines the [TestTree](https://hackage.haskell.org/package/tasty-1.5/docs/Test-Tasty.html#t:TestTree) to be used for running each of the tests
2. Test Architecture: Each [Property](https://hackage.haskell.org/package/hedgehog-1.4/docs/Hedgehog.html#t:Property) shares a single instance of a test environment meaning that [Tasty](https://hackage.haskell.org/package/tasty) is not aware of the multiple test cases being run. As a result, logging and test report generation is handled by Antaeus in [Test.hs](https://github.com/input-output-hk/antaeus/blob/main/e2e-tests/test/Helpers/Test.hs) and [TestResults.hs](https://github.com/input-output-hk/antaeus/blob/main/e2e-tests/test/Helpers/TestResults.hs) respectively. This design choice was made so that each test does not require its own environment, significantly reducing the total excution duration.

#### Ledger functionality
Testing specific ledger functionality require tests than build specific transactions. These tests often perform queries of the ledger state to determine current epoch and protocol paramters. The transaction body must be correctly constructed to satisfy the ledger to effectivly test each feature. For example, `constitutionProposalAndVoteTestInfo` in [ConwayFeatures.hs](https://github.com/input-output-hk/antaeus/blob/main/e2e-tests/test/Spec/ConwayFeatures.hs) builds and submits a transaction that includes a constitutional proposal as well as inputs and output before proceeding to vote and assert for expected outcome.

#### Plutus builtin
Tests for internal Plutus functionality can follow a standard template of: building a transaction to spend a single UTxO and mint an asset with a policy that uses the builtin under tests, then assert the expected output exists on-chain with the newly minted asset. One example test is `verifyBlsFunctionsTest` in [BLS.hs](https://github.com/input-output-hk/antaeus/blob/main/e2e-tests/test/Spec/Builtins/BLS.hs) that runs multiple scripts to check all functions relating to BLS and asserts that all tokens were successfully minted. When adding new tests that check builtins this template should be resused to reduce complexity.

## Code Style and Formatting
To ensure consistency in our codebase, please adhere to the [styleguide](https://github.com/input-output-hk/antaeus/blob/main/STYLEGUIDE.adoc).

## Testing and Code Review
1. Run tests: Before submitting a pull request, ensure that all tests pass.
2. Code Review: Be prepared to engage in code reviews and make necessary adjustments based on feedback from maintainers.
3. Handling Test Failures: Investigate failing tests immediately. Create issues for problems found in Antaeus or external components in relevant GitHub repository, and either fix the test or temporarily disable it with an explanatory comment.
