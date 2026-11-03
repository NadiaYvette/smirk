# smirk-effectful

**Effectful (algebraic effects) interface for the Smirk PCRE engine.**

`smirk-effectful` provides first-class `effectful` bindings for Smirk's pure, step-bounded PCRE regular expression engine.

---

## Features

- **First-Class Effect (`SmirkRegex`):** Idiomatic effect definition integrating seamlessly into `Eff es` stacks.
- **Dual Interpreters:**
  - `runSmirkPure`: Unbounded pure in-memory execution.
  - `runSmirkFuel`: Fuel-bounded execution guaranteeing ReDoS immunity by returning `Left OutOfFuel` if a step budget is exceeded.
- **Lightweight Dependency:** Only depends on `smirk` and `effectful`, without dragging in `polysemy`.

---

## Quick Example

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Effectful
import Smirk (re)
import Smirk.Effect.Effectful

prog :: (SmirkRegex :> es) => Eff es (Maybe (Match Text))
prog = match [re|hello (\w+)|] "hello world"

main :: IO ()
main = do
  let result = runPureEff (runSmirkPure prog)
  print result
```

---

## License

Licensed under the Apache License 2.0 (`LICENSE`).
