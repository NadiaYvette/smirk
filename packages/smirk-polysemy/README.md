# smirk-polysemy

**Polysemy interface for the Smirk PCRE engine.**

`smirk-polysemy` provides first-class `polysemy` bindings for Smirk's pure, step-bounded PCRE regular expression engine.

---

## Features

- **Polysemy Effect (`SmirkRegex`):** Idiomatic effect definition integrating seamlessly into `Sem r` effect rows.
- **Dual Interpreters:**
  - `runSmirkPure`: Unbounded pure in-memory execution.
  - `runSmirkFuel`: Fuel-bounded execution guaranteeing ReDoS immunity by returning `Left OutOfFuel` if a step budget is exceeded.
- **Lightweight Dependency:** Only depends on `smirk` and `polysemy`, without dragging in `effectful`.

---

## Quick Example

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Polysemy
import Smirk (re)
import Smirk.Effect.Polysemy

prog :: (Member SmirkRegex r) => Sem r (Maybe (Match Text))
prog = match [re|hello (\w+)|] "hello world"

main :: IO ()
main = do
  let result = run (runSmirkPure prog)
  print result
```

---

## License

Licensed under the Apache License 2.0 (`LICENSE`).
