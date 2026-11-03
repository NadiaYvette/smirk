# smirk-penelope

`smirk-penelope` exposes Smirk's pure, step-bounded PCRE engine as an algebraic
effect for the [`penelope`](https://github.com/NadiaYvette/penelope) effect system.

## Features

- **Zero-Cost Polycategorical Dispatch:** Dispatches regex matching over flat Polytropos row vectors (`SmallArray#`).
- **ReDoS Protection:** Bound execution via `matchRegexFuel` or `runRegexEngineWithFuel`.
- **First-Class Effect:** Embeds into Penelope's `Eff eh ef a` unary monad via `sendEff @"regex"`.

## Quick Start

```haskell
import Data.Text (Text)
import Penelope.Effects (Eff)
import Smirk (re)
import Smirk.Effect.Penelope

searchUser :: (ConstructorIndex "regex" ef) => Text -> Eff eh ef (Maybe Text)
searchUser input = do
  mMatch <- matchRegex [re|user:(\w+)|] input
  pure (fmap matchedSlice mMatch)
```
