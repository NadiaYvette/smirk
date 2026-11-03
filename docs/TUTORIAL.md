# Smirk Tutorial — Hands-On Guide to Pure Native Resumable PCRE in Haskell

`smirk` (;-P / ;-) provides a pure native Perl-Compatible Regular Expression (PCRE) engine for Haskell, featuring step-bounded ReDoS immunity, pausable and resumable continuations, sequence polymorphism, and first-class algebraic effect handlers without C dependencies or FFI overhead.

---

## 1. Quick Setup & Required Extensions

To use Smirk's core functions and quasiquoters, import the top-level module:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Smirk
```

---

## 2. Basic PCRE Matching & Capture Groups

### Compiling Patterns & Testing Matches
Compile regular expressions dynamically using `compileRegex`:

```haskell
case compileRegex "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" of
  Left err  -> putStrLn ("Syntax error: " ++ show err)
  Right reg -> do
    print (testMatch reg ("user@example.com" :: Text)) -- True
    print (testMatch reg ("not-an-email" :: Text))     -- False
```

### Extracting Slices & Capture Groups
`execMatch` returns a structured `MatchResult` containing full offset metadata and capture groups:

```haskell
let pattern = "(?<protocol>https?)://(?<host>[a-zA-Z0-9.-]+)(?::(?<port>[0-9]+))?"
case compileRegex pattern of
  Right reg -> do
    let input = "Connecting to https://api.haskell.org:8080/v1" :: Text
    case execMatch reg input of
      Nothing  -> putStrLn "No match found"
      Just res -> do
        putStrLn ("Matched slice: " ++ show (matchedSlice res))
        -- "https://api.haskell.org:8080"
        putStrLn ("Match span: " ++ show (matchOffset res, matchLength res))
        -- (14, 28)
        putStrLn ("Named groups: " ++ show (namedGroups res))
        -- fromList [("host","api.haskell.org"),("port","8080"),("protocol","https")]
        putStrLn ("Numbered groups: " ++ show (captureGroups res))
        -- fromList [(1,"https"),(2,"api.haskell.org"),(3,"8080")]
  Left err -> putStrLn (show err)
```

### Finding All Non-Overlapping Matches
`findAllMatches` traverses the target sequence, collecting every match:

```haskell
case compileRegex "[0-9]+" of
  Right reg -> do
    let matches = findAllMatches reg ("Order 1042 has 3 items totaling 99 dollars" :: Text)
    print (map matchedSlice matches)
    -- ["1042", "3", "99"]
  Left _ -> pure ()
```

---

## 3. Compile-Time QuasiQuoters (`[re|...|]` & `[ed|...|]`)

Template Haskell QuasiQuoters detect regex syntax errors at compile time:

### Static Regex Compilation (`[re|...|]`)

```haskell
-- Syntax errors in this pattern cause GHC compilation to fail immediately:
let emailRegex = [re|\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b|]

testMatch emailRegex ("contact@deepmind.com" :: Text) -- True
```

### Stream Editor Search & Replace (`[ed|...|]`)

The `[ed|search///replacement|]` quasiquoter builds a structured `Editor` record:

```haskell
-- Substitute markdown bold syntax with HTML <strong> tags:
let boldEditor = [ed|\*\*([^*]+)\*\*///<strong>${1}</strong>|]

let formatted = applyEditor boldEditor "This is **very important** text."
-- formatted == "This is <strong>very important</strong> text."
```

By default, `applyEditor` replaces all occurrences globally. To restrict replacement to the first occurrence only, append `///1`:

```haskell
let singleEditor = [ed|foo///bar///1|]
applyEditor singleEditor "foo foo foo" -- "bar foo foo"
```

---

## 4. Fuel-Bounded Step Budgets & ReDoS Defense

Standard regex engines risk catastrophic backtracking when evaluating ambiguous grammars against adversarial inputs. Smirk eliminates this vulnerability using explicit step budgets:

```haskell
-- Ambiguous grammar susceptible to exponential backtracking:
let evilRegex = [re|(a+)+$|]
let adversarialInput = "aaaaaaaaaaaaaaaaaaaaaaaaaaaa!" :: Text

-- Enforce a strict ceiling of 5,000 instruction steps:
case execMatchWithFuel evilRegex adversarialInput (StepBudget 5000) of
  Matched res      -> putStrLn ("Matched: " ++ show (matchedSlice res))
  NoMatch          -> putStrLn "Pattern definitely does not match"
  OutOfFuel cont   -> do
    putStrLn "ReDoS averted! VM halted safely after consuming fuel budget."
    putStrLn ("Steps consumed so far: " ++ show (currentSteps cont))
```

---

## 5. Pausable & Resumable Continuations

When execution halts with `OutOfFuel`, Smirk preserves the complete state of the virtual machine inside `MatchContinuation`. The host program can pause, switch tasks, or provide more fuel later:

```haskell
let cont = ... -- from OutOfFuel cont above

-- Resume execution with 2,500 additional steps:
case resumeMatch cont (StepBudget 2500) of
  Matched res     -> putStrLn ("Eventually matched: " ++ show (matchedSlice res))
  NoMatch         -> putStrLn "Determined no match upon resumption"
  OutOfFuel cont' -> putStrLn "Still out of fuel; pausing again..."
```

---

## 6. Sequence Polymorphism across Haskell Containers

Smirk's execution engine does not require converting input sequences to `Text`. The `RegexTarget` typeclass enables direct matching across five standard container formats:

```haskell
import qualified Data.ByteString.Char8 as BS
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

let digits = [re|[0-9]+|]

-- 1. Strict Text
testMatch digits ("code 404" :: Text)

-- 2. Strict ByteString (ASCII / UTF-8 bytes)
testMatch digits (BS.pack "code 404")

-- 3. Standard String linked list ([Char])
testMatch digits "code 404"

-- 4. Finger-tree sequence (Data.Sequence.Seq Char)
testMatch digits (Seq.fromList "code 404")

-- 5. Contiguous Vector (Data.Vector.Vector Char)
testMatch digits (V.fromList "code 404")
```

---

## 7. Algebraic Effect Handlers

### Effectful Integration (`smirk-effectful`)

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Effectful
import Smirk (re)
import Smirk.Effect.Effectful

myProgram :: (RegexEngine :> es) => Eff es (Maybe Text)
myProgram = do
  mMatch <- matchRegex [re|id:([0-9]+)|] "user id:12345 active"
  pure (fmap matchedSlice mMatch)

runPure :: IO ()
runPure = do
  let res = runPureEff (runRegexEnginePure myProgram)
  print res -- Just "id:12345"

runWithFuelBudget :: IO ()
runWithFuelBudget = do
  let res = runPureEff (runRegexEngineWithFuel (StepBudget 1000) myProgram)
  print res
```

### Polysemy Integration (`smirk-polysemy`)

```haskell
{-# LANGUAGE DataKinds #-}

import Polysemy
import Smirk (re)
import Smirk.Effect.Polysemy

polyProgram :: (Member RegexEngine r) => Sem r (Maybe Text)
polyProgram = do
  mMatch <- matchRegex [re|id:([0-9]+)|] "user id:12345 active"
  pure (fmap matchedSlice mMatch)

runPolyPure :: IO ()
runPolyPure = do
  let res = run (runRegexEnginePure polyProgram)
  print res -- Just "id:12345"
```

---

## 8. Native Filesystem Globbing (`[glob|...|]`)

Smirk includes native POSIX and Git-style globbing:

```haskell
import Smirk

-- Filesystem glob: '*' stops at '/', '**' recurses
let fileGlob = [glob|packages/**/src/**/*.hs|]
matchGlob fileGlob "packages/core/src/MyModule.hs" -- True
matchGlob fileGlob "packages/core/test/Main.hs"    -- False

-- Unconstrained wildcard: '*' matches across all characters
case compileWildcard "mcp__plugin__*" of
  Right wc -> print (matchGlob wc "mcp__plugin__git_status") -- True
  Left _   -> pure ()
```

---

## 9. Next Steps

- For architectural rationale and design decisions (D1–D10), read [`docs/notes/SMIRK_DESIGN.md`](notes/SMIRK_DESIGN.md).
- For quickstart recipes and token-saving symbol navigation, read [`docs/NEWBIE_GUIDE.md`](NEWBIE_GUIDE.md).
- For verification commands and maintenance rules, read [`docs/MAINTAINER_GUIDE.md`](MAINTAINER_GUIDE.md).
