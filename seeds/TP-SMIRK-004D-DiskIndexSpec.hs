{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module DiskIndexSpec (spec) where

import Data.List (sort)
import Data.Text (Text)
import Test.Hspec

import Smirk

spec :: Spec
spec = describe "Smirk.Continuation.DiskIndex (High-Throughput Posting Indexer)" $ do
  let mReg = compileRegex "(?<key>[a-z]+)=(?<val>\\d+)"
      target = ("config: timeout=500ms max=2000ms retry=3s port=8080" :: Text)

  it "projects matches into compact 20-byte MatchIndexEntry descriptors" $ do
    sizeofMatchIndexEntry `shouldBe` 20

  it "generates strictly ordered IndexRun batches" $ do
    case mReg of
      Right reg -> do
        let cfg = defaultDiskIndexConfig { dicMaxEntries = 2 }
            runs = generateSortedIndexRun cfg 101 1 reg target
        length runs `shouldBe` 2
        let r1 = head runs
        r1.irTotalEntries `shouldBe` 2
        r1.irByteSize `shouldBe` 40
        let e1 = head r1.irEntries
            e2 = r1.irEntries !! 1
        e1.mieDocId `shouldBe` 101
        e1.miePatternId `shouldBe` 1
        (e1.mieUtf8ByteOffset < e2.mieUtf8ByteOffset) `shouldBe` True

        let r2 = runs !! 1
        r2.irTotalEntries `shouldBe` 2
      Left err -> expectationFailure (show err)

  it "verifies generated entries match sorted order" $ do
    case mReg of
      Right reg -> do
        let cfg = defaultDiskIndexConfig { dicMaxEntries = 100 }
            runs = generateSortedIndexRun cfg 42 7 reg target
            entries = concatMap irEntries runs
        entries `shouldBe` sort entries
      Left err -> expectationFailure (show err)
