{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module BatchSpec (spec) where

import Control.Monad (forM_)
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import Data.Text (Text)
import Test.Hspec

import Smirk
import Smirk.Continuation.Batch

spec :: Spec
spec = describe "Smirk.Continuation.Batch (Memory-Bounded Match Batching)" $ do
  let mReg = compileRegex "(?<key>[a-z]+)=(?<val>\\d+)"
      target = ("config: timeout=500ms max=2000ms retry=3s port=8080" :: Text)

  it "batches all matches into memory-bounded SmallArray pages" $ do
    case mReg of
      Right reg -> do
        let cfg = defaultBatchConfig
              { bcMaxBytesPerPage = 25
              , bcMaxMatchesPerPage = 2
              }
            batches = findAllMatchesBatched cfg reg target
        length batches `shouldBe` 2
        let b1 = head batches
        sizeofSmallArray b1.batchItems `shouldBe` 2
        (indexSmallArray b1.batchItems 0).matchedSlice `shouldBe` "timeout=500"
        (indexSmallArray b1.batchItems 1).matchedSlice `shouldBe` "max=2000"

        let b2 = batches !! 1
        sizeofSmallArray b2.batchItems `shouldBe` 2
        (indexSmallArray b2.batchItems 0).matchedSlice `shouldBe` "retry=3"
        (indexSmallArray b2.batchItems 1).matchedSlice `shouldBe` "port=8080"
      Left err -> expectationFailure (show err)

  it "enforces strict byte footprint boundary per batch" $ do
    case mReg of
      Right reg -> do
        let cfg = defaultBatchConfig
              { bcMaxBytesPerPage = 15
              , bcMaxMatchesPerPage = 100
              }
            batches = findAllMatchesBatched cfg reg target
        forM_ batches $ \b -> do
          (b.batchByteFootprint <= 15) `shouldBe` True
      Left err -> expectationFailure (show err)
