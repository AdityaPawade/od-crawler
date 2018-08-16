{-# LANGUAGE OverloadedStrings #-}

module StorageSpec (spec) where

import Storage
import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "fileNameForURL" $ do
    it "root URL" $ do
      fileNameForURL "https://www.w3schools.com" `shouldBe` "www.w3schools.com.txt"

    it "deep URL" $ do
     fileNameForURL "https://www.w3schools.com/a/b/c" `shouldBe` "www.w3schools.com-a-b-c.txt"
