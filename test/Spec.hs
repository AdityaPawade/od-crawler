{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Test.Hspec
import qualified Data.Text as T

main :: IO()
main = hspec $
  describe "createLink" $ do
    it "regular link" $ do
      let display  = T.pack "linkDisplayName"
      let url      = T.pack "https://root.com/a/b/c/"
      let expected = Link display (T.pack "https://root.com/a/b/c/linkDisplayName")
      createLink url display `shouldBe` expected

    it "relative parent link" $ do
      let display  = T.pack "../"
      let url      = T.pack "https://root.com/a/b/c/"
      let expected = Link "b" (T.pack "https://root.com/a/b/")
      createLink url display `shouldBe` expected

    it "absolute parent link" $ do
      let display  = T.pack "/a/b"
      let url      = T.pack "https://root.com/a/b/c/"
      let expected = Link "b" (T.pack "https://root.com/a/b/")
      createLink url display `shouldBe` expected

    describe "shouldFollow" $ do
      it "regular deeper link" $ do
        let display    = T.pack "d/"
        let url        = T.pack "https://root.com/a/b/c/d/"
        let currentUrl = T.pack "https://root.com/a/b/c/"
        let link = Link display url
        shouldFollow link currentUrl `shouldBe` True

      it "block ascending link" $ do
        let display    = T.pack "b/"
        let url        = T.pack "https://root.com/a/b"
        let currentUrl = T.pack "https://root.com/a/b/c/"
        let link = Link display url
        shouldFollow link currentUrl `shouldBe` False

      it "block identical link" $ do
        let display    = T.pack "c/"
        let url        = T.pack "https://root.com/a/b/c/"
        let currentUrl = T.pack "https://root.com/a/b/c/"
        let link = Link display url
        shouldFollow link currentUrl `shouldBe` False

    describe "createResource" $ do
      it "create Folder" $ do
        let display    = T.pack "c/"
        let url        = T.pack "https://root.com/a/b/c/"
        let link = Link display url
        createResource link `shouldBe` Folder link

      it "create File" $ do
        let display    = T.pack "c"
        let url        = T.pack "https://root.com/a/b/c"
        let link = Link display url
        createResource link `shouldBe` File link
