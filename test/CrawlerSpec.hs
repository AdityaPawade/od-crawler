{-# LANGUAGE QuasiQuotes #-}

module CrawlerSpec (spec) where

import Crawler
import Types
import Test.Hspec
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "createLink" $ do
    it "regular link" $ do
      let display  = T.pack "linkDisplayName"
      let url      = T.pack "https://root.com/a/b/c/"
      let expected = Link display (T.pack "https://root.com/a/b/c/linkDisplayName")
      createLink url display `shouldBe` expected

    it "regular link (without trailing /)" $ do
      let display  = T.pack "linkDisplayName"
      let url      = T.pack "https://root.com/a/b/c"
      let expected = Link display (T.pack "https://root.com/a/b/c/linkDisplayName")
      createLink url display `shouldBe` expected

    it "absolute link to child (without trailing /)" $ do
      let display  = T.pack "/a/b/c/d"
      let url      = T.pack "https://root.com/a/b/c"
      let expected = Link "d" (T.pack "https://root.com/a/b/c/d")
      createLink url display `shouldBe` expected

    it "absolute link to child (with trailing /)" $ do
      let display  = T.pack "/a/b/c/d/"
      let url      = T.pack "https://root.com/a/b/c"
      let expected = Link "d" (T.pack "https://root.com/a/b/c/d/")
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

    it "full parent link" $ do
      let display  = T.pack "https://root.com/a/b/"
      let url      = T.pack "https://root.com/a/b/c/"
      let expected = Link "https://root.com/a/b/" (T.pack "https://root.com/a/b/")
      createLink url display `shouldBe` expected

  describe "shouldFollow" $ do
    it "regular deeper link" $ do
      let display    = T.pack "d/"
      let url        = T.pack "https://root.com/a/b/c/d/"
      let currentUrl = T.pack "https://root.com/a/b/c/"
      let lk = Link display url
      shouldFollow lk currentUrl `shouldBe` True

    it "block ascending link" $ do
      let display    = T.pack "b/"
      let url        = T.pack "https://root.com/a/b"
      let currentUrl = T.pack "https://root.com/a/b/c/"
      let lk = Link display url
      shouldFollow lk currentUrl `shouldBe` False

    it "block identical link" $ do
      let display    = T.pack "c/"
      let url        = T.pack "https://root.com/a/b/c/"
      let currentUrl = T.pack "https://root.com/a/b/c/"
      let lk = Link display url
      shouldFollow lk currentUrl `shouldBe` False

  describe "createResource" $ do
    it "create Folder" $ do
      let display    = T.pack "c/"
      let url        = T.pack "https://root.com/a/b/c/"
      let lk = Link display url
      createResource lk `shouldBe` Folder lk

    it "create File" $ do
      let display    = T.pack "c"
      let url        = T.pack "https://root.com/a/b/c"
      let lk = Link display url
      createResource lk `shouldBe` File lk

  describe "prettyLink" $ do
    it "works on non URL encoded link" $ do
      let display    = T.pack "c"
      let url        = T.pack "https://root.com/a/b/c"
      let lk = Link display url
      prettyLink lk `shouldBe` T.pack "c --> https://root.com/a/b/c"

    it "works on URL encoded link" $ do
      let display    = T.pack "my%20encoded%20file!"
      let url        = T.pack "https://root.com/a/b/my%20encoded%20file!"
      let lk = Link display url
      prettyLink lk `shouldBe` T.pack "my encoded file! --> https://root.com/a/b/my%20encoded%20file!"

  describe "extractLinksFromBody" $ do
    it "parse no href link" $ do
      let body = [r|<HTML>
      <HEAD>
      <TITLE>Auto-generated html formated source</TITLE>
      <META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1252">
      </HEAD>
      <BODY LINK="800080" BGCOLOR="#ffffff">
      <P> </P>
      <PRE>|]
      extractLinksFromBody (BS.pack body) `shouldBe` []

    it "parse href link" $ do
      let body = [r|<HTML>
      <HEAD>
      <TITLE>Auto-generated html formated source</TITLE>
      <META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1252">
      </HEAD>
      <BODY LINK="800080" BGCOLOR="#ffffff">
      <P> <a href="https://www.w3schools.com">Visit W3Schools</a>  </P>
      <PRE>|]
      extractLinksFromBody (BS.pack body) `shouldBe` ["https://www.w3schools.com"]

    it "parse href link (uppercased)" $ do
      let body = [r|<HTML>
      <HEAD>
      <TITLE>Auto-generated html formated source</TITLE>
      <META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1252">
      </HEAD>
      <BODY LINK="800080" BGCOLOR="#ffffff">
      <P> <A HREF="https://www.w3schools.com">Visit W3Schools</A>  </P>
      <PRE>|]
      extractLinksFromBody (BS.pack body) `shouldBe` ["https://www.w3schools.com"]