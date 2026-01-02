{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import MyLib
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "MyLib.mapemail" $ do
    it "maps @ to [at] in emails" $ do
      applyRules myRules "aaa@a.com" `shouldBe` "aaa[at]a.com"

  describe "MyLib.mapfruits" $ do
    it "maps apple to orange" $ do
      applyRules myRules "apple" `shouldBe` "orange"

  describe "MyLib.fullWidthSymbolRule" $ do
    it "你:好" $ do
      applyRules myRules "你:好" `shouldBe` "你：好"