{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import MyLib
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "MyLib.cjksymcjk" $ do
    it "converts symbols between CJK characters to fullwidth" $ do
      applyRules myRules "你 : 好" `shouldBe` "你：好"
      applyRules myRules "你.好" `shouldBe` "你。好"
      applyRules myRules "你   ? 好" `shouldBe` "你？好"