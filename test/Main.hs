{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import MyLib
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "MyLib.cjksym(cjk)" $ do
    it "converts symbols to fullwidth" $ do
      applyRules myRules "你 : 好" `shouldBe` "你：好"
      applyRules myRules "你.好" `shouldBe` "你。好"
      applyRules myRules "你:好:他" `shouldBe` "你：好：他"
      applyRules myRules "你   ? 好" `shouldBe` "你？好"
      applyRules myRules "你…好" `shouldBe` "你… 好"
      applyRules myRules "你...好" `shouldBe` "你... 好"
      applyRules myRules "你:0" `shouldBe` "你：0"
    it "fixes quotes" $ do
      applyRules myRules "我说:\" 他说:'你好'\"" `shouldBe` "我说:\"他说:' 你好 '\""
      -- applyRules myRules "'你好'" `shouldBe` "' 你好'"  -- strange behavior
      applyRules myRules "你'hello'" `shouldBe` "你 'hello'"
      applyRules myRules "我 's " `shouldBe` "我's "