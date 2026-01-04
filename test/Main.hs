{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Pangu
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Pangu.cjksym(cjk)" $ do
    it "converts symbols to fullwidth" $ do
      pangu "你 : 好" `shouldBe` "你：好"
      pangu "你.好" `shouldBe` "你。好"
      pangu "你:好:他" `shouldBe` "你：好：他"
      pangu "你   ? 好" `shouldBe` "你？好"
      pangu "你…好" `shouldBe` "你… 好"
      pangu "你...好" `shouldBe` "你... 好"
      pangu "你:0" `shouldBe` "你：0"
    it "fixes quotes" $ do
      pangu "我说:\" 他说:'你好'\"" `shouldBe` "我说:\"他说:' 你好 '\""
      -- pangu "'你好'" `shouldBe` "' 你好'"  -- strange behavior
      pangu "你'hello'" `shouldBe` "你 'hello'"
      pangu "我 's " `shouldBe` "我's "
    it "fixes hash" $ do
      pangu "你好#测试#世界" `shouldBe` "你好 #测试# 世界"
    it "add spaces" $ do
      pangu "你好and世界" `shouldBe` "你好 and 世界"
      pangu "當你凝視著bug，bug也凝視著你" `shouldBe` "當你凝視著 bug，bug 也凝視著你"
      pangu "與PM戰鬥的人,應當小心自己不要成為PM" `shouldBe` "與 PM 戰鬥的人，應當小心自己不要成為 PM"