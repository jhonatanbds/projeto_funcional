module ProjetoFuncionalSpec where

import Test.Hspec
import ProjetoFuncional20192

main :: IO ()
main = hspec $ do
    describe "filtrarTransacoesPorAno" $ do
    it "returns the original number when given a positive input" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case d of
        Left err -> putStrLn err
        Right ps -> [x | x <- ps, (year (dataC x)) == ano ]  `shouldBe` [34534,433]

    it "returns a positive number when given a negative input" $
        absolute (-1) `shouldBe` 1

    it "returns zero when given zero" $
        absolute 0 `shouldBe` 0