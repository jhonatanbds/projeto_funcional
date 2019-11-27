{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Crud (Transaction(..), GregorianCalendar(..), module Prelude,  module Data.Foldable) where

import Test.Hspec
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import GHC.List (sum)
import Data.Foldable (Foldable, foldr, foldl, sum)


data GregorianCalendar =
  GregorianCalendar {
    year :: !Int,
    month :: !Int,
    dayOfMonth :: !Int
} deriving (Show,Generic)

data Transaction = Transaction {
  textoIdentificador :: !String,
  valor :: !Float,
  descricao :: !String,
  numeroDOC :: !String,
  classificada :: Bool,
  dataC :: GregorianCalendar, {-- data is a keyword for haskell --}
  tipos :: [String]
} deriving (Show,Generic)


-- Instances to convert our type to/from JSON.

instance FromJSON GregorianCalendar where
  parseJSON (Object v) =
    GregorianCalendar <$> v .: "year"
                      <*> v .: "month"
                      <*> v .: "dayOfMonth"

instance ToJSON GregorianCalendar


instance FromJSON Transaction
instance ToJSON Transaction

-- | Location of the local copy, in case you have it,
--   of the JSON file.
jsonFile :: FilePath
jsonFile = "data.json"

-- Move the right brace (}) from one comment to another
-- to switch from local to remote.

{--}
-- Read the local copy of the JSON file.
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile
--}

filtrarTransacoesPorAno :: Int -> IO()
filtrarTransacoesPorAno ano= do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print [x | x <- ps, (year (dataC x)) == ano ]

filtrarTransacoesPorAnoMes :: Int -> Int -> IO()
filtrarTransacoesPorAnoMes ano mes= do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (filtrarPorMesAno mes ano ps)


calculaValorDasReceitas  :: Int -> Int -> IO()
calculaValorDasReceitas mes ano  = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (Data.Foldable.sum (pegaTodasAsReceitas mes ano (removeTransacoesNaoConsideradas ps)))

calculaValorDasDespesas  :: Int -> Int -> IO()
calculaValorDasDespesas mes ano  = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (Data.Foldable.sum (pegaTodasAsDespesas mes ano (removeTransacoesNaoConsideradas ps)))

calculaASobra :: Int -> Int -> IO()
calculaASobra mes ano = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (Data.Foldable.sum (pegaTodasAsReceitas mes ano (removeTransacoesNaoConsideradas ps)) - Data.Foldable.sum (pegaTodasAsDespesas mes ano (removeTransacoesNaoConsideradas ps)) )
  

calculaMediaDaSobra :: Int -> IO()
calculaMediaDaSobra ano = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print ((Data.Foldable.sum (pegaTodasAsReceitasDoAno ano (removeTransacoesNaoConsideradas ps)) - Data.Foldable.sum (pegaTodasAsDespesasDoAno ano (removeTransacoesNaoConsideradas ps)) ) / (fromIntegral (Prelude.length  (pegaTodasAsDespesasDoAno ano (removeTransacoesNaoConsideradas ps)))))

calculaMaxNoMesAno :: Int -> Int -> IO ()
calculaMaxNoMesAno mes ano = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (maxNoDado (filtrarPorMesAno mes ano (removeTransacoesNaoConsideradas ps)))

calculaMinNoMesAno :: Int -> Int -> IO ()
calculaMinNoMesAno mes ano = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (minNoDado (filtrarPorMesAno mes ano (removeTransacoesNaoConsideradas ps)))

calculaMediaDasReceitasDoAno :: Int -> IO ()
calculaMediaDasReceitasDoAno ano = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print ((Data.Foldable.sum  (pegaTodasAsReceitasDoAno ano (removeTransacoesNaoConsideradas ps))) / (fromIntegral (Prelude.length  (pegaTodasAsReceitasDoAno ano (removeTransacoesNaoConsideradas ps)))))

calculaMediaDasDespesasDoAno :: Int -> IO ()
calculaMediaDasDespesasDoAno ano = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print ((Data.Foldable.sum  (pegaTodasAsDespesasDoAno ano (removeTransacoesNaoConsideradas ps))) / (fromIntegral (Prelude.length  (pegaTodasAsDespesasDoAno ano (removeTransacoesNaoConsideradas ps)))))

calculaFluxoDeCaixa :: Int -> Int -> IO ()
calculaFluxoDeCaixa mes ano = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (fluxo (filtrarPorMesAno mes ano (removeTransacoesNaoConsideradas ps)))


{-- AUXILIARY FUNCTIONS --}

diasPresentesNoMes :: [Transaction] -> [Int]
diasPresentesNoMes dado = quickSort (diasNoMes dado)

diasNoMes :: [Transaction] -> [Int]
diasNoMes dado = Data.Foldable.foldl (\acc x -> if not (elem ( dayOfMonth (dataC x))  acc) then acc ++ [( dayOfMonth (dataC x)) ] else acc) [ ( dayOfMonth (dataC (Prelude.head dado)))] dado

transacoesDoDia :: [Transaction] -> Int -> [Transaction]
transacoesDoDia dado dia = [x | x <- dado, ( dayOfMonth (dataC x)) == dia]

saldoDoDia :: [Transaction] -> Float
saldoDoDia transacoesDiaria = Data.Foldable.sum [(valor x) | x <- transacoesDiaria]

saldoDiario :: [Transaction] -> Int -> Float
saldoDiario dado dia = saldoDoDia (transacoesDoDia dado dia)

fluxo :: [Transaction] -> [(Int, Float)]
fluxo dado = Prelude.map (\x -> (x, saldoDiario dado x)) (diasPresentesNoMes dado)

calculaSaldoDoDia :: Foldable t => t1 -> t Transaction -> (t1, Float)
calculaSaldoDoDia dia dado = (dia, saldo) where
  saldo = Data.Foldable.foldl (\acc x -> acc + (valor x)) 0 dado

filtrarPorMesAno :: Int -> Int -> [Transaction] -> [Transaction]
filtrarPorMesAno mes ano dado = [x | x <- dado, ( year (dataC x)) == ano, ( month (dataC x)) == mes ]

removeTransacoesNaoConsideradas :: [Transaction] -> [Transaction]
removeTransacoesNaoConsideradas dado = [x | x <- dado, not ("SALDO_CORRENTE" `elem` (tipos x)), not ("APLICACAO" `elem` (tipos x)), not ("VALOR_APLICACAO" `elem` (tipos x))  ]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs)  = quickSort smaller ++ [x] ++ quickSort larger
    where smaller = Prelude.filter (<=x) xs
          larger  = Prelude.filter (> x) xs

minNoDado :: [Transaction] -> Transaction
minNoDado dado = minNoDado' (Prelude.head dado) (Prelude.tail dado)

minNoDado' :: Transaction -> [Transaction] -> Transaction
minNoDado' maximum [] = maximum
minNoDado' maximum (x:xs)
  | (valor maximum) > (valor x) = minNoDado' x xs
  | otherwise = minNoDado' maximum xs

maxNoDado :: [Transaction] -> Transaction
maxNoDado dado = maxNoDado' (Prelude.head dado) (Prelude.tail dado)

maxNoDado' :: Transaction -> [Transaction] -> Transaction
maxNoDado' maximum [] = maximum
maxNoDado' maximum (x:xs)
  | (valor maximum) <= (valor x) = maxNoDado' x xs
  | otherwise = maxNoDado' maximum xs

saldoFinalDoMesANo:: Int -> Int -> IO()
saldoFinalDoMesANo = calculaASobra

pegaTodasAsReceitas :: Int -> Int -> [Transaction] -> [Float]
pegaTodasAsReceitas mes ano dado = [(valor x) | x <- dado, ( year (dataC x)) == ano, ( month (dataC x)) == mes, ( valor x) > 0 ]

pegaTodasAsDespesas :: Int -> Int -> [Transaction] -> [Float]
pegaTodasAsDespesas mes ano dado = [(valor x) | x <- dado, ( year (dataC x)) == ano, ( month (dataC x)) == mes, ( valor x) < 0 ]

pegaTodasAsReceitasDoAno ano dado = [(valor x) | x <- dado, ( year (dataC x)) == ano, ( valor x) > 0 ]
pegaTodasAsDespesasDoAno ano dado =  [(valor x) | x <- dado, ( year (dataC x)) == ano, ( valor x) < 0 ]

get_texto t = (textoIdentificador t)

main :: IO ()
main = hspec $ do
    describe "filtrarTransacoesPorAno" $ do
    it "quantidade de transacoes no ano de 2017" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
            Left err -> putStrLn err
            Right ps -> Prelude.length [x | x <- ps, (year (dataC x)) == 2017 ]  `shouldBe` 1336
    it "quantidade de transacoes no ano de 2018" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
            Left err -> putStrLn err
            Right ps -> Prelude.length [x | x <- ps, (year (dataC x)) == 2018 ]  `shouldBe` 1146
    it "quantidade de transacoes no ano de 2019" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
            Left err -> putStrLn err
            Right ps -> Prelude.length [x | x <- ps, (year (dataC x)) == 2019 ]  `shouldBe` 1207
    describe "filtrarTransacoesPorAnoMes" $ do
    it "quantidade de transacoes no ano de 2018 no mes 3" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
          Left err -> putStrLn err
          Right ps -> Prelude.length (filtrarPorMesAno 3 2018 ps) `shouldBe` 96
        
    it "quantidade de transacoes no ano de 2019 no mes 1" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
          Left err -> putStrLn err
          Right ps -> Prelude.length (filtrarPorMesAno 1 2019 ps) `shouldBe` 84

    it "quantidade de transacoes no ano de 2018 no mes 12" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
            Left err -> putStrLn err
            Right ps -> Prelude.length (filtrarPorMesAno 12 2018 ps) `shouldBe` 0
    
    describe "calculaValorDasReceitas" $ do        
      it "Calcular o valor das receitas (créditos) no ano de 2018 no mes 12" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
          Left err -> putStrLn err
          Right ps -> (Data.Foldable.sum (pegaTodasAsReceitas 12 2018 (removeTransacoesNaoConsideradas ps))) `shouldBe` 0.0

    describe "calculaValorDasDespesas" $ do        
      it "Calcular o valor das despesas (débitos) ano de 2018 no mes 12" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
          Left err -> putStrLn err
          Right ps -> (Data.Foldable.sum (pegaTodasAsDespesas 12 2018 (removeTransacoesNaoConsideradas ps))) `shouldBe` 0.0

    describe "calculaValorDasSobras" $ do        
      it "Calcular a sobra (receitas - despesas) no ano de 2018 no mes 12" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
          Left err -> putStrLn err
          Right ps -> (Data.Foldable.sum (pegaTodasAsReceitas 12 2018 (removeTransacoesNaoConsideradas ps)) - Data.Foldable.sum (pegaTodasAsDespesas 12 2018 (removeTransacoesNaoConsideradas ps)) ) `shouldBe` 0.0
      
    describe "calculaSaldo" $ do        
      it "Calcular o saldo final" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        pending
    
    describe "calculaSaldoMaximo" $ do        
      it "Calcular o saldo máximo atingido no ano de 2018 no mes 3" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
          Left err -> putStrLn err
          Right ps -> (get_texto (maxNoDado (filtrarPorMesAno 3 2018 (removeTransacoesNaoConsideradas ps)))) `shouldBe` "CR\201D.LIQ.COBRAN\199A"
      
    describe "calculaSaldoMinimo" $ do        
      it "Calcular o saldo minimo atingido no ano de 2018 no mes 3" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
          Left err -> putStrLn err
          Right ps -> (get_texto (minNoDado (filtrarPorMesAno 3 2018 (removeTransacoesNaoConsideradas ps)))) `shouldBe` "PGS-CH PR\211P COOP/AG"
      
    describe "calculaMediaReceitas" $ do        
      it "Calcular a média das receitas no ano de 2018" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
          Left err -> putStrLn err
          Right ps -> ((Data.Foldable.sum  (pegaTodasAsReceitasDoAno 2018 (removeTransacoesNaoConsideradas ps))) / (fromIntegral (Prelude.length  (pegaTodasAsReceitasDoAno 2018 (removeTransacoesNaoConsideradas ps))))) `shouldBe` 5415.0576
      
    describe "calculaMediaDespesas" $ do        
      it "Calcular a média das despesas no ano de 2018" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
          Left err -> putStrLn err
          Right ps -> ((Data.Foldable.sum  (pegaTodasAsDespesasDoAno 2018 (removeTransacoesNaoConsideradas ps))) / (fromIntegral (Prelude.length  (pegaTodasAsDespesasDoAno 2018 (removeTransacoesNaoConsideradas ps))))) `shouldBe` -1428.7981
      
    describe "calculaMediaSobras" $ do        
      it "Calcular a média das sobras" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        pending
  
    describe "retornaFluxoDeCaixa" $ do        
      it "Retornar o fluxo de caixa de determinado mês/ano no ano de 2018 no mes 3" $ do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
        case d of
          Left err -> putStrLn err
          Right ps -> (fluxo (filtrarPorMesAno 3 2018 (removeTransacoesNaoConsideradas ps))) `shouldBe` [(2,8.350048),(3,5962.22),(4,-789.92),(5,-23551.18),(6,-15569.9),(9,-1154.0),(10,-13446.461),(11,3908.8801),(12,1360.1399),(13,2607.11),(16,-6976.09),(17,2136.5999),(18,3174.2002),(19,-3577.6802),(20,55066.8),(23,1644.8301),(24,1905.19),(25,88.51997),(26,-14158.721),(27,475.88),(30,304.35)]
      
      



    

