{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module ProjetoFuncional20192 (module Prelude,  module Data.Foldable) where
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
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
    Right ps -> print (Data.Foldable.sum (pegaTodasAsReceitas mes ano (removeTransacoesNaoConsideradas ps)) + Data.Foldable.sum (pegaTodasAsDespesas mes ano (removeTransacoesNaoConsideradas ps)) )

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

main :: IO ()
main = do
 d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])

 case d of
  Left err -> putStrLn err
  Right ps -> print [x | x <- ps, (valor x) > 10000 ]