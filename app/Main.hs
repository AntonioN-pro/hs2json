module Main (main) where

import Prelude hiding ((<>))

import SimpleJSON
import PutJSON (putJValue)
import qualified PrettyJSON
import Prettify (compact, pretty)

sample :: JValue
sample =
  JObject
    [ ("dupla", JString "Antonio Neto e Gabriel Mesquita Torres"),
      ("disciplina", JString "Programacao Funcional"),
      ("unidade", JNumber 2),
      ("aprovado", JBool True),
      ("topicos", JArray [JString "Haskell", JString "Cabal", JString "QuickCheck"]),
      ("observacoes", JNull)
    ]

main :: IO ()
main = do
  putStrLn "== Renderizador simples (PutJSON) =="
  putJValue sample

  putStrLn ""
  putStrLn "== Renderizacao compacta (Prettify / PrettyJSON) =="
  putStrLn (compact (PrettyJSON.renderJValue sample))

  putStrLn ""
  putStrLn "== Renderizacao formatada (pretty, largura 40) =="
  putStrLn (pretty 40 (PrettyJSON.renderJValue sample))

  putStrLn ""
  putStrLn "== Extracao segura com accessors (SimpleJSON) =="
  print (getString (JString "hs2json"))
  print (getInt (JNumber 42.9))
  print (getDouble (JNumber 3.14159))
  print (getBool (JBool True))
  print (isNull JNull)
  print (getObject sample)
