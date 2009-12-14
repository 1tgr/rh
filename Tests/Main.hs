module Main where

import Rh.ExtractValue
import Rh.RenameType
import Rh.Source
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList t
  where t = [ "Transforming an invalid program should result in an error" ~:
                let input = "??? LOL PONIES"
                 in transformFileContents return input ~?= Right "Parse failed at (1:1): Parse error: ???",

               "Transforming nothing should change nothing" ~:
                 let input = "data NotBlah = NotBlah\n"
                  in transformFileContents return input ~?= Left input,

               "Renaming nonexistent type should change nothing" ~:
                 let input = "data NotBlah = NotBlah\n"
                  in transformFileContents (renameType "Blah" "Blah2") input ~?= Left input,
              
               "Renaming type without changing spacing" ~:
                 let input = "data Blah1 = Blah1\n"
                     expectedProgram = "data Blah2 = Blah1\n"
                  in transformFileContents (renameType "Blah1" "Blah2") input ~?= Left expectedProgram,
                  
               "Renaming type, increasing spacing" ~:
                  let input = "data Blah = Blah\n"
                      expectedProgram = Left "data NewBlah = Blah\n"
                      actualProgram = transformFileContents (renameType "Blah" "NewBlah") input
                   in actualProgram ~?= expectedProgram,
                   
               "Renaming type, decreasing spacing" ~:
                  let input = "data Blah = Blah\n"
                      expectedProgram = "data B = Blah\n"
                   in transformFileContents (renameType "Blah" "B") input ~?= Left expectedProgram,
                   
               "Renaming exported type" ~:
                  let input = "module Main(Blah) where\ndata Blah = Blah\n"
                      expectedProgram = "module Main(NewBlah) where\ndata NewBlah = Blah\n"
                   in transformFileContents (renameType "Blah" "NewBlah") input ~?= Left expectedProgram,

                "Extract literal" ~:
                  let input = "doSomething = 1 + 1\n"
                      expectedProgram = "doSomething =one+one where one = 1\n"
                   in transformFileContents (extractValue (1, 15) (1, 16) "one") input ~?= Left expectedProgram ]