module Main where

import Rh.RenameType
import Rh.Source
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList [ "Transforming an invalid program should result in an error" ~:
                               let input = "??? LOL PONIES"
                                in transformFileContents id input ~?= Right "Parse failed at (1:1): Parse error: ???",

                              "Transforming nothing should change nothing" ~:
                                let input = "data NotBlah = NotBlah\n"
                                 in transformFileContents id input ~?= Left input,

                              "Renaming nonexistent type should change nothing" ~:
                                let input = "data NotBlah = NotBlah\n"
                                 in transformFileContents (renameType "Blah" "Blah2") input ~?= Left input,
                              
                              "Renaming type without changing spacing" ~:
                                let input = "data Blah1 = Blah1\n"
                                    expectedProgram = "data Blah2 = Blah1\n"
                                 in transformFileContents (renameType "Blah1" "Blah2") input~?= Left expectedProgram ]