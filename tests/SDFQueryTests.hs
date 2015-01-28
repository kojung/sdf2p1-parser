-----------------------------------------------------------------------------
-- The MIT License (MIT)
--
-- Copyright (c) 2015 Jung Ko
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import qualified Text.SDF.V2p1.Parser.SDFQuery as Q
import Text.SDF.V2p1.Parser.SDFTypes
import qualified Test.HUnit.Base as HT
import Test.Framework (defaultMain, Test, testGroup, TestName)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import qualified Data.ByteString.Lazy.Char8 as B

----------------------------------------------------------------------------------
-- Test harness
----------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests

buildTestGroup :: TestName -> [HT.Test] -> Test
buildTestGroup name lst = testGroup name (hUnitTestToTests (HT.TestList lst))

prefix :: String
prefix = "tests/sdf"   -- for Cabal
-- prefix = "../tests/sdf"   -- for GHCi

----------------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------------
version :: HT.Test
version = HT.TestCase (do s <- B.readFile $ prefix ++ "/sample.sdf"
                          let header = Q.parseHeaders "sample.sdf" s
                          HT.assertEqual "version" (getVersion header) "OVI 2.1")
    where getVersion (SdfHeader v _ _ _ _ _ _ _ _ _ _) = v

divider :: HT.Test
divider = HT.TestCase (do s <- B.readFile $ prefix ++ "/sample.sdf"
                          let header = Q.parseHeaders "sample.sdf" s
                          HT.assertEqual "divider" (getDivider header) "/")
    where getDivider (SdfHeader _ _ _ _ _ _ (Just d) _ _ _ _) = d
          getDivider _ = error "Could not find divider"
    
cellsLengthStrict :: HT.Test
cellsLengthStrict = HT.TestCase (do s <- B.readFile $ prefix ++ "/sample.sdf"
                                    let cells = Q.parseCells "sample.sdf" s id
                                    HT.assertEqual "length of cells" (length cells) 17)

cellsInstanceStrict :: HT.Test
cellsInstanceStrict = HT.TestCase (do s <- B.readFile $ prefix ++ "/sample.sdf"
                                      let cells = Q.parseCells "sample.sdf" s fn
                                      HT.assertEqual "cells instance" 
                                        cells
                                        ["","Idff/Istdec1mux16","Idff/Imux1_Islave_Iand","Idff/Istdec0mux16","Idff/Imux4_Islave_Iand","Idff/Imux6_Islave_Iand","Idff/Imux16","Idff/Imux10_Islave_Iand","Idff/Imux3_Islave_Iand","Idff/Imux8_Islave_Iand","Idff/Imux5_Islave_Iand","Idff/Imux7_Islave_Iand","Idff/Imaster","Idff/Imux9_Islave_Iand","Idff/Imux11_Islave_Iand","Idff/Imux0_Islave_Iand","Idff/Imux2_Islave_Iand"])
    where fn (Cell _ iden _ _) = concat iden

cellsSpecificStrict :: HT.Test
cellsSpecificStrict = HT.TestCase (do s <- B.readFile $ prefix ++ "/sample.sdf"
                                      let cells = Q.parseCells "sample.sdf" s fn
                                      HT.assertEqual "look for specific cells"
                                            (length (filter (\(Cell name _ _ _) -> name /= "") cells))
                                            2)
    where fn c@(Cell "cfg_ctxymuxmux16_2_e" _ _ _) = c
          fn _ = Cell "" [] Nothing []

cellsLengthLazy :: HT.Test
cellsLengthLazy = HT.TestCase (do s <- B.readFile $ prefix ++ "/sample.sdf"
                                  let cells = Q.parseCellsLazy "sample.sdf" s id
                                  HT.assertEqual "length of cells" (length cells) 17)

cellsInstanceLazy :: HT.Test
cellsInstanceLazy = HT.TestCase (do s <- B.readFile $ prefix ++ "/sample.sdf"
                                    let cells = Q.parseCellsLazy "sample.sdf" s fn
                                    HT.assertEqual "cells instance" 
                                      cells
                                      ["","Idff/Istdec1mux16","Idff/Imux1_Islave_Iand","Idff/Istdec0mux16","Idff/Imux4_Islave_Iand","Idff/Imux6_Islave_Iand","Idff/Imux16","Idff/Imux10_Islave_Iand","Idff/Imux3_Islave_Iand","Idff/Imux8_Islave_Iand","Idff/Imux5_Islave_Iand","Idff/Imux7_Islave_Iand","Idff/Imaster","Idff/Imux9_Islave_Iand","Idff/Imux11_Islave_Iand","Idff/Imux0_Islave_Iand","Idff/Imux2_Islave_Iand"])
    where fn (Cell _ iden _ _) = concat iden

cellsSpecificLazy :: HT.Test
cellsSpecificLazy = HT.TestCase (do s <- B.readFile $ prefix ++ "/sample.sdf"
                                    let cells = Q.parseCellsLazy "sample.sdf" s fn
                                    HT.assertEqual "look for specific cells"
                                          (length (filter (\(Cell name _ _ _) -> name /= "") cells))
                                          2)
    where fn c@(Cell "cfg_ctxymuxmux16_2_e" _ _ _) = c
          fn _ = Cell "" [] Nothing []

tests :: [Test]
tests = [buildTestGroup "Headers" [version,divider]
        ,buildTestGroup "Cells Strict" [cellsLengthStrict
                                       ,cellsInstanceStrict
                                       ,cellsSpecificStrict]
        ,buildTestGroup "Cells Lazy" [cellsLengthLazy
                                     ,cellsInstanceLazy
                                     ,cellsSpecificLazy]
        ]


