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
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Text.SDF.V2p1.Parser.SDFParser as P
import Test.HUnit.Base hiding (Test, path)
import Text.Parsec (ParseError, runParser, eof)
import Text.Printf (printf)
import Test.Framework (defaultMain, Test, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Text.Parsec.ByteString.Lazy (Parser)

----------------------------------------------------------------------------------
-- Make ParsecTest an instance of Testable
-- This is the same as Test.HUnit.Parsec, except that it works for ByteString
----------------------------------------------------------------------------------
data ParsecTest a = ParsecTest {
      parser :: Parser a
    , positiveCases :: [(a, [ByteString])]
    , negativeCases :: [ByteString]
    }

instance (Eq a, Show a) => Testable (ParsecTest a) where
    test (ParsecTest (parser :: Parser a) posCases negCases) = 
        test [test (map posTest posCases) 
             ,test (map negTest negCases)]
        where posTest (expected, inputs) = test (map testInput inputs)
                  where testInput input = 
                            assertEqual (describe input parsed) (Just expected) (maybify parsed)
                            where parsed = testParse input
                                  maybify :: Either l r -> Maybe r
                                  maybify = either (const Nothing) Just
              negTest input = assertBool (describe input result) (isError result)
                  where result = testParse input
                        isError = either (const True) (const False)
              testParse :: ByteString -> Either ParseError a
              testParse = runParser eofParser () "<test input>"
              eofParser :: Parser a
              eofParser =
                  do v <- parser
                     eof
                     return v
              describe :: ByteString -> Either ParseError a -> String
              describe input parsed =
                  printf "Input %s\nParsed:\n%s" (show input) (show parsed)

----------------------------------------------------------------------------------
-- Variables
----------------------------------------------------------------------------------
qstring = buildTest P.qstring
          [("abc", ["\"abc\""])         
          ,("  abc", ["\"  abc\""])]
          ["a", "a\"", "\"a"]
          
number = buildTest P.number
         [(123, ["123", "123.0", "123.0e0", "123e0", "1230e-1"])
         ,(0.12, ["0.12", "1.2e-1", "0.12e0", "12e-2"])]
         ["a", "12a", "123e"]
         
rnumber = buildTest P.rnumber
          [(-123, ["-123", "-123.0", "-123.0e0", "-123e0", "-1230e-1"])
          ,(-0.12, ["-0.12", "-1.2e-1", "-0.12e0", "-12e-2"])]
          ["a", "12a", "123e"]
          
dnumber = buildTest P.dnumber
          [(123, ["123", "+123"])]
          ["a", "12a", "123e"]
          
tsvalue = buildTest P.tsvalue
          [("1 ps", ["1 ps", "1 ps"])
          ,("100 ns", ["100 ns", "100  ns"])]
          ["200 ns", "1.0 ms", "abc"]
          
identifier = buildTest P.identifier
             [("abc", ["abc", "abc "])
             ,("a[1:23]", ["a[1:23]", "a[1:23]    "])
             ,("a[1:23](24)!@#$z", ["a[1:23]\\(24\\)\\!\\@\\#\\$z"])]
             ["!#@$@$"]

path = buildTest P.path
       [("abc", ["abc", "abc "])
       ,("a[1:23]", ["a[1:23]", "a[1:23]    "])
       ,("a[1:23](24)!@#$z", ["a[1:23]\\(24\\)\\!\\@\\#\\$z"])
       ,("a.b.c.d[1]", ["a.b.c.d[1]  "])
       ,("a/b/c/d[1]", ["a/b/c/d[1]  "])]
       ["!#@$@$"]
             
----------------------------------------------------------------------------------
-- SDF file syntax
----------------------------------------------------------------------------------
delay_file = buildTest P.delay_file
             [(P.DelayFile (P.SdfHeader "OVI 2.1" (Just "dff_all_v0") (Just "Mon Mar 12 14:09:12 2012") (Just "comp_rfsf_1p00v_0c") (Just "Synopsys PrimeTime") (Just "D-2010.06-SP2") (Just "/") (Just (Just 1.0,Nothing,Just 1.0)) (Just "1.000::1.000") (Just (Just 0.0,Nothing,Just 0.0)) (Just "1 ns")) [P.Cell "dff_all_v0" [""] Nothing [P.TimingSpecDel [P.DeltypeAbsolute [P.DelDefInterconnect (P.PortInstance Nothing "Idff/Imaster/d_latch") (P.PortInstance Nothing "Idff/Imux1_Islave_Iand/in0") [(Just 0.0,Nothing,Just 0.0)]]]],P.Cell "cfg_ctxymuxmux16_2_e" ["Idff/Istdec1mux16"] Nothing [P.TimingSpecDel [P.DeltypeAbsolute [P.DelDefIopath (P.PortSpecPortPath "in0") "out0" [(Just 6.3e-2,Nothing,Just 4.7e-2),(Just 6.4e-2,Nothing,Just 4.6e-2)]]]],P.Cell "dff_1bit" ["Idff/Imux1_Islave_Iand"] Nothing [P.TimingSpecDel [P.DeltypeAbsolute [P.DelDefIopath (P.PortSpecPortEdge (P.PortEdge "negedge" "close_slave")) "cavebit" [(Just 4.3e-2,Nothing,Just 4.3e-2),(Just 5.1e-2,Nothing,Just 5.1e-2)],P.DelDefIopath (P.PortSpecPortPath "in0") "cavebit" [(Just 6.5e-2,Nothing,Just 6.5e-2),(Just 6.2e-2,Nothing,Just 6.2e-2)]]],P.TimingSpecTc [P.TcDefTchkDef (P.TchkDefWidth (P.PortTchkPortSpec (P.PortSpecPortEdge (P.PortEdge "negedge" "close_slave"))) (Just 5.9e-2,Nothing,Just 5.9e-2)),P.TcDefTchkDef (P.TchkDefSetup (P.PortTchkPortSpec (P.PortSpecPortEdge (P.PortEdge "posedge" "in0"))) (P.PortTchkPortSpec (P.PortSpecPortEdge (P.PortEdge "posedge" "close_slave"))) (Just 5.5e-2,Nothing,Just 5.5e-2)),P.TcDefTchkDef (P.TchkDefHold (P.PortTchkPortSpec (P.PortSpecPortEdge (P.PortEdge "posedge" "in0"))) (P.PortTchkPortSpec (P.PortSpecPortEdge (P.PortEdge "posedge" "close_slave"))) (Just (-5.1e-2),Nothing,Just (-5.1e-2)))]]]
              ,["(DELAYFILE (SDFVERSION \"OVI 2.1\") (DESIGN \"dff_all_v0\") (DATE \"Mon Mar 12 14:09:12 2012\") (VENDOR \"comp_rfsf_1p00v_0c\") (PROGRAM \"Synopsys PrimeTime\") (VERSION \"D-2010.06-SP2\") (DIVIDER /) (VOLTAGE 1.00::1.00) (PROCESS \"1.000::1.000\") (TEMPERATURE 0.00::0.00) (TIMESCALE 1ns) (CELL (CELLTYPE \"dff_all_v0\") (INSTANCE) (DELAY (ABSOLUTE (INTERCONNECT Idff/Imaster/d_latch Idff/Imux1_Islave_Iand/in0 (0.000::0.000))))) (CELL (CELLTYPE \"cfg_ctxymuxmux16_2_e\") (INSTANCE Idff/Istdec1mux16) (DELAY (ABSOLUTE (IOPATH in0 out0 (0.063::0.047) (0.064::0.046))))) (CELL (CELLTYPE \"dff_1bit\") (INSTANCE Idff/Imux1_Islave_Iand) (DELAY (ABSOLUTE (IOPATH (negedge close_slave) cavebit (0.043::0.043) (0.051::0.051)) (IOPATH in0 cavebit (0.065::0.065) (0.062::0.062)))) (TIMINGCHECK (WIDTH (negedge close_slave) (0.059::0.059)) (SETUP (posedge in0) (posedge close_slave) (0.055::0.055)) (HOLD (posedge in0) (posedge close_slave) (-0.051::-0.051)))))"])
             ]
             []
             
sdf_header = buildTest P.sdf_header
             [(P.SdfHeader "OVI 2.1"
                    (Just "foo")
                    (Just "Mon Mar 12 14:08:51 2012")
                    (Just "lib") 
                    (Just "Synopsys PrimeTime")
                    (Just "D-2010.06")
                    (Just "/")
                    (Just (Just 0.7,Nothing,Just 0.7))
                    (Just "1.000::1.000")
                    (Just (Just 0.0,Nothing,Just 0.0))
                    (Just "1 ns")
              ,["(SDFVERSION \"OVI 2.1\") (DESIGN \"foo\") (DATE \"Mon Mar 12 14:08:51 2012\") (VENDOR \"lib\") (PROGRAM \"Synopsys PrimeTime\") (VERSION \"D-2010.06\") (DIVIDER /) // OPERATING CONDITION \"tt\"\n (VOLTAGE 0.7::0.7) (PROCESS \"1.000::1.000\") (TEMPERATURE 0.00::0.00) (TIMESCALE 1ns)"])
             ,(P.SdfHeader "OVI 2.1"
                    (Just "foo") 
                    (Just "Mon Mar 12 14:08:51 2012")
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
              ,["(SDFVERSION \"OVI 2.1\") (DESIGN \"foo\") (DATE \"Mon Mar 12 14:08:51 2012\")"])
             ,(P.SdfHeader "OVI 2.1"
                    (Just "foo") 
                    (Just "Mon Mar 12 14:08:51 2012")
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    (Just "1 ns")
              ,["(SDFVERSION \"OVI 2.1\") (DESIGN \"foo\") (DATE \"Mon Mar 12 14:08:51 2012\") (TIMESCALE 1ns)"])]
             ["(DESIGN \"foo\")", "(SDFVERSION 1.0)"]
             
sdf_version = buildTest P.sdf_version
              [("foo", ["(SDFVERSION \"foo\")", "(SDFVERSION    \"foo\"   )"])]
              ["(SDFVERSION \"foo\""]
              
design_name = buildTest P.design_name
              [("Super Hot Chip", ["(DESIGN \"Super Hot Chip\")"])]
              ["(DESIGN \"foo\""]

date = buildTest P.date
       [("  1/1/2012", ["(DATE \"  1/1/2012\")"])]
       ["(SDFVERSION \"foo\""]

vendor = buildTest P.vendor
         [("Tom & Jerry", ["(VENDOR \"Tom & Jerry\")"])]
         ["(VENDOR \"foo\""]

program_name = buildTest P.program_name
               [("Primetime", ["(PROGRAM \"Primetime\")"])]
               ["(PROGRAMNAME \"foo\")"]

program_version = buildTest P.program_version
                  [("1.0", ["(VERSION \"1.0\")"])]
                  ["(VERSIONS \"1.0\")", "(VERSION 1.0)"]
                  
hierarchy_divider = buildTest P.hierarchy_divider
                    [("/", ["(DIVIDER /)", "(DIVIDER   /)"])
                    ,(".", ["(DIVIDER .)"])]
                    ["(SDFVERSION \"foo\""]
                    
voltage = buildTest P.voltage
          [((Just 1.0, Just 1.0, Just 1.0), ["(VOLTAGE 1)"
                                            ,"(VOLTAGE 1:1.0:1)"])
          ,((Just (-1.0), Just (-1.0), Just (-1.0)), ["(VOLTAGE -1)"
                                                     ,"(VOLTAGE -1:-1.0:-1)"])]
          ["(VOLTAGE abc)"]
          
process = buildTest P.process
          [("TTTT", ["(PROCESS \"TTTT\")"])]
          ["(PROCESSES \"1.0\")", "(PROCESS 1.0)"]
          
temperature = buildTest P.temperature
              [((Just 1.0, Just 1.0, Just 1.0), ["(TEMPERATURE 1)"
                                                ,"(TEMPERATURE 1:1.0:1)"])
              ,((Just (-1.0), Just (-1.0), Just (-1.0)), ["(TEMPERATURE -1)"
                                                         ,"(TEMPERATURE -1:-1.0:-1)"])]
              ["(TEMPERATURE abc)", "(TEMP 125)"]
              
time_scale = buildTest P.time_scale
             [("1 ps", ["(TIMESCALE 1 ps)"])]
             ["(TIMESCALE 2ps)"]
             
----------------------------------------------------------------------------------
-- Cell Entries
----------------------------------------------------------------------------------
cell = buildTest P.cell
       [(P.Cell "dff_all_v0" [""] Nothing [P.TimingSpecDel [P.DeltypeAbsolute [P.DelDefInterconnect (P.PortInstance Nothing "Idff/Imaster/d_latch") (P.PortInstance Nothing "Idff/Imux1_Islave_Iand/in0") [(Just 0.0,Nothing,Just 0.0)],P.DelDefInterconnect (P.PortInstance Nothing "Idff/Imaster/gate") (P.PortInstance Nothing "Idff/Imux2_Islave_Iand/sel") [(Just 0.0,Nothing,Just 0.0)]]]]
        ,["(CELL (CELLTYPE \"dff_all_v0\") (INSTANCE) (DELAY (ABSOLUTE (INTERCONNECT Idff/Imaster/d_latch Idff/Imux1_Islave_Iand/in0 (0.000::0.000)) (INTERCONNECT Idff/Imaster/gate Idff/Imux2_Islave_Iand/sel (0.000::0.000)))))"])
       ,(P.Cell "cfg_ctxymuxmux16_2_e" ["Idff/Istdec1mux16"] Nothing [P.TimingSpecDel [P.DeltypeAbsolute [P.DelDefIopath (P.PortSpecPortPath "in0") "out0" [(Just 6.3e-2,Nothing,Just 4.7e-2),(Just 6.4e-2,Nothing,Just 4.6e-2)],P.DelDefIopath (P.PortSpecPortPath "in0") "out0_n" [(Just 6.4e-2,Nothing,Just 4.6e-2),(Just 6.5e-2,Nothing,Just 4.9e-2)],P.DelDefIopath (P.PortSpecPortPath "in0") "out1" [(Just 7.7e-2,Nothing,Just 5.9e-2),(Just 7.1e-2,Nothing,Just 5.5e-2)]]]]
        ,["(CELL (CELLTYPE \"cfg_ctxymuxmux16_2_e\") (INSTANCE Idff/Istdec1mux16) (DELAY (ABSOLUTE (IOPATH in0 out0 (0.063::0.047) (0.064::0.046)) (IOPATH in0 out0_n (0.064::0.046) (0.065::0.049)) (IOPATH in0 out1 (0.077::0.059) (0.071::0.055)))))"])
       ,(P.Cell "dff_1bit" ["Idff/Imux1_Islave_Iand"] Nothing [P.TimingSpecDel [P.DeltypeAbsolute [P.DelDefIopath (P.PortSpecPortEdge (P.PortEdge "negedge" "close_slave")) "cavebit" [(Just 8.1e-2,Nothing,Just 8.1e-2),(Just 9.7e-2,Nothing,Just 9.7e-2)],P.DelDefIopath (P.PortSpecPortPath "in0") "cavebit" [(Just 0.129,Nothing,Just 0.129),(Just 0.127,Nothing,Just 0.127)]]],P.TimingSpecTc [P.TcDefTchkDef (P.TchkDefWidth (P.PortTchkPortSpec (P.PortSpecPortEdge (P.PortEdge "negedge" "close_slave"))) (Just 6.4e-2,Nothing,Just 6.4e-2)),P.TcDefTchkDef (P.TchkDefSetup (P.PortTchkPortSpec (P.PortSpecPortEdge (P.PortEdge "posedge" "in0"))) (P.PortTchkPortSpec (P.PortSpecPortEdge (P.PortEdge "posedge" "close_slave"))) (Just 0.105,Nothing,Just 0.105)),P.TcDefTchkDef (P.TchkDefHold (P.PortTchkPortSpec (P.PortSpecPortEdge (P.PortEdge "posedge" "in0"))) (P.PortTchkPortSpec (P.PortSpecPortEdge (P.PortEdge "posedge" "close_slave"))) (Just (-0.1),Nothing,Just (-0.1)))]]
        ,["(CELL (CELLTYPE \"dff_1bit\") (INSTANCE Idff/Imux1_Islave_Iand) (DELAY (ABSOLUTE (IOPATH (negedge close_slave) cavebit (0.081::0.081) (0.097::0.097)) (IOPATH in0 cavebit (0.129::0.129) (0.127::0.127)))) (TIMINGCHECK (WIDTH (negedge close_slave) (0.064::0.064)) (SETUP (posedge in0) (posedge close_slave) (0.105::0.105)) (HOLD (posedge in0) (posedge close_slave) (-0.100::-0.100))))"])
       ]
       ["(CELL)"]
       
celltype = buildTest P.celltype
           [("foo", ["(CELLTYPE \"foo\")"])]
           ["(TIMESCALE 2ps)"]
           
cell_instance = buildTest P.cell_instance
                [(["foo"], ["(INSTANCE foo)"])
                ,(["foo","bar"], ["(INSTANCE foo) (INSTANCE bar)"])
                ,(["*"], ["(INSTANCE *)"])
                ,(["a.b.c[12:0]"], ["(INSTANCE a.b.c[12:0])"])
                ,(["a.b.c[12:0]"], ["(INSTANCE a.b.c\\[12\\:0\\])"])]
                ["(INSTANCE \"foo\")"]
                
instance' = buildTest P.instance'
    [("foo", ["(INSTANCE foo)"])
    ,("foo", ["(INSTANCE foo)"])
    ,("", ["(INSTANCE)"])
    ,("a.b.c[12:0]", ["(INSTANCE a.b.c[12:0])"])
    ,("a.b.c[12:0]", ["(INSTANCE a.b.c\\[12\\:0\\])"])]
    ["(INSTANCE \"foo\")"]
    
correlation = buildTest P.correlation
              [(P.Correlation "foo" (Just [12]), ["(CORRELATION \"foo\" 12)"])
              ,(P.Correlation "foo" (Just [12,13,14]), ["(CORRELATION \"foo\" 12 13 14)"])
              ]
              ["(CORRELATION)"]
              
corr_factor = buildTest P.corr_factor
              [([10], ["10"])
              ,([1,2,3], ["1 2 3"])
              ]
              ["(foo)"]
              
----------------------------------------------------------------------------------
-- Timing Specfifications
----------------------------------------------------------------------------------
timing_spec = buildTest P.timing_spec
              [(P.TimingSpecDel
                     [P.DeltypePathpulse
                           (Just (P.InputOutputPath "a" "b"))
                           (Just 12.0,Just 13.0,Just 14.0)
                           (Just (Just 10.0,Just 10.0,Just 10.0))
                     ,P.DeltypeAbsolute
                      [P.DelDefIopath
                            (P.PortSpecPortEdge (P.PortEdge "posedge" "clk"))
                            "din"
                            [(Just 12.0,Just 13.0,Just 14.0)
                            ,(Just 15.0,Just 14.0,Just 16.0)]
                      ,P.DelDefIopath
                       (P.PortSpecPortPath "clk")
                       "din" 
                       [(Just 12.0,Just 13.0,Just 14.0)
                       ,(Just 15.0,Just 14.0,Just 16.0)]]]
               ,["(DELAY (PATHPULSE a b (12:13:14) (10)) (ABSOLUTE (IOPATH (posedge clk) din (12:13:14) (15:14:16)) (IOPATH clk din (12:13:14) (15:14:16))))"])
              ,(P.TimingSpecTc
                     [P.TcDefTchkDef
                           (P.TchkDefSetup
                                 (P.PortTchkCond "~init" (P.PortSpecPortPath "bar"))
                                 (P.PortTchkCond "clk" (P.PortSpecPortPath "bar"))
                                 (Just 1.0,Just 2.0,Just 3.0))
                     ,P.TcDefTchkDef
                      (P.TchkDefHold
                            (P.PortTchkCond "~init" (P.PortSpecPortPath "bar"))
                            (P.PortTchkCond "clk" (P.PortSpecPortPath "bar"))
                            (Just 1.0,Just 2.0,Just 3.0))
                     ,P.TcDefTchkDef
                      (P.TchkDefSetuphold
                            (P.PortTchkCond "~init" (P.PortSpecPortPath "bar"))
                            (P.PortTchkCond "clk" (P.PortSpecPortPath "bar"))
                            (Just 1.0,Just 2.0,Just 3.0)
                            (Just 45.0,Just 45.0,Just 45.0))]
               ,["(TIMINGCHECK (SETUP (COND ~init bar) (COND clk bar) (1:2:3)) (HOLD (COND ~init bar) (COND clk bar) (1:2:3)) (SETUPHOLD (COND ~init bar) (COND clk bar) (1:2:3) (45)))"])
              ]
              ["(DELAY)(TIMINGCHECK)(TIMINGENV)"]
              
del_spec = buildTest P.del_spec
           [([P.DeltypePathpulse
                   (Just (P.InputOutputPath "a" "b"))
                   (Just 12.0,Just 13.0,Just 14.0)
                   (Just (Just 10.0,Just 10.0,Just 10.0))
             ,P.DeltypeAbsolute
              [P.DelDefIopath
                    (P.PortSpecPortEdge (P.PortEdge "posedge" "clk"))
                    "din"
                    [(Just 12.0,Just 13.0,Just 14.0)
                    ,(Just 15.0,Just 14.0,Just 16.0)]
              ,P.DelDefIopath
               (P.PortSpecPortPath "clk")
               "din"
               [(Just 12.0,Just 13.0,Just 14.0)
               ,(Just 15.0,Just 14.0,Just 16.0)]]]
            ,["(DELAY (PATHPULSE a b (12:13:14) (10)) (ABSOLUTE (IOPATH (posedge clk) din (12:13:14) (15:14:16)) (IOPATH clk din (12:13:14) (15:14:16))))"])
           ]
           ["(DELAY)"]
           
tc_spec = buildTest P.tc_spec
          [([P.TcDefTchkDef
             (P.TchkDefSetup
                   (P.PortTchkCond "~init" (P.PortSpecPortPath "bar"))
                   (P.PortTchkCond "clk" (P.PortSpecPortPath "bar"))
                   (Just 1.0,Just 2.0,Just 3.0))
            ,P.TcDefTchkDef
             (P.TchkDefHold 
                   (P.PortTchkCond "~init" (P.PortSpecPortPath "bar"))
                   (P.PortTchkCond "clk" (P.PortSpecPortPath "bar"))
                   (Just 1.0,Just 2.0,Just 3.0))
            ,P.TcDefTchkDef
             (P.TchkDefSetuphold
                   (P.PortTchkCond "~init" (P.PortSpecPortPath "bar"))
                   (P.PortTchkCond "clk" (P.PortSpecPortPath "bar"))
                   (Just 1.0,Just 2.0,Just 3.0)
                   (Just 45.0,Just 45.0,Just 45.0))
            ,P.TcDefCnsDef
             (P.CnsDefSkewconstraint
                   (P.PortSpecPortPath "clk")
                   (Just 12.0,Just 12.0,Just 12.0))]
           ,["(TIMINGCHECK (SETUP (COND ~init bar) (COND clk bar) (1:2:3)) (HOLD (COND ~init bar) (COND clk bar) (1:2:3)) (SETUPHOLD (COND ~init bar) (COND clk bar) (1:2:3) (45)) (SKEWCONSTRAINT clk (12)))"])
          ]
          ["(TIMINGCHECK)"]
          
deltype = buildTest P.deltype
          [(P.DeltypePathpulse
                 (Just (P.InputOutputPath "a" "b"))
                 (Just 12.0,Just 13.0,Just 14.0)
                 (Just (Just 10.0,Just 10.0,Just 10.0))
           ,["(PATHPULSE a b (12:13:14) (10))"])
          ,(P.DeltypeAbsolute
                 [P.DelDefIopath
                       (P.PortSpecPortEdge (P.PortEdge "posedge" "clk"))
                       "din"
                       [(Just 12.0,Just 13.0,Just 14.0),(Just 15.0,Just 14.0,Just 16.0)]
                 ,P.DelDefIopath
                  (P.PortSpecPortPath "clk")
                  "din"
                  [(Just 12.0,Just 13.0,Just 14.0),(Just 15.0,Just 14.0,Just 16.0)]]
           ,["(ABSOLUTE (IOPATH (posedge clk) din (12:13:14) (15:14:16)) (IOPATH clk din (12:13:14) (15:14:16)))"])
          ]
          []
          
input_output_path = buildTest P.input_output_path
                    [(P.InputOutputPath "foo" "bar", ["foo bar"])]
                    ["foo"]
                    
del_def = buildTest P.del_def
          [(P.DelDefIopath 
                 (P.PortSpecPortEdge (P.PortEdge "posedge" "clk"))
                 "din"
                 [(Just 12.0,Just 13.0,Just 14.0),(Just 15.0,Just 14.0,Just 16.0)]
           ,["(IOPATH (posedge clk) din (12:13:14) (15:14:16))"])
          ,(P.DelDefCond
                 "!init"
                 (P.PortSpecPortEdge (P.PortEdge "posedge" "clk"))
                 "din"
                 [(Just 12.0,Just 13.0,Just 14.0)
                 ,(Just 15.0,Just 14.0,Just 16.0)]
           ,["(COND !init (IOPATH (posedge clk) din (12:13:14) (15:14:16)))"])
          ,(P.DelDefInterconnect
                 (P.PortInstance Nothing "a/b/c")
                 (P.PortInstance Nothing "d/e/f")
                 [(Just 12.0,Just 13.0,Just 14.0)
                 ,(Just 15.0,Nothing,Just 16.0)]
           ,["(INTERCONNECT a/b/c d/e/f (12:13:14) (15::16))"])
          ]
          ["(SETUP)", "(HOLD)"]
          
net_spec = buildTest P.net_spec
           [(P.NetSpec (Just "a/b/c") "bus[123]"
            ,["(INSTANCE a/b/c) bus[123]"])
           ]
           ["(foo)"]
           
tc_def = buildTest P.tc_def
         [(P.TcDefTchkDef 
                (P.TchkDefSetup
                      (P.PortTchkCond "~init" (P.PortSpecPortPath "bar"))
                      (P.PortTchkCond "clk" (P.PortSpecPortPath "bar"))
                      (Just (-1.0),Just 2.0,Just 3.0))
          ,["(SETUP (COND ~init bar) (COND clk bar) (-1:2:3))"])
         ,(P.TcDefCnsDef
                (P.CnsDefPathconstraint
                      (P.PortInstance Nothing "din0[1:0]")
                      [P.PortInstance Nothing "din1",P.PortInstance Nothing "din2"]
                      (Just 1.0,Just 2.0,Just 3.0)
                      (Just 4.0,Just 4.0,Just 4.0))
          ,["(PATHCONSTRAINT din0[1:0] din1 din2 (1:2:3) (4))"])
         ]
         ["(foo)"]
         
tchk_def = buildTest P.tchk_def
           [(P.TchkDefSetup 
                  (P.PortTchkCond "~init" (P.PortSpecPortPath "bar"))
                  (P.PortTchkCond "clk" (P.PortSpecPortPath "bar"))
                  (Just (-1.0),Just 2.0,Just 3.0)
            ,["(SETUP (COND ~init bar) (COND clk bar) (-1:2:3))"])
           ,(P.TchkDefHold
                  (P.PortTchkCond "~init" (P.PortSpecPortPath "bar"))
                  (P.PortTchkCond "clk" (P.PortSpecPortPath "bar"))
                  (Just 1.0,Just 2.0,Just 3.0)
            ,["(HOLD (COND ~init bar) (COND clk bar) (1:2:3))"])
           ,(P.TchkDefSetuphold
                  (P.PortTchkCond "~init" (P.PortSpecPortPath "bar"))
                  (P.PortTchkCond "clk" (P.PortSpecPortPath "bar"))
                  (Just 1.0,Just 2.0,Just 3.0)
                  (Just 45.0,Just 45.0,Just 45.0)
            ,["(SETUPHOLD (COND ~init bar) (COND clk bar) (1:2:3) (45))"])
           ]
           ["(SETUP)", "(HOLD)"]
           
cns_def = buildTest P.cns_def
          [(P.CnsDefPathconstraint 
                 (P.PortInstance Nothing "din0[1:0]")
                 [P.PortInstance Nothing "din1", P.PortInstance Nothing "din2"]
                 (Just 1.0,Just 2.0,Just 3.0)
                 (Just 4.0,Just 4.0,Just 4.0)
           ,["(PATHCONSTRAINT din0[1:0] din1 din2 (1:2:3) (4))"])
          ,(P.CnsDefPathconstraint 
                 (P.PortInstance (Just "foo") "din0[1:0]")
                 [P.PortInstance (Just "bar") "din1", P.PortInstance Nothing "din2"]
                 (Just 1.0,Just 2.0,Just 3.0)
                 (Just 4.0,Just 4.0,Just 4.0)
           ,["(PATHCONSTRAINT (INSTANCE foo) din0[1:0] (INSTANCE bar) din1 din2 (1:2:3) (4))"])
          ,(P.CnsDefSum
                 (P.PortInstance Nothing "abc", P.PortInstance Nothing "def")
                 [(P.PortInstance Nothing "def", P.PortInstance Nothing "feg")]
                 (Just 12.0,Just 12.0,Just 12.0)
                 (Just (Just 13.0,Just 13.0,Just 13.0))
           ,["(SUM (abc def) (def feg) (12) (13))"])
          ,(P.CnsDefDiff
                 (P.PortInstance Nothing "abc", P.PortInstance Nothing "def")
                 (P.PortInstance Nothing "def", P.PortInstance Nothing "feg")
                 (Just 12.0,Just 12.0,Just 12.0)
                 (Just (Just 13.0,Just 13.0,Just 13.0))
           ,["(DIFF (abc def) (def feg) (12) (13))"])
          ,(P.CnsDefSkewconstraint 
                 (P.PortSpecPortPath "abc")
                 (Just 12.0,Just 13.0,Just 14.0)
           ,["(SKEWCONSTRAINT abc (12:13:14))"])
          ,(P.CnsDefSkewconstraint 
                 (P.PortSpecPortEdge (P.PortEdge "posedge" "clk"))
                 (Just 12.0,Just 13.0,Just 14.0)
           ,["(SKEWCONSTRAINT (posedge clk) (12:13:14))"])
          ]
          ["(PATHCONSTRAINT (NAME foo) din0[1:0] din1 din2 (1:2:3) (4))"]
          
port_tchk = buildTest P.port_tchk
            [(P.PortTchkCond "~init" (P.PortSpecPortPath "bar")
             ,["(COND ~init bar)"])
            ,(P.PortTchkCond "init=='b0" (P.PortSpecPortEdge (P.PortEdge "posedge" "clk"))
             ,["(COND init=='b0 (posedge clk))"])
            ,(P.PortTchkPortSpec (P.PortSpecPortPath "abc[10]")
             , ["abc[10]  "])
            ]
            ["(COND \"foo\" ~init)"]
            
constraint_path = buildTest P.constraint_path
                  [((P.PortInstance Nothing "foo"
                    ,P.PortInstance Nothing "bar[12:0]")
                   ,["(foo bar[12:0])"])]
                  ["(\"foo\" \"bar[12:0])"]
                  
port_spec = buildTest P.port_spec
            [(P.PortSpecPortPath "abc[10]", ["abc[10]  "])
            ,(P.PortSpecPortEdge (P.PortEdge "posedge" "abc[10]"), ["(posedge abc[10])"])]
            ["(11 abc)"]
            
port_edge = buildTest P.port_edge
            [(P.PortEdge "posedge" "foo", ["(posedge foo)"])]
            ["(posedge \"foo\")"]
            
edge_identifier = buildTest P.edge_identifier
                  [("posedge", ["posedge"])
                  ,("negedge", ["negedge"])
                  ,("01", ["01"])
                  ,("10", ["10"])
                  ,("0z", ["0z"])
                  ,("z1", ["z1"])
                  ,("1z", ["1z"])
                  ,("z0", ["z0"])]
                  ["010", "101", "z00"]
                  
port_instance = buildTest P.port_instance
                [(P.PortInstance Nothing "foo", ["foo"])
                ,(P.PortInstance (Just "a/b/c") "bus[123]", ["(INSTANCE a/b/c) bus[123]  "])
                ]
                ["(fewlj)"]
                
port = buildTest P.port
       [("foo", ["foo"])
       ,("bus[123]", ["bus[123]  "])
       ,("bus[12:2]", ["bus[12:2]  "])]
       ["(fewlj)"]
       
----------------------------------------------------------------------------------
-- Data Values
----------------------------------------------------------------------------------
value = buildTest P.value
        [((Just 10.0, Just 11.0, Just 12.0), ["(10:11:12)"])
        ,((Just 10.0, Just 10.0, Just 10.0), ["(10)"])
        ]
        ["::", "a:b:c", "(-10:-11:-12)", "()", "( )"]
        
triple = buildTest P.triple
         [((Just 10.0, Just 11.0, Just 12.0), ["10:11:12"])
         ,((Just 10.0, Nothing, Nothing), ["10::"])
         ,((Nothing, Just 11.0, Nothing), [":11:"])
         ,((Nothing, Nothing, Just 12.0), ["::12"])]
         ["::", "a:b:c"]
         
rvalue = buildTest P.rvalue
         [((Just (-10.0), Just (-11.0), Just (-12.0)), ["(-10:-11:-12)"])
         ,((Just (-10.0), Just (-10.0), Just (-10.0)), ["(-10)"])
         ]
         ["::", "a:b:c", "()", "( )"]
         
rtriple = buildTest P.rtriple
          [((Just (-10.0), Just (-11.0), Just (-12.0)), ["-10:-11:-12"])
          ,((Just (-10.0), Nothing, Nothing), ["-10::"])
          ,((Nothing, Just (-11.0), Nothing), [":-11:"])
          ,((Nothing, Nothing, Just (-12.0)), ["::-12"])
          ,((Nothing, Just (-1.0), Just (-12.0)), [":-1:-12"])
          ,((Just 1.0, Nothing, Just (-12.0)), ["1::-12"])
          ,((Just 1.0, Just (-2.0), Nothing), ["1:-2:"])]
          ["::", "a:b:c"]
          
rvalue_list = buildTest P.rvalue_list
              [([(Just 12.0, Just 13.0, Just 14.0)]
               ,["(12:13:14)"])
              ,([(Just 12.0,Just 12.0,Just 12.0)
                ,(Just 13.0,Just 13.0,Just 13.0)
                ,(Just 14.0,Just 14.0,Just 14.0)]
               ,["(12)(13)(14)"])
              ,([(Just 12.0,Just 12.0,Just 12.0)
                ,(Just 13.0,Just 14.0,Just 15.0)
                ,(Just 10.0,Just 12.0,Just (-13.0))
                ,(Just (-1.0), Nothing, Just 19.0)]
               ,["(12) (13:14:15) (10:12:-13) (-1::19)"])
              ]
              ["12", "()"]
              
----------------------------------------------------------------------------------
-- Conditions for Path Delays
----------------------------------------------------------------------------------
conditional_port_expr = simple_expression

simple_expression = buildTest P.simple_expression
                    [("abc", ["abc"])
                    ,("(abc)", ["(abc)", "( abc)"])
                    ,("~abc", ["~abc"])
                    ,("!abc", ["!abc"])
                    ,("124+456", ["124 + 456", "124+456"])
                    ,("123+(456*123)", ["123 + (456 * 123)", "123+(456*123)"])
                    ,("{abc==(def+456*(456/124))}", ["{abc==(def + 456 * (456/124))}"
                                                    ,"{abc==(def+456*(456/124))}"])
                    ,("!a2&!a5&!a7", ["!a2&!a5&!a7"])
                    ,("!a2&!a5&!a7", ["!a2 & !a5 & !a7"])
                    ,("a==b", ["a==b", "a == b", "a ==b"])
                    ,("a===b", ["a===b", "a=== b", "a === b"])
                    ,("a&!b&~c", ["a&!b&~c", "a & !b & ~c"])
                    ,("a+(b)", ["a+(b)", "a + (b)"])
                    ,("a*(b)", ["a*(b)", "a* (b)", "a *(b)"])
                    ]
                    ["(abc"]
                    
----------------------------------------------------------------------------------
-- Conditions for Timing Checks
----------------------------------------------------------------------------------
timing_check_condition = buildTest P.timing_check_condition
                         [("foo", ["foo"])
                         ,("~bar", ["~bar"])
                         ,("bar===baz", ["bar === baz"])
                         ,("foo==1'b0", ["foo==1'b0"])
                         ]
                         ["(abc"]
                         
----------------------------------------------------------------------------------
-- Run tests
----------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [buildTestGroup "Variables" 
                            [qstring
                            ,number
                            ,rnumber
                            ,dnumber
                            ,tsvalue
                            ,identifier
                            ,path
                            ]
        ,buildTestGroup "SDF File Syntax"
                            [delay_file
                            ,sdf_header
                            ,sdf_version
                            ,design_name
                            ,date
                            ,vendor
                            ,program_name
                            ,program_version
                            ,hierarchy_divider
                            ,voltage
                            ,process
                            ,temperature
                            ,time_scale
                            ]
        ,buildTestGroup "Cell Entries" 
                            [cell
                            ,celltype
                            ,cell_instance
                            ,instance'
                            ,correlation
                            ,corr_factor
                            ]
        ,buildTestGroup "Timing Specification"
                            [timing_spec
                            ,del_spec
                            ,tc_spec
                            ,deltype
                            ,input_output_path
                            ,del_def
                            ,net_spec
                            ,tc_def
                            ,tchk_def
                            ,cns_def
                            ,port_tchk
                            ,constraint_path
                            ,port_spec
                            ,port_edge
                            ,edge_identifier
                            ,port_instance
                            ,port
                            ]
        ,buildTestGroup "Data Values"
                            [value
                            ,triple
                            ,rvalue
                            ,rtriple
                            ,rvalue_list
                            ]
        ,buildTestGroup "Conditions for Path Delays"
                            [conditional_port_expr
                            ,simple_expression
                            ]
        ,buildTestGroup "Conditions for Timing Checks"
                            [timing_check_condition]
        ]

buildTestGroup name lst = testGroup name (hUnitTestToTests (TestList lst))

buildTest p pos neg = test ParsecTest {
                        parser = p
                      , positiveCases = map processPos pos
                      , negativeCases = map pack neg
                      }
    where processPos (expected, inputs) = (expected, map pack inputs)
       




