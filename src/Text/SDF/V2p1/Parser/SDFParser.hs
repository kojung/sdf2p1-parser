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

{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Text.SDF.V2p1.Parser.SDFParser 
    (module Text.SDF.V2p1.Parser.SDFParser
    ,module Text.SDF.V2p1.Parser.SDFTypes
    ) where
    
import Text.Parsec
import qualified Text.Parsec.Token    as P
import qualified Text.Parsec.Expr     as E
import qualified Text.Parsec.Language as L
import Control.Applicative            hiding ((<|>), many)
import Text.Parsec.ByteString.Lazy           (Parser)
import Data.Functor.Identity                 (Identity)
 
----------------------------------------------------------------------------------
-- The following imports are useful for debugging interactively
----------------------------------------------------------------------------------
-- import Text.Parsec.ByteString.Lazy (parseFromFile)
-- import Data.ByteString.Lazy.Char8  (ByteString, pack)
-- import Debug.Trace (trace, traceShow)
-- import Data.List (sort)

import Text.SDF.V2p1.Parser.SDFTypes

{- | 
   Language definition for the SDF file format
   Note that the input stream is specialized for the 'SdfString' type so that 
   we can easily change the stream type in one single place
-}
sdflang :: P.GenLanguageDef SdfString a Identity
sdflang = L.emptyDef
          {P.commentStart = "/*"
          ,P.commentEnd	 = "*/"
          ,P.commentLine = "//"
          ,P.nestedComments = True
          ,P.identStart = letter
          ,P.identLetter = alphaNum <|> oneOf "_'"
          ,P.reservedNames = 
              ["DELAYFILE", "SDFVERSION", "DESIGN", "DATE", "VENDOR", "PROGRAM"
              ,"VERSION", "DIVIDER", "VOLTAGE", "PROCESS", "TEMPERATURE", "TIMSCALE"
              ,"CELL", "CELLTYPE", "INSTANCE", "DELAY", "TIMINGCHECK", "TIMINGENV" 
              ,"PATHPULSE", "PATHPULSEPERCENT", "ABSOLUTE", "INCREMENT", "IOPATH" 
              ,"RETAIN", "COND", "CONDELSE", "PORT", "INTERCONNECT", "DEVICE" 
              ,"SETUP", "HOLD", "SETUPHOLD", "RECOVERY", "REMOVAL", "RECREM" 
              ,"SKEW", "WIDTH", "PERIOD", "NOCHANGE", "SCOND", "CCOND", "NAME"
              ,"EXCEPTION", "PATHCONSTRAINT", "PERIODCONSTRAINT", "SUM", "DIFF" 
              ,"SKEWCONSTRAINT", "ARRIVAL", "DEPARTURE", "SLACK", "WAVEFORM"
              ,"posedge", "negedge", "01", "10", "0z", "z1", "1z", "z0"
              ,"1'b0", "1'b1", "1'B0", "1'B1", "'b0", "'b1", "'B0", "'B1", "0", "1"]
          ,P.reservedOpNames = 
             ["+", "-", "!", "~", "&", "~&", "|", "~|", "^", "^~", "~^"
             ,"*", "/", "%", "==", "!=", "===", "!==", "&&", "||", "<", "<="
             ,">", ">=", ">>", "<<"]
          ,P.caseSensitive  = False
          ,P.opStart        = P.opLetter sdflang
          ,P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~" 
          }

-- | This is a convenience function used to parse a SDF file and return the AST
-- representation of the SDF file. 
parseSdf :: FilePath -> SdfString -> DelayFile
parseSdf f s = case parse delay_file f s of
                 Left err -> error $ show err
                 Right df -> df

----------------------------------------------------------------------------------
-- Lexer
----------------------------------------------------------------------------------
lexer :: P.GenTokenParser SdfString a Identity
lexer = P.makeTokenParser sdflang

lexeme :: ParsecT SdfString u Identity a -> ParsecT SdfString u Identity a
lexeme = P.lexeme lexer

symbol :: String -> ParsecT SdfString u Identity String
symbol = P.symbol lexer

parens :: ParsecT SdfString u Identity a -> ParsecT SdfString u Identity a
parens = P.parens lexer

reserved   :: String -> ParsecT SdfString u Identity ()
reserved   = P.reserved lexer

reservedOp :: String -> ParsecT SdfString u Identity ()
reservedOp = P.reservedOp lexer

braces :: ParsecT SdfString u Identity a -> ParsecT SdfString u Identity a
braces = P.braces lexer

whiteSpace :: ParsecT SdfString u Identity ()
whiteSpace = P.whiteSpace lexer

----------------------------------------------------------------------------------
-- Variables
----------------------------------------------------------------------------------
qstring :: Parser String
qstring = lexeme $ between (char '"') (symbol "\"") (many (noneOf "\""))
               
number :: Parser Number
number = lexeme     (try (read1 <$> many1 digit <*> (char '.' *> many1 digit) <*> (oneOf "eE" *> sign) <*> many1 digit)
                 <|> try (read2 <$> many1 digit <*> (char '.' *> many1 digit))
                 <|> try (read3 <$> many1 digit <*> (oneOf "eE" *> sign) <*> many1 digit)                             
                 <|> (read <$> many1 digit)
                 <?> "number")
    where read1 d1 d2 s d3 = read $ d1 ++ "." ++ d2 ++ "e" ++ s ++ d3
          read2 d1 d2 = read $ d1 ++ "." ++ d2
          read3 d1 s d2 = read $ d1 ++ "e" ++ s ++ d2

rnumber :: Parser Rnumber
rnumber = lexeme $ neg <$> sign <*> number
    where neg s n = case s of
                      "" -> n
                      "-" -> negate n
                      _ -> error "Unexpected sign character from function 'rnumber'"
                                 
dnumber :: Parser Dnumber
dnumber = lexeme $ read <$> ((char '+' <|> return '+') *> many1 digit)
 
tsvalue :: Parser Tsvalue
tsvalue = join <$> lexeme tsvalue_n <*> lexeme tsvalue_u
    where tsvalue_n =     try (string "100.0")
                      <|> try (string "10.0")
                      <|> try (string "1.0")
                      <|> try (string "100")
                      <|> try (string "10")
                      <|> string "1"
          tsvalue_u = choice [string "ns", string "us", string "ps"]
          join n u = n ++ " " ++ u

identifier :: Parser Identifier
identifier = lexeme identifier' 

path :: Parser Identifier
path = lexeme $ concat <$> many1 (choice [identifier', hchar])
             
----------------------------------------------------------------------------------
-- SDF file syntax
----------------------------------------------------------------------------------
delay_file :: Parser DelayFile
delay_file = parens (reserved "DELAYFILE" *>
                     (DelayFile <$> sdf_header <*> many1 cell))

sdf_header :: Parser SdfHeader
sdf_header = SdfHeader <$> sdf_version
             <*> maybeParser design_name
             <*> maybeParser date
             <*> maybeParser vendor
             <*> maybeParser program_name
             <*> maybeParser program_version
             <*> maybeParser hierarchy_divider
             <*> maybeParser voltage
             <*> maybeParser process
             <*> maybeParser temperature
             <*> maybeParser time_scale

sdf_version :: Parser SdfVersion
sdf_version = sdf_simple "SDFVERSION" qstring

design_name :: Parser DesignName
design_name = sdf_simple "DESIGN" qstring
                        
date :: Parser Date
date = sdf_simple "DATE" qstring
                  
vendor :: Parser Vendor
vendor = sdf_simple "VENDOR" qstring
                    
program_name :: Parser ProgramName
program_name = sdf_simple "PROGRAM" qstring
                          
program_version :: Parser ProgramVersion
program_version = sdf_simple "VERSION" qstring
                             
hierarchy_divider :: Parser HierarchyDivider
hierarchy_divider = sdf_simple "DIVIDER" hchar
                               
hchar :: Parser String
hchar = choice [symbol ".", symbol "/"]

voltage :: Parser Voltage
voltage = sdf_simple "VOLTAGE" rtriple_or_rnumber
                     
process :: Parser Process
process = sdf_simple "PROCESS" qstring
                     
temperature :: Parser Temperature
temperature = sdf_simple "TEMPERATURE" rtriple_or_rnumber 
                                                 
time_scale :: Parser TimeScale
time_scale = sdf_simple "TIMESCALE" tsvalue

----------------------------------------------------------------------------------
-- Cell Entries
----------------------------------------------------------------------------------
cell :: Parser Cell
cell = parens (reserved "CELL" *> (Cell <$> celltype <*> cell_instance <*> maybeParser correlation <*> many timing_spec))

celltype :: Parser Celltype
celltype = sdf_simple "CELLTYPE" qstring

cell_instance :: Parser CellInstance
cell_instance = try (sdf_simple "INSTANCE" (string "*") *> return ["*"]) 
                <|> many1 (try instance') 

instance' :: Parser Instance
instance' = sdf_simple "INSTANCE" (try path <|> return "")

correlation :: Parser Correlation
correlation = parens (reserved "CORRELATION" *> (Correlation <$> qstring <*> maybeParser corr_factor))

corr_factor :: Parser CorrFactor
corr_factor = many number

----------------------------------------------------------------------------------
-- Timing Specifications
----------------------------------------------------------------------------------
timing_spec :: Parser TimingSpec
timing_spec = try (TimingSpecDel <$> del_spec)
              <|> try (TimingSpecTc <$> tc_spec)
              <?> "timing_spec"

del_spec :: Parser DelSpec
del_spec = parens (reserved "DELAY" *> many1 deltype)

tc_spec :: Parser TcSpec
tc_spec = parens (reserved "TIMINGCHECK" *> many1 tc_def)

deltype :: Parser Deltype
deltype = parens     (try (reserved "PATHPULSE"       *> (DeltypePathpulse       <$> maybeParser input_output_path <*> value <*> maybeParser value))
                  <|> try (reserved "GLOBALPATHPULSE" *> (DeltypeGlobalpathpulse <$> maybeParser input_output_path <*> value <*> maybeParser value))
                  <|> try (reserved "ABSOLUTE"        *> (DeltypeAbsolute        <$> many1 del_def))
                  <|> try (reserved "INCREMENT"       *> (DeltypeIncrement       <$> many1 del_def))
                  <?> "deltype")

input_output_path :: Parser InputOutputPath
input_output_path = InputOutputPath <$> port_path <*> port_path
                       
del_def :: Parser DelDef
del_def = parens     (try (reserved "IOPATH"       *> (DelDefIopath       <$> port_spec <*> port_path <*> rvalue_list))
                  <|> try (reserved "COND"         *> (DelDefCond         <$> conditional_port_expr <*> (symbol "(" *> reserved "IOPATH" *> port_spec) <*> port_path <*> (rvalue_list <* symbol ")")))
                  <|> try (reserved "PORT"         *> (DelDefPort         <$> port_path <*> rvalue_list))
                  <|> try (reserved "INTERCONNECT" *> (DelDefInterconnect <$> port_instance <*> port_instance <*> rvalue_list))
                  <|> try (reserved "NETDELAY"     *> (DelDefNetdelay     <$> net_spec <*> rvalue_list))
                  <|> try (reserved "DEVICE"       *> (DelDefDevice       <$> maybeParser port_instance <*> rvalue_list))
                  <?> "del_def"
                 )

net_spec :: Parser NetSpec
net_spec = NetSpec <$> maybeParser instance' <*> identifier
              
tc_def :: Parser TcDef
tc_def = try (TcDefTchkDef <$> tchk_def)
         <|> (TcDefCnsDef <$> cns_def)
         <?> "tc_def"

tchk_def :: Parser TchkDef
tchk_def = parens     (try (reserved "SETUP"     *> (TchkDefSetup     <$> port_tchk <*> port_tchk <*> rvalue))
                   <|> try (reserved "HOLD"      *> (TchkDefHold      <$> port_tchk <*> port_tchk <*> rvalue))
                   <|> try (reserved "SETUPHOLD" *> (TchkDefSetuphold <$> port_tchk <*> port_tchk <*> rvalue <*> rvalue))
                   <|> try (reserved "RECOVERY"  *> (TchkDefRecovery  <$> port_tchk <*> port_tchk <*> rvalue))
                   <|> try (reserved "SKEW"      *> (TchkDefSkew      <$> port_tchk <*> port_tchk <*> rvalue))
                   <|> try (reserved "WIDTH"     *> (TchkDefWidth     <$> port_tchk <*> value))
                   <|> try (reserved "PERIOD"    *> (TchkDefPeriod    <$> port_tchk <*> value))
                   <|> try (reserved "NOCHANGE"  *> (TchkDefNochange  <$> port_tchk <*> port_tchk <*> rvalue <*> rvalue))
                   <?> "tchk_def"
                  )

cns_def :: Parser CnsDef
cns_def = parens     (try (reserved "PATHCONSTRAINT" *> (CnsDefPathconstraint <$> port_instance <*> many1 port_instance <*> rvalue <*> rvalue))
                  <|> try (reserved "SUM"            *> (CnsDefSum            <$> constraint_path <*> many1 (try constraint_path) <*> rvalue <*> maybeParser rvalue))
                  <|> try (reserved "DIFF"           *> (CnsDefDiff           <$> constraint_path <*> constraint_path <*> value <*> maybeParser value))
                  <|> try (reserved "SKEWCONSTRAINT" *> (CnsDefSkewconstraint <$> port_spec <*> value))
                  <?> "cns_def")

port_tchk :: Parser PortTchk
port_tchk = try (PortTchkPortSpec <$> port_spec)
            <|> parens (reserved "COND" *> (PortTchkCond <$> timing_check_condition <*> port_spec))
            <?> "port_tchk"

constraint_path :: Parser ConstraintPath
constraint_path = parens ((,) <$> port_instance <*> port_instance)

port_spec :: Parser PortSpec
port_spec = try (PortSpecPortPath <$> port_path)
            <|> (PortSpecPortEdge <$> port_edge)
            <?> "port_spec"

port_edge :: Parser PortEdge
port_edge = parens (PortEdge <$> edge_identifier <*> port_path)

edge_identifier :: Parser EdgeIdentifier
edge_identifier = try (res "posedge")
                  <|> try (res "negedge")
                  <|> try (res "01")
                  <|> try (res "10")
                  <|> try (res "0z")
                  <|> try (res "z1")
                  <|> try (res "1z")
                  <|> try (res "z0")
                  <?> "edge_identifier"
    where res r = reserved r *> return r

port_path :: Parser PortPath
port_path = path -- The spec say port_path = (port | PATH hchar port), which simplifies to path
            
port :: Parser Port
port = scalar_port <|> bus_port

scalar_port :: Parser ScalarPort
scalar_port = identifier
              
bus_port :: Parser BusPort
bus_port = identifier

port_instance :: Parser PortInstance
port_instance = PortInstance <$> maybeParser instance' <*> port_path
                   
----------------------------------------------------------------------------------
-- Data Values
----------------------------------------------------------------------------------
value :: Parser Triple
value = valueOrRvalue number

triple :: Parser Triple
triple = tripleOrRtriple number

rvalue :: Parser Triple
rvalue = valueOrRvalue rnumber

rtriple :: Parser Triple
rtriple = tripleOrRtriple rnumber

rvalue_list :: Parser RvalueList
rvalue_list = many1 rvalue

----------------------------------------------------------------------------------
-- Conditions for Path Delays
----------------------------------------------------------------------------------
conditional_port_expr :: Parser String
conditional_port_expr = simple_expression

simple_expression :: Parser String
simple_expression = E.buildExpressionParser table factor <?> "simple_expression"
    where table = 
              -- since we are returning a string, and not a strict AST
              -- simply sort the operators by length, the longest having the
              -- highest precence. This allows the naive implementation of
              -- reservedOpNaive to work
              [map unary unary_operator
              ,map unary inversion_operator
              ,map binary binary_operator
              ,map binary equality_operator
              ]
              where unary  op = E.Prefix (reservedOpNaive op *> return (op++))
                    binary op = E.Infix  (reservedOpNaive op *> return (\a b -> a++op++b)) E.AssocLeft
          factor =     (\s -> "("++s++")") <$> parens simple_expression
                   <|> (\s -> "{"++s++"}") <$> braces simple_expression
                   <|> scalar_constant
                   <|> port 
                   <?> "simple_expression"
          reservedOpNaive name = try (symbol name)

----------------------------------------------------------------------------------
-- Conditions for Timing Checks
----------------------------------------------------------------------------------
timing_check_condition :: Parser TimingCheckCondition
timing_check_condition = simple_expression

----------------------------------------------------------------------------------
-- Constants for Expressions
----------------------------------------------------------------------------------
scalar_constant :: Parser String
scalar_constant = try (res "1'b0")
                  <|> try (res "1'b1")
                  <|> try (res "1'B0")
                  <|> try (res "1'B1")
                  <|> try (res "'b0")
                  <|> try (res "'b1")
                  <|> try (res "'B0")
                  <|> try (res "'B1")
                  <|> try (res "0")
                  <|> try (res "1")
                  <?> "edge_identifier"
    where res r = reserved r *> return r

----------------------------------------------------------------------------------
-- Operators for Expressions
----------------------------------------------------------------------------------
unary_operator :: [String]
unary_operator = ["~|","~^","~&","~","|","^~","^","-","+","&","!"]

inversion_operator :: [String]
inversion_operator = ["!", "~"]

binary_operator :: [String]
binary_operator = ["~^","||","|","^~","^",">>",">=",">","===","==","<=","<<"
                  ,"<","/","-",",","+","*","&&","&","%","!==","!="]

equality_operator :: [String]
equality_operator = ["===","==","!==","!="]

----------------------------------------------------------------------------------
-- Parser utilities
----------------------------------------------------------------------------------
-- | Given a parser, attempt to parse. If parser succeeds, returns a 'Just' value,
-- else 'Nothing' is returned.
maybeParser :: Parser a -> Parser (Maybe a)
maybeParser p = try (Just <$> p)
                <|> return Nothing

-- | Since the format (FOO ...) occurs so often in SDF syntax, the parser
-- 'sdf_simple' is a short cut version for this parser
sdf_simple :: String -> Parser a -> Parser a
sdf_simple name p = parens (reserved name *> p)

-- | Parses a rtriple or a rnumber
-- Here we take a shortcut in the data representation, where an 'rnumber'
-- is represented as an rtriple of the same value
rtriple_or_rnumber :: Parser Rtriple
rtriple_or_rnumber = try rtriple
                     <|> (\n -> (Just n, Just n, Just n)) <$> rnumber

-- | Parses a 'value' or an 'rvalue'
valueOrRvalue :: Parser Double -> Parser Triple
valueOrRvalue numOrRnum = parens     (try (tripleOrRtriple numOrRnum)
                                  <|> try ((\n -> (Just n, Just n, Just n)) <$> numOrRnum)
                                  <?> "value or rvalue")

-- | Parses a posPair or a negPair
-- name1 and name2 is the edge name, ie. posedge or negedge  
posOrNegPair :: String 
             -> String 
             -> Parser ((Rnumber, Maybe Rnumber), (Rnumber, Maybe Rnumber))
posOrNegPair name1 name2 = (,) 
                           <$> parens (reserved name1 *> ((,) <$> rnumber <*> maybeParser rnumber))
                           <*> parens (reserved name2 *> ((,) <$> rnumber <*> maybeParser rnumber))

-- | Parses the negative sign '-', or the positive '+'
-- Absence of sign implies positive
sign :: Parser String
sign = choice [string "-", string "+", string ""]

tripleOrRtriple :: Parser Double -> Parser Triple
tripleOrRtriple numOrRnum = 
      try ((,,) <$> (Just <$> numOrRnum)
                <*> (symbol ":" *> maybeParser numOrRnum)
                <*> (symbol ":" *> maybeParser numOrRnum))
  <|> try ((,,) <$> maybeParser numOrRnum
                <*> (symbol ":" *> (Just <$> numOrRnum))
                <*> (symbol ":" *> maybeParser numOrRnum))
  <|> try ((,,) <$> maybeParser numOrRnum
                <*> (symbol ":" *> maybeParser numOrRnum)
                <*> (symbol ":" *> (Just <$> numOrRnum)))
  <?> "triple or rtriple"

-- | This is the identifier defined in the SDF spec, but the parser
-- does not consume trailing white spaces (ie. wrapped in a lexeme)
-- as other parsers do. We keep this version of the parser so that
-- we can combine it with 'hchar' parser later to form 'path' parser
identifier' :: Parser Identifier
identifier' = many1 (validChar <|> (backslashChar *> specialChar))
    where backslashChar = char '\\'
          validChar = choice [alphaNum, oneOf "_:[]"]
          specialChar = oneOf "!\"#$%%'()*+,-./:;<=>?@[\\]^`{|}~"
