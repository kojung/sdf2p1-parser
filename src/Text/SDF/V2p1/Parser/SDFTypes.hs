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

module Text.SDF.V2p1.Parser.SDFTypes where

import qualified Data.ByteString.Lazy.Char8 as B

----------------------------------------------------------------------------------
-- Stream type
----------------------------------------------------------------------------------
-- | This is the parser stream type. We can switch from String to ByteString or
-- other input stream by changing the definition of SdfString
type SdfString = B.ByteString

----------------------------------------------------------------------------------
-- Variables
----------------------------------------------------------------------------------
type Qstring    = String
type Number     = Double
type Rnumber    = Double
type Dnumber    = Int
type Tsvalue    = String
type Identifier = String

----------------------------------------------------------------------------------
-- SDF File Syntax
----------------------------------------------------------------------------------
data DelayFile = DelayFile SdfHeader [Cell] deriving (Show, Eq)
data SdfHeader = SdfHeader SdfVersion (Maybe DesignName) (Maybe Date) (Maybe Vendor) 
    (Maybe ProgramName) (Maybe ProgramVersion) (Maybe HierarchyDivider) 
    (Maybe Voltage) (Maybe Process) (Maybe Temperature) (Maybe TimeScale)
    deriving (Show, Eq)
type SdfVersion       = Qstring
type DesignName       = Qstring
type Date             = Qstring
type Vendor           = Qstring
type ProgramName      = Qstring
type ProgramVersion   = Qstring
type HierarchyDivider = Qstring
type Voltage          = Rtriple
type Process          = Qstring
type Temperature      = Rtriple
type TimeScale        = Qstring

----------------------------------------------------------------------------------
-- Cell Entries
----------------------------------------------------------------------------------
data Cell         = Cell Celltype CellInstance (Maybe Correlation) [TimingSpec] 
                    deriving (Show, Eq)
type Celltype     = Qstring
type CellInstance = [Identifier]
type Instance     = Identifier
data Correlation  = Correlation Qstring (Maybe CorrFactor) deriving (Show, Eq)
type CorrFactor   = [Number]

----------------------------------------------------------------------------------
-- Timing Specification
----------------------------------------------------------------------------------
data TimingSpec = TimingSpecDel DelSpec
                | TimingSpecTc  TcSpec deriving (Show, Eq)
type DelSpec = [Deltype]
type TcSpec  = [TcDef]
data Deltype = DeltypePathpulse (Maybe InputOutputPath) Value (Maybe Value)
             | DeltypeGlobalpathpulse (Maybe InputOutputPath) Value (Maybe Value)
             | DeltypeAbsolute [DelDef]
             | DeltypeIncrement [DelDef] 
               deriving (Show, Eq)
data InputOutputPath = InputOutputPath PortPath PortPath deriving (Show, Eq)
data DelDef = DelDefIopath PortSpec PortPath RvalueList
            | DelDefCond ConditionalPortExpr PortSpec PortPath RvalueList
            | DelDefPort PortPath RvalueList
            | DelDefInterconnect PortInstance PortInstance RvalueList
            | DelDefNetdelay NetSpec RvalueList
            | DelDefDevice (Maybe PortInstance) RvalueList 
              deriving (Show, Eq)
data NetSpec = NetSpec (Maybe Instance) Identifier 
               deriving (Show, Eq)
data TcDef = TcDefTchkDef TchkDef
           | TcDefCnsDef CnsDef deriving (Show, Eq)
data TchkDef = TchkDefSetup PortTchk PortTchk Rvalue
             | TchkDefHold PortTchk PortTchk Rvalue
             | TchkDefSetuphold PortTchk PortTchk Rvalue Rvalue
             | TchkDefRecovery PortTchk PortTchk Rvalue
             | TchkDefSkew PortTchk PortTchk Rvalue
             | TchkDefWidth PortTchk Value
             | TchkDefPeriod PortTchk Value
             | TchkDefNochange PortTchk PortTchk Rvalue Rvalue
               deriving (Show, Eq)
data CnsDef = CnsDefPathconstraint PortInstance [PortInstance] Rvalue Rvalue
            | CnsDefSum ConstraintPath [ConstraintPath] Rvalue (Maybe Rvalue)
            | CnsDefDiff ConstraintPath ConstraintPath Value (Maybe Value)
            | CnsDefSkewconstraint PortSpec Value
              deriving (Show, Eq)
data PortTchk = PortTchkPortSpec PortSpec
              | PortTchkCond TimingCheckCondition PortSpec
                deriving (Show, Eq)
type ConstraintPath = (PortInstance, PortInstance)
data PortSpec = PortSpecPortPath PortPath
              | PortSpecPortEdge PortEdge
                deriving (Show, Eq)
data PortEdge = PortEdge EdgeIdentifier PortPath
                deriving (Show, Eq)
type EdgeIdentifier = String
type PortPath = Port
type Port = Identifier
type ScalarPort = Identifier
type BusPort = Identifier
data PortInstance = PortInstance (Maybe Instance) Identifier 
                    deriving (Show, Eq)

----------------------------------------------------------------------------------
-- Data values
----------------------------------------------------------------------------------
type Value = Triple
type Triple = (Maybe Double, Maybe Double, Maybe Double)
type Rvalue = Triple
type Rtriple = Triple
type RvalueList = [Rvalue]

----------------------------------------------------------------------------------
-- Conditions for Path Delays
----------------------------------------------------------------------------------
type ConditionalPortExpr = String
type SimpleExpression = String

----------------------------------------------------------------------------------
-- Conditions for Timing Checks
----------------------------------------------------------------------------------
type TimingCheckCondition = String

----------------------------------------------------------------------------------
-- Constants for Expressions
----------------------------------------------------------------------------------
type ScalarConstant = String

----------------------------------------------------------------------------------
-- Operators for Expressions
----------------------------------------------------------------------------------
type UnaryOperator = String
type InversionOperator = String
type BinaryOperator = String
type EqualityOperator = String

