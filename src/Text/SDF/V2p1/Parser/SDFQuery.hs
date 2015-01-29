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

module Text.SDF.V2p1.Parser.SDFQuery 
    (parseCells
    ,parseCellsLazy
    ,parseHeaders) where
    
import Text.Parsec

----------------------------------------------------------------------------------
-- The following imports are useful for debugging interactively
----------------------------------------------------------------------------------
-- import Text.SDF.V2p1.Parser.SDFParser (parseFromFile)
-- import Debug.Trace (trace, traceShow)
-- import Data.ByteString.Lazy.Char8  (pack, readFile)

import Text.SDF.V2p1.Parser.SDFParser
import Data.ByteString.Lazy.Char8  (empty)
import Text.Parsec.ByteString.Lazy (GenParser)
import Control.Applicative hiding (empty)

-- | Given a SDF file, parse the cells using the customized cell parser
-- and return a list of parsed cells. This function can be used whenever the SDF
-- is too large to parse wholesale
parseCells :: FilePath -> SdfString -> (Cell -> a) -> [a]
parseCells f s g = case parse p f s of
                     Left err -> error $ show err
                     Right cells -> map g cells
    where p = parens (reserved "DELAYFILE" *> sdf_header *> many1 cell)

-- | Given a SDF file, lazily parse the cells using the customized cell parser
-- and return a list of parsed cells. This function can be used whenever the SDF
-- is too large to parse wholesale. 
parseCellsLazy :: FilePath -> SdfString -> (Cell -> a) -> [a]
parseCellsLazy f s g = map g (lazyMany cell f input)
    where input = case parse p f s of
                    Left err -> error $ show err
                    Right x -> x
          p =  symbol "(" *> reserved "DELAYFILE" *> sdf_header *> getInput

-- | Given a SDF file, parse the SDF headers, ignoring all the cell instances
parseHeaders :: FilePath -> SdfString -> SdfHeader
parseHeaders f s = case parse p f s of
                     Left err -> error $ show err
                     Right res -> res
    where p = symbol "(" *> reserved "DELAYFILE" *> sdf_header                     

-- | lazy version of the many parser
-- Note that the following function does not return a parser, but returns a 
-- list instead
lazyMany :: GenParser SdfString () a -> SourceName -> SdfString -> [a]
lazyMany p file contents = lm state0
    where
      Right state0 = parse getParserState file contents
      lm state = case parse p' "" empty of
                   Left _ -> [] -- ignore parse errors
                   Right x -> x
          where
            p' = do
              setParserState state
              choice
                 [ do eof
                      return []
                 , do x <- p
                      state' <- getParserState
                      return (x : lm state')
                 ]
