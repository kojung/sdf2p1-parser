# INTRODUCTION

This is a standard compliant Standard Delay Format (SDF 2.1) parser. The library reads SDF file and produces
an Abstract Syntax Tree (AST) that downstream tools can use to query information in the SDF.

See http://www.eda.org/sdf/sdf_2.1.pdf for more information about the SDF syntax.

# INSTALLATION

Follow the standard cabal installation procedure:

> cabal install

# TEST

Unit tests are provided and can be executed like this:

> cabal configure --enable-tests
> cabal test

# USAGE

See the package `Text.SDF.V2p1.Parser.SDFParser` for the exposed methods for parsing and accessing the AST.


