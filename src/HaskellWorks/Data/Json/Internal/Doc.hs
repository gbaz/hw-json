module HaskellWorks.Data.Json.Internal.Doc
  ( hEncloseSep
  ) where

import Prettyprinter

hEncloseSep :: Doc a -> Doc a -> Doc a -> [Doc a] -> Doc a
hEncloseSep l r s ds = case ds of
  []  -> l <> r
  [d] -> l <> d <> r
  _   -> hcat (zipWith (<>) (l : repeat s) ds) <> r
