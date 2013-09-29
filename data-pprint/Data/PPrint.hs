-- |Prettyprint and compare 'Data' values.
module Data.PPrint
    ( pprint
    , (===)
    ) where

import Data.Data.GenRep.Functions (numberErrors)
import Data.Data.GenRep.Doc (Doc, toDoc)
import Data.Data.Eval (eval)
import Data.Data.Compare

import Text.PrettyPrint.HughesPJ (fsep, nest, text, vcat, (<>), (<+>), ($+$))
import Data.Data (Data)

---------------------

-- |Prettyprint a 'Data' value.
--
-- There is a 1 second time limit and the output
-- contains at most approximately 500 characters.
--
-- The exceptions are shown as bottom signs
-- followed by explanations.
pprint :: Data a => a -> IO Doc
pprint = pprintTo 700

-- |Prettyprint a 'Data' value, showing up to approximately
-- the specified number of characters. Use this to show
-- more than the default pprint allows.
pprintTo :: Data a => Int -> a -> IO Doc
pprintTo max x = do
    x <- eval 1 max x
    let ([x'], es) = numberErrors [x]
    return $ toDoc x' $+$ nest 2 (showBotts es)


infix 0 === 

-- |Compare two 'Data' values.
--
-- The can be yes, no or maybe.
-- The differences are highlighted.
--
-- There is a 1 second time limit and the output
-- contains at most approximately 500 characters.
--
-- The exceptions are shown as bottom signs
-- followed by explanations.
(===) :: Data a => a -> a -> IO Doc
a === b = do
    (ans, a, b) <- compareData 0.8 0.2 700 a b
    let x = showAnswer ans
    let ([a', b'], es) = numberErrors [a, b]
    return $ fsep [nest (length x + 1) (toDoc a'), text x <+> toDoc b']
         $+$ nest 2 (showBotts es)

----------------------

showBotts :: [(String, String)] -> Doc
showBotts es = vcat $ map f es
 where
    f (i, e) = text i <> text ":" <+> vcat (map text $ lines e)


