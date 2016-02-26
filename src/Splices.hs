{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Splices where

------------------------------------------------------------------------------
--import Control.Applicative
--import Control.Arrow (second)
import qualified Data.Text as T
import Heist.Interpreted
import Heist.SpliceAPI

import qualified Models.Shoes as Shoes

{----------------------------------------------------------------------------------------------------{
                                                                       | Generic Splices
}----------------------------------------------------------------------------------------------------}

numericSplice :: (Num a, Show a, Monad m) => a -> Splice m
numericSplice = textSplice . T.pack . show

listToSplice :: Monad m => (a -> Splices (Splice m)) -> [a] -> Splice m
listToSplice splice = mapSplices (runChildrenWith . splice)

{----------------------------------------------------------------------------------------------------{
                                                                       | Shoe Splices
}----------------------------------------------------------------------------------------------------}

shoeSplices :: Monad m => Shoes.Shoe -> Splices (Splice m)
shoeSplices s = do
	"id" #! numericSplice $ Shoes.id s
	"description" #! textSplice $ Shoes.description s
	"color" #! textSplice $ Shoes.color s
	"size" #! numericSplice $ Shoes.size s
