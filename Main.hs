{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-simpl #-}
module Main where

import Compact

main :: IO ()
main = print $$(lift True)
