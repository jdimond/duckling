-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.PT.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = PT}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 1)
             [ "1"
             , "um"
             , "Uma"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "dois"
             , "duas"
             ]
  , examples (NumeralValue 3)
             [ "3"
             , "três"
             , "tres"
             ]
  , examples (NumeralValue 6)
             [ "6"
             , "seis"
             ]
  , examples (NumeralValue 11)
             [ "11"
             , "onze"
             ]
  , examples (NumeralValue 12)
             [ "12"
             , "doze"
             , "uma dúzia"
             , "uma duzia"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "Catorze"
             , "quatorze"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "dezesseis"
             , "dezasseis"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "dezessete"
             , "dezassete"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "dezoito"
             ]
  , examples (NumeralValue 19)
             [ "19"
             , "dezenove"
             , "dezanove"
             ]
  , examples (NumeralValue 21)
             [ "21"
             , "vinte e um"
             ]
  , examples (NumeralValue 23)
             [ "23"
             , "vinte e tres"
             , "vinte e três"
             ]
  , examples (NumeralValue 24)
             [ "24"
             , "vinte e quatro"
             , "duas dúzias"
             , "duas duzias"
             ]
  , examples (NumeralValue 50)
             [ "50"
             , "cinquenta"
             , "cinqüenta"
             , "cincoenta"
             ]
  , examples (NumeralValue 70)
             [ "setenta"
             ]
  , examples (NumeralValue 78)
             [ "setenta e oito"
             ]
  , examples (NumeralValue 80)
             [ "oitenta"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "trinta e três"
             , "trinta e tres"
             ]
  , examples (NumeralValue 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (NumeralValue 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (NumeralValue 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (NumeralValue 100)
             [ "100"
             , "Cem"
             ]
  , examples (NumeralValue 243)
             [ "243"
             ]
  , examples (NumeralValue 300)
             [ "trezentos"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             ]
  , examples (NumeralValue 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "menos 1.200.000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  , examples (NumeralValue 1.5)
             [ "1 ponto cinco"
             , "um ponto cinco"
             , "1,5"
             ]
  ]
