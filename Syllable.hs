module Syllable where

-- Taken from IPA Pulmonic Consonants
data Consonant = VlBilabialPlosive             -- p
               | VdBilabialPlosive             -- b
               | VdBilabialNasal               -- m
               | VdBilabialTrill               -- ʙ
               | VlBilabialFricative           -- ɸ
               | VdBilabialFricative           -- β
               | VdLabiodentalNasal            -- ɱ
               | VdLabiodentalFlap             -- ⱱ
               | VlLabiodentalFricative        -- f
               | VdLabiodentalFricative        -- v
               | VdLabiodentalApproximant      -- ʋ
               | VlAlveolarPlosive             -- t
               | VdAlveolarPlosive             -- d
               | VdAlveolarNasal               -- n
               | VdAlveolarTrill               -- r
               | VdAlveolarTap                 -- ɾ
               | VlDentalFricative             -- θ
               | VdDentalFricative             -- ð
               | VlAlveolarFricative           -- s
               | VdAlveolarFricative           -- z
               | VlPostalveolarFricative       -- ʃ
               | VdPostalveolarFricative       -- ʒ
               | VlLateralAlveolarFricative    -- ɬ
               | VdLateralAlveolarFricative    -- ɮ
               | VdAlveolarApproximant         -- ɹ
               | VdLateralAlveolarApproximant  -- l
               | VlRetroflexPlosive            -- ʈ
               | VdRetroflexPlosive            -- ɖ
               | VdRetroflexNasal              -- ɳ
               | VdRetroflexFlap               -- ɽ
               | VlRetroflexFricative          -- ʂ
               | VdRetroflexFricative          -- ʐ
               | VdRetroflexApproximant        -- ɻ
               | VdRetroflexLateralApproximant -- ɭ
               | VlPalatalPlosive              -- c
               | VdPalatalPlosive              -- ɟ
               | VdPalatalNasal                -- ɲ
               | VlPalatalFricative            -- ç
               | VdPalatalFricative            -- ʝ
               | VdPalatalApproximant          -- j
               | VdPalatalLateralApproximant   -- ʎ
               | VlVelarPlosive                -- k
               | VdVelarPlosive                -- g
               | VdVelarNasal                  -- ŋ
               | VlVelarFricative              -- x
               | VdVelarFricative              -- ɣ
               | VdVelarApproximant            -- ɰ
               | VdVelarLateralApproximant     -- ʟ
               | VlUvularPlosive               -- q
               | VdUvularPlosive               -- ɢ
               | VdUvularNasal                 -- ɴ
               | VdUvularTrill                 -- ʀ
               | VlUvularFricative             -- χ
               | VdUvularFricative             -- ʁ
               | VlPharyngealFricative         -- ħ
               | VdPharyngealFricative         -- ʕ
               | VlGlottalPlosive              -- ʔ
               | VlGlottalFricative            -- h
               | VdGlottalFricative            -- ɦ
               deriving (Enum, Bounded)

-- IPA Vowels
data Vowel = CloseFrontUnrounded        -- i
           | CloseFrontRounded          -- y
           | LoweredCloseFrontUnrounded -- ɪ
           | LoweredCloseFrontRounded   -- ʏ
           | CloseMidFrontUnrounded     -- e
           | CloseMidFrontRounded       -- ø
           | OpenMidFrontUnrounded      -- ɛ
           | OpenMidFrontRounded        -- œ
           | RaisedOpenFrontUnrounded   -- æ
           | OpenFrontUnrounded         -- a
           | OpenFrontRounded           -- ɶ
           | CloseCentralUnrounded      -- ɨ
           | CloseCentralRounded        -- ʉ
           | CloseMidCentralUnrounded   -- ɘ
           | CloseMidCentralRounded     -- ɵ
           | MidCentralUnrounded        -- ə
           | OpenMidCentralUnrounded    -- ɜ
           | OpenMidCentralRounded      -- ɞ
           | RaisedOpenCentralUnrounded -- ɐ
           | CloseBackUnrounded         -- ɯ
           | CloseBackRounded           -- u
           | LoweredCloseBackRounded    -- ʊ
           | CloseMidBackUnrounded      -- ɤ
           | CloseMidBackRounded        -- o
           | OpenMidBackUnrounded       -- ʌ
           | OpenMidBackRounded         -- ɔ
           | OpenBackUnrounded          -- ɑ
           | OpenBackRounded            -- ɒ
           deriving (Enum, Bounded)

data Phoneme = ConsonantPhoneme Consonant
             | VowelPhoneme Vowel

getPotentialRomanizations :: Phoneme -> [[Char]]
getPotentialRomanizations (ConsonantPhoneme c) = getConsMappings c
getPotentialRomanizations (VowelPhoneme v)     = getVowelMappings v

getConsMappings :: Consonant -> [[Char]]
getConsMappings VlBilabialPlosive             = ["<consonant-ex>"]
getConsMappings VdBilabialPlosive             = ["<consonant-ex>"]
getConsMappings VdBilabialNasal               = ["<consonant-ex>"]
getConsMappings VdBilabialTrill               = ["<consonant-ex>"]
getConsMappings VlBilabialFricative           = ["<consonant-ex>"]
getConsMappings VdBilabialFricative           = ["<consonant-ex>"]
getConsMappings VdLabiodentalNasal            = ["<consonant-ex>"]
getConsMappings VdLabiodentalFlap             = ["<consonant-ex>"]
getConsMappings VlLabiodentalFricative        = ["<consonant-ex>"]
getConsMappings VdLabiodentalFricative        = ["<consonant-ex>"]
getConsMappings VdLabiodentalApproximant      = ["<consonant-ex>"]
getConsMappings VlAlveolarPlosive             = ["<consonant-ex>"]
getConsMappings VdAlveolarPlosive             = ["<consonant-ex>"]
getConsMappings VdAlveolarNasal               = ["<consonant-ex>"]
getConsMappings VdAlveolarTrill               = ["<consonant-ex>"]
getConsMappings VdAlveolarTap                 = ["<consonant-ex>"]
getConsMappings VlDentalFricative             = ["<consonant-ex>"]
getConsMappings VdDentalFricative             = ["<consonant-ex>"]
getConsMappings VlAlveolarFricative           = ["<consonant-ex>"]
getConsMappings VdAlveolarFricative           = ["<consonant-ex>"]
getConsMappings VlPostalveolarFricative       = ["<consonant-ex>"]
getConsMappings VdPostalveolarFricative       = ["<consonant-ex>"]
getConsMappings VlLateralAlveolarFricative    = ["<consonant-ex>"]
getConsMappings VdLateralAlveolarFricative    = ["<consonant-ex>"]
getConsMappings VdAlveolarApproximant         = ["<consonant-ex>"]
getConsMappings VdLateralAlveolarApproximant  = ["<consonant-ex>"]
getConsMappings VlRetroflexPlosive            = ["<consonant-ex>"]
getConsMappings VdRetroflexPlosive            = ["<consonant-ex>"]
getConsMappings VdRetroflexNasal              = ["<consonant-ex>"]
getConsMappings VdRetroflexFlap               = ["<consonant-ex>"]
getConsMappings VlRetroflexFricative          = ["<consonant-ex>"]
getConsMappings VdRetroflexFricative          = ["<consonant-ex>"]
getConsMappings VdRetroflexApproximant        = ["<consonant-ex>"]
getConsMappings VdRetroflexLateralApproximant = ["<consonant-ex>"]
getConsMappings VlPalatalPlosive              = ["<consonant-ex>"]
getConsMappings VdPalatalPlosive              = ["<consonant-ex>"]
getConsMappings VdPalatalNasal                = ["<consonant-ex>"]
getConsMappings VlPalatalFricative            = ["<consonant-ex>"]
getConsMappings VdPalatalFricative            = ["<consonant-ex>"]
getConsMappings VdPalatalApproximant          = ["<consonant-ex>"]
getConsMappings VdPalatalLateralApproximant   = ["<consonant-ex>"]
getConsMappings VlVelarPlosive                = ["<consonant-ex>"]
getConsMappings VdVelarPlosive                = ["<consonant-ex>"]
getConsMappings VdVelarNasal                  = ["<consonant-ex>"]
getConsMappings VlVelarFricative              = ["<consonant-ex>"]
getConsMappings VdVelarFricative              = ["<consonant-ex>"]
getConsMappings VdVelarApproximant            = ["<consonant-ex>"]
getConsMappings VdVelarLateralApproximant     = ["<consonant-ex>"]
getConsMappings VlUvularPlosive               = ["<consonant-ex>"]
getConsMappings VdUvularPlosive               = ["<consonant-ex>"]
getConsMappings VdUvularNasal                 = ["<consonant-ex>"]
getConsMappings VdUvularTrill                 = ["<consonant-ex>"]
getConsMappings VlUvularFricative             = ["<consonant-ex>"]
getConsMappings VdUvularFricative             = ["<consonant-ex>"]
getConsMappings VlPharyngealFricative         = ["<consonant-ex>"]
getConsMappings VdPharyngealFricative         = ["<consonant-ex>"]
getConsMappings VlGlottalPlosive              = ["<consonant-ex>"]
getConsMappings VlGlottalFricative            = ["<consonant-ex>"]
getConsMappings VdGlottalFricative            = ["<consonant-ex>"]

getVowelMappings :: Vowel -> [[Char]]
getVowelMappings CloseFrontUnrounded        = ["<vowel-ex>"]
getVowelMappings CloseFrontRounded          = ["<vowel-ex>"]
getVowelMappings LoweredCloseFrontUnrounded = ["<vowel-ex>"]
getVowelMappings LoweredCloseFrontRounded   = ["<vowel-ex>"]
getVowelMappings CloseMidFrontUnrounded     = ["<vowel-ex>"]
getVowelMappings CloseMidFrontRounded       = ["<vowel-ex>"]
getVowelMappings OpenMidFrontUnrounded      = ["<vowel-ex>"]
getVowelMappings OpenMidFrontRounded        = ["<vowel-ex>"]
getVowelMappings RaisedOpenFrontUnrounded   = ["<vowel-ex>"]
getVowelMappings OpenFrontUnrounded         = ["<vowel-ex>"]
getVowelMappings OpenFrontRounded           = ["<vowel-ex>"]
getVowelMappings CloseCentralUnrounded      = ["<vowel-ex>"]
getVowelMappings CloseCentralRounded        = ["<vowel-ex>"]
getVowelMappings CloseMidCentralUnrounded   = ["<vowel-ex>"]
getVowelMappings CloseMidCentralRounded     = ["<vowel-ex>"]
getVowelMappings MidCentralUnrounded        = ["<vowel-ex>"]
getVowelMappings OpenMidCentralUnrounded    = ["<vowel-ex>"]
getVowelMappings OpenMidCentralRounded      = ["<vowel-ex>"]
getVowelMappings RaisedOpenCentralUnrounded = ["<vowel-ex>"]
getVowelMappings CloseBackUnrounded         = ["<vowel-ex>"]
getVowelMappings CloseBackRounded           = ["<vowel-ex>"]
getVowelMappings LoweredCloseBackRounded    = ["<vowel-ex>"]
getVowelMappings CloseMidBackUnrounded      = ["<vowel-ex>"]
getVowelMappings CloseMidBackRounded        = ["<vowel-ex>"]
getVowelMappings OpenMidBackUnrounded       = ["<vowel-ex>"]
getVowelMappings OpenMidBackRounded         = ["<vowel-ex>"]
getVowelMappings OpenBackUnrounded          = ["<vowel-ex>"]
getVowelMappings OpenBackRounded            = ["<vowel-ex>"]

data Syllable = Syllable Consonant Vowel Consonant
instance Show Syllable where
    show (Syllable c1 v c2) = show c1 ++ show v ++ show c2
