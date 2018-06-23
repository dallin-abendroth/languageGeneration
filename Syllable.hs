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
getConsMappings VlBilabialPlosive             = ["p"] -- p
getConsMappings VdBilabialPlosive             = ["b"] -- b
getConsMappings VdBilabialNasal               = ["m"] -- m
-- getConsMappings VdBilabialTrill               = ["<consonant-ex>"] -- ʙ
-- getConsMappings VlBilabialFricative           = ["<consonant-ex>"] -- ɸ
-- getConsMappings VdBilabialFricative           = ["<consonant-ex>"] -- β
getConsMappings VdLabiodentalNasal            = ["m"] -- ɱ
-- getConsMappings VdLabiodentalFlap             = ["<consonant-ex>"] -- ⱱ
getConsMappings VlLabiodentalFricative        = ["f","ph"] -- f
getConsMappings VdLabiodentalFricative        = ["v"] -- v
-- getConsMappings VdLabiodentalApproximant      = ["<consonant-ex>"] -- ʋ
getConsMappings VlAlveolarPlosive             = ["t"] -- as in 'stay' -- t
getConsMappings VdAlveolarPlosive             = ["d"] -- d
getConsMappings VdAlveolarNasal               = ["n"] -- n
getConsMappings VdAlveolarTrill               = ["r"] -- "rolling r" -- r
-- getConsMappings VdAlveolarTap                 = ["<consonant-ex>"] -- ɾ
getConsMappings VlDentalFricative             = ["th"] -- as in 'thing' -- θ
getConsMappings VdDentalFricative             = ["th","ḍ"] -- as in 'that' -- ð
getConsMappings VlAlveolarFricative           = ["s","c", "sc"] -- s
getConsMappings VdAlveolarFricative           = ["z","x"] -- z
getConsMappings VlPostalveolarFricative       = ["sh","š","x"] -- as in 'sharp' -- ʃ
getConsMappings VdPostalveolarFricative       = ["sh","g","s","j","zh","ž","ż","x"] -- as in 'pleasure' -- ʒ
-- getConsMappings VlLateralAlveolarFricative    = ["<consonant-ex>"] -- ɬ
-- getConsMappings VdLateralAlveolarFricative    = ["<consonant-ex>"] -- ɮ
getConsMappings VdAlveolarApproximant         = ["r"] -- as in 'red' -- ɹ
getConsMappings VdLateralAlveolarApproximant  = ["l"] -- l
-- getConsMappings VlRetroflexPlosive            = ["<consonant-ex>"] -- ʈ
-- getConsMappings VdRetroflexPlosive            = ["<consonant-ex>"] -- ɖ
-- getConsMappings VdRetroflexNasal              = ["<consonant-ex>"] -- ɳ
-- getConsMappings VdRetroflexFlap               = ["<consonant-ex>"] -- ɽ
-- getConsMappings VlRetroflexFricative          = ["<consonant-ex>"] -- ʂ
-- getConsMappings VdRetroflexFricative          = ["<consonant-ex>"] -- ʐ
-- getConsMappings VdRetroflexApproximant        = ["<consonant-ex>"] -- ɻ
-- getConsMappings VdRetroflexLateralApproximant = ["<consonant-ex>"] -- ɭ
-- getConsMappings VlPalatalPlosive              = ["<consonant-ex>"] -- c
-- getConsMappings VdPalatalPlosive              = ["<consonant-ex>"] -- ɟ
-- getConsMappings VdPalatalNasal                = ["<consonant-ex>"] -- ɲ
-- getConsMappings VlPalatalFricative            = ["<consonant-ex>"] -- ç
-- getConsMappings VdPalatalFricative            = ["<consonant-ex>"] -- ʝ
getConsMappings VdPalatalApproximant          = ["y","j"] -- as in 'yes' -- j
-- getConsMappings VdPalatalLateralApproximant   = ["<consonant-ex>"] -- ʎ
getConsMappings VlVelarPlosive                = ["k","c","ck","ch","ç","q"] -- as in 'keep' -- k
getConsMappings VdVelarPlosive                = ["g"] -- as in 'go' -- g
getConsMappings VdVelarNasal                  = ["ng","n","g"] -- as in 'singing' -- ŋ
getConsMappings VlVelarFricative              = ["ch","k","c","ḳ","q"] -- as in 'loch' -- x
-- getConsMappings VdVelarFricative              = ["<consonant-ex>"] -- ɣ
-- getConsMappings VdVelarApproximant            = ["<consonant-ex>"] -- ɰ
-- getConsMappings VdVelarLateralApproximant     = ["<consonant-ex>"] -- ʟ
-- getConsMappings VlUvularPlosive               = ["<consonant-ex>"] -- q
-- getConsMappings VdUvularPlosive               = ["<consonant-ex>"] -- ɢ
-- getConsMappings VdUvularNasal                 = ["<consonant-ex>"] -- ɴ
-- getConsMappings VdUvularTrill                 = ["<consonant-ex>"] -- ʀ
-- getConsMappings VlUvularFricative             = ["<consonant-ex>"] -- χ
-- getConsMappings VdUvularFricative             = ["<consonant-ex>"] -- ʁ
-- getConsMappings VlPharyngealFricative         = ["<consonant-ex>"] -- ħ
-- getConsMappings VdPharyngealFricative         = ["<consonant-ex>"] -- ʕ
-- getConsMappings VlGlottalPlosive              = ["<consonant-ex>"] -- ʔ
getConsMappings VlGlottalFricative            = ["h","j"] -- as in 'hear' -- h
-- getConsMappings VdGlottalFricative            = ["<consonant-ex>"] -- ɦ

getVowelMappings :: Vowel -> [[Char]]
getVowelMappings CloseFrontUnrounded        = ["i","e","y"] -- as in 'see' -- i
-- getVowelMappings CloseFrontRounded          = ["<vowel-ex>"] -- y
getVowelMappings LoweredCloseFrontUnrounded = ["i","e","y"] -- as in 'pit' -- ɪ
-- getVowelMappings LoweredCloseFrontRounded   = ["<vowel-ex>"] -- ʏ
-- getVowelMappings CloseMidFrontUnrounded     = ["<vowel-ex>"] -- e
-- getVowelMappings CloseMidFrontRounded       = ["<vowel-ex>"] -- ø
getVowelMappings OpenMidFrontUnrounded      = ["e"] -- as in 'get' -- ɛ
-- getVowelMappings OpenMidFrontRounded        = ["<vowel-ex>"] -- œ
getVowelMappings RaisedOpenFrontUnrounded   = ["a"] -- as in 'cat' -- æ
-- getVowelMappings OpenFrontUnrounded         = ["<vowel-ex>"] -- a
-- getVowelMappings OpenFrontRounded           = ["<vowel-ex>"] -- ɶ
-- getVowelMappings CloseCentralUnrounded      = ["<vowel-ex>"] -- ɨ
-- getVowelMappings CloseCentralRounded        = ["<vowel-ex>"] -- ʉ
-- getVowelMappings CloseMidCentralUnrounded   = ["<vowel-ex>"] -- ɘ
-- getVowelMappings CloseMidCentralRounded     = ["<vowel-ex>"] -- ɵ
getVowelMappings MidCentralUnrounded        = ["a","u","uh","o"] -- as in 'a' or 'uh' -- ə
-- getVowelMappings OpenMidCentralUnrounded    = ["<vowel-ex>"] -- ɜ
-- getVowelMappings OpenMidCentralRounded      = ["<vowel-ex>"] -- ɞ
-- getVowelMappings RaisedOpenCentralUnrounded = ["<vowel-ex>"] -- ɐ
-- getVowelMappings CloseBackUnrounded         = ["<vowel-ex>"] -- ɯ
getVowelMappings CloseBackRounded           = ["u","ou","ew"] -- as in 'you' -- u
getVowelMappings LoweredCloseBackRounded    = ["u"] -- as in 'put' -- ʊ
-- getVowelMappings CloseMidBackUnrounded      = ["<vowel-ex>"] -- ɤ
-- getVowelMappings CloseMidBackRounded        = ["<vowel-ex>"] -- o
getVowelMappings OpenMidBackUnrounded       = ["u","o"] -- as in 'but' -- ʌ
getVowelMappings OpenMidBackRounded         = ["a","o","au"] -- as in 'dawn' -- ɔ
-- getVowelMappings OpenBackUnrounded          = ["<vowel-ex>"] -- ɑ
getVowelMappings OpenBackRounded            = ["o","a"] -- as in (British) 'pot' -- ɒ

data Syllable = Syllable Consonant Vowel Consonant
instance Show Syllable where
    show (Syllable c1 v c2) = show c1 ++ show v ++ show c2
