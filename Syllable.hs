module Syllable where

-- Taken from IPA Pulmonic Consonants
data Consonant = VlBilabialPlosive                                -- p
               | VdBilabialPlosive                                -- b
               | VdBilabialNasal                                  -- m
               | VdLabiodentalNasal                               -- ɱ
               | VlLabiodentalFricative                           -- f
               | VdLabiodentalFricative                           -- v
               | VlAlveolarPlosive            -- as in 'stay'     -- t
               | VdAlveolarPlosive                                -- d
               | VdAlveolarNasal                                  -- n
               | VdAlveolarTrill              -- "rolling r"      -- r
               | VlDentalFricative            -- as in 'thing'    -- θ
               | VdDentalFricative            -- as in 'that'     -- ð
               | VlAlveolarFricative                              -- s
               | VdAlveolarFricative                              -- z
               | VlPostalveolarFricative      -- as in 'sharp'    -- ʃ
               | VdPostalveolarFricative      -- as in 'pleasure' -- ʒ
               | VdAlveolarApproximant        -- as in 'red'      -- ɹ
               | VdLateralAlveolarApproximant                     -- l
               | VdPalatalApproximant         -- as in 'yes'      -- j
               | VlVelarPlosive               -- as in 'keep'     -- k
               | VdVelarPlosive               -- as in 'go'       -- g
               | VdVelarNasal                 -- as in 'singing'  -- ŋ
               | VlVelarFricative             -- as in 'loch'     -- x
               | VlGlottalFricative           -- as in 'hear'     -- h
               deriving (Enum, Bounded)

getConsMappings :: Consonant -> [[Char]]
getConsMappings VlBilabialPlosive            = 
    ["p"]                                                   -- p
getConsMappings VdBilabialPlosive            = 
    ["b"]                                                   -- b
getConsMappings VdBilabialNasal              = 
    ["m"]                                                   -- m
getConsMappings VdLabiodentalNasal           = 
    ["m"]                                                   -- ɱ
getConsMappings VlLabiodentalFricative       = 
    ["f","ph"]                                              -- f
getConsMappings VdLabiodentalFricative       = 
    ["v"]                                                   -- v
getConsMappings VlAlveolarPlosive            = 
    ["t"]                               -- as in 'stay'     -- t
getConsMappings VdAlveolarPlosive            = 
    ["d"]                                                   -- d
getConsMappings VdAlveolarNasal              = 
    ["n"]                                                   -- n
getConsMappings VdAlveolarTrill              = 
    ["r"]                               -- "rolling r"      -- r
getConsMappings VlDentalFricative            = 
    ["th"]                              -- as in 'thing'    -- θ
getConsMappings VdDentalFricative            = 
    ["th","ḍ"]                          -- as in 'that'     -- ð
getConsMappings VlAlveolarFricative          = 
    ["s","c","sc"]                                          -- s
getConsMappings VdAlveolarFricative          = 
    ["z","x"]                                               -- z
getConsMappings VlPostalveolarFricative      = 
    ["sh","š","x"]                      -- as in 'sharp'    -- ʃ
getConsMappings VdPostalveolarFricative      = 
    ["sh","g","s","j","zh","ž","ż","x"] -- as in 'pleasure' -- ʒ
getConsMappings VdAlveolarApproximant        = 
    ["r"]                               -- as in 'red'      -- ɹ
getConsMappings VdLateralAlveolarApproximant = 
    ["l"]                                                   -- l
getConsMappings VdPalatalApproximant         = 
    ["y","j"]                           -- as in 'yes'      -- j
getConsMappings VlVelarPlosive               = 
    ["k","c","ck","ch","ç","q"]         -- as in 'keep'     -- k
getConsMappings VdVelarPlosive               = 
    ["g"]                               -- as in 'go'       -- g
getConsMappings VdVelarNasal                 = 
    ["ng","n","g"]                      -- as in 'singing'  -- ŋ
getConsMappings VlVelarFricative             = 
    ["ch","k","c","ḳ","q"]              -- as in 'loch'     -- x
getConsMappings VlGlottalFricative           = 
    ["h","j"]                           -- as in 'hear'     -- h
-- ignore these for now
-- getConsMappings VdBilabialTrill               = ["<consonant-ex>"] -- ʙ
-- getConsMappings VlBilabialFricative           = ["<consonant-ex>"] -- ɸ
-- getConsMappings VdBilabialFricative           = ["<consonant-ex>"] -- β
-- getConsMappings VdLabiodentalFlap             = ["<consonant-ex>"] -- ⱱ
-- getConsMappings VdLabiodentalApproximant      = ["<consonant-ex>"] -- ʋ
-- getConsMappings VdAlveolarTap                 = ["<consonant-ex>"] -- ɾ
-- getConsMappings VlLateralAlveolarFricative    = ["<consonant-ex>"] -- ɬ
-- getConsMappings VdLateralAlveolarFricative    = ["<consonant-ex>"] -- ɮ
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
-- getConsMappings VdPalatalLateralApproximant   = ["<consonant-ex>"] -- ʎ
-- getConsMappings VdGlottalFricative            = ["<consonant-ex>"] -- ɦ


-- IPA Vowels
data Vowel = CloseFrontUnrounded        -- as in 'see'           -- i
           | LoweredCloseFrontUnrounded -- as in 'pit'           -- ɪ
           | OpenMidFrontUnrounded      -- as in 'get'           -- ɛ
           | RaisedOpenFrontUnrounded   -- as in 'cat'           -- æ
           | MidCentralUnrounded        -- as in 'a' or 'uh'     -- ə
           | CloseBackRounded           -- as in 'you'           -- u
           | LoweredCloseBackRounded    -- as in 'put'           -- ʊ
           | OpenMidBackUnrounded       -- as in 'but'           -- ʌ
           | OpenMidBackRounded         -- as in 'dawn'          -- ɔ
           | OpenBackRounded            -- as in (British) 'pot' -- ɒ
           deriving (Enum, Bounded)

getVowelMappings :: Vowel -> [[Char]]
getVowelMappings CloseFrontUnrounded        = 
    ["i","e","y"]      -- as in 'see'           -- i
getVowelMappings LoweredCloseFrontUnrounded = 
    ["i","e","y"]      -- as in 'pit'           -- ɪ
getVowelMappings OpenMidFrontUnrounded      = 
    ["e"]              -- as in 'get'           -- ɛ
getVowelMappings RaisedOpenFrontUnrounded   = 
    ["a"]              -- as in 'cat'           -- æ
getVowelMappings MidCentralUnrounded        = 
    ["a","u","uh","o"] -- as in 'a' or 'uh'     -- ə
getVowelMappings CloseBackRounded           = 
    ["u","ou","ew"]    -- as in 'you'           -- u
getVowelMappings LoweredCloseBackRounded    = 
    ["u"]              -- as in 'put'           -- ʊ
getVowelMappings OpenMidBackUnrounded       = 
    ["u","o"]          -- as in 'but'           -- ʌ
getVowelMappings OpenMidBackRounded         = 
    ["a","o","au"]     -- as in 'dawn'          -- ɔ
getVowelMappings OpenBackRounded            = 
    ["o","a"]          -- as in (British) 'pot' -- ɒ
-- ignore these for now
-- getVowelMappings CloseFrontRounded          = ["<vowel-ex>"] -- y
-- getVowelMappings LoweredCloseFrontRounded   = ["<vowel-ex>"] -- ʏ
-- getVowelMappings CloseMidFrontUnrounded     = ["<vowel-ex>"] -- e
-- getVowelMappings CloseMidFrontRounded       = ["<vowel-ex>"] -- ø
-- getVowelMappings OpenMidFrontRounded        = ["<vowel-ex>"] -- œ
-- getVowelMappings OpenFrontUnrounded         = ["<vowel-ex>"] -- a
-- getVowelMappings OpenFrontRounded           = ["<vowel-ex>"] -- ɶ
-- getVowelMappings CloseCentralUnrounded      = ["<vowel-ex>"] -- ɨ
-- getVowelMappings CloseCentralRounded        = ["<vowel-ex>"] -- ʉ
-- getVowelMappings CloseMidCentralUnrounded   = ["<vowel-ex>"] -- ɘ
-- getVowelMappings CloseMidCentralRounded     = ["<vowel-ex>"] -- ɵ
-- getVowelMappings OpenMidCentralUnrounded    = ["<vowel-ex>"] -- ɜ
-- getVowelMappings OpenMidCentralRounded      = ["<vowel-ex>"] -- ɞ
-- getVowelMappings RaisedOpenCentralUnrounded = ["<vowel-ex>"] -- ɐ
-- getVowelMappings CloseBackUnrounded         = ["<vowel-ex>"] -- ɯ
-- getVowelMappings CloseMidBackUnrounded      = ["<vowel-ex>"] -- ɤ
-- getVowelMappings CloseMidBackRounded        = ["<vowel-ex>"] -- o
-- getVowelMappings OpenBackUnrounded          = ["<vowel-ex>"] -- ɑ

data Phoneme = ConsonantPhoneme Consonant
             | VowelPhoneme Vowel

getPotentialRomanizations :: Phoneme -> [[Char]]
getPotentialRomanizations (ConsonantPhoneme c) = getConsMappings c
getPotentialRomanizations (VowelPhoneme v)     = getVowelMappings v

data Syllable = Syllable Consonant Vowel Consonant
instance Show Syllable where
    show (Syllable c1 v c2) = show c1 ++ show v ++ show c2
