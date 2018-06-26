module Syllable where

import Data.Ix

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
               deriving (Enum, Bounded, Eq, Ord, Ix, Show)

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
--
-- VdBilabialTrill               -- ʙ
-- VlBilabialFricative           -- ɸ
-- VdBilabialFricative           -- β
-- VdLabiodentalFlap             -- ⱱ
-- VdLabiodentalApproximant      -- ʋ
-- VdAlveolarTap                 -- ɾ
-- VlLateralAlveolarFricative    -- ɬ
-- VdLateralAlveolarFricative    -- ɮ
-- VlRetroflexPlosive            -- ʈ
-- VdRetroflexPlosive            -- ɖ
-- VdRetroflexNasal              -- ɳ
-- VdRetroflexFlap               -- ɽ
-- VlRetroflexFricative          -- ʂ
-- VdRetroflexFricative          -- ʐ
-- VdRetroflexApproximant        -- ɻ
-- VdRetroflexLateralApproximant -- ɭ
-- VlPalatalPlosive              -- c
-- VdPalatalPlosive              -- ɟ
-- VdPalatalNasal                -- ɲ
-- VlPalatalFricative            -- ç
-- VdPalatalFricative            -- ʝ
-- VdVelarFricative              -- ɣ
-- VdVelarApproximant            -- ɰ
-- VdVelarLateralApproximant     -- ʟ
-- VlUvularPlosive               -- q
-- VdUvularPlosive               -- ɢ
-- VdUvularNasal                 -- ɴ
-- VdUvularTrill                 -- ʀ
-- VlUvularFricative             -- χ
-- VdUvularFricative             -- ʁ
-- VlPharyngealFricative         -- ħ
-- VdPharyngealFricative         -- ʕ
-- VlGlottalPlosive              -- ʔ
-- VdPalatalLateralApproximant   -- ʎ
-- VdGlottalFricative            -- ɦ


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
           deriving (Enum, Bounded, Eq, Ord, Ix, Show)

getVowelMappings :: Vowel -> [[Char]]
getVowelMappings CloseFrontUnrounded        = -- as in 'see'           -- i
    ["i","ī","í","ǐ","ì","î","e","ē","é","ě","è","ê","y"]      
getVowelMappings LoweredCloseFrontUnrounded = -- as in 'pit'           -- ɪ
    ["i","ī","í","ǐ","ì","î","e","ē","é","ě","è","ê","y"]      
getVowelMappings OpenMidFrontUnrounded      = -- as in 'get'           -- ɛ
    ["e","ē","é","ě","è","ê"]              
getVowelMappings RaisedOpenFrontUnrounded   = -- as in 'cat'           -- æ
    ["a","ā","ǎ","à","á","â"]              
getVowelMappings MidCentralUnrounded        = -- as in 'a' or 'uh'     -- ə
    ["a","ā","ǎ","à","á","â","u","ū","ǖ","ú","ǘ","ǔ","ǚ","ù","ǜ","û"
    ,"uh","o","ō","ó","ǒ","ò","ô","e","ē","é","ě","è","ê"] 
getVowelMappings CloseBackRounded           = -- as in 'you'           -- u
    ["u","ū","ǖ","ú","ǘ","ǔ","ǚ","ù","ǜ","û","ou","ew"]    
getVowelMappings LoweredCloseBackRounded    = -- as in 'put'           -- ʊ
    ["u","ū","ǖ","ú","ǘ","ǔ","ǚ","ù","ǜ","û","ou"]              
getVowelMappings OpenMidBackUnrounded       = -- as in 'but'           -- ʌ
    ["u","ū","ǖ","ú","ǘ","ǔ","ǚ","ù","ǜ","û","o","ō","ó","ǒ","ò","ô"]          
getVowelMappings OpenMidBackRounded         = -- as in 'dawn'          -- ɔ
    ["a","ā","ǎ","à","á","â","o","ō","ó","ǒ","ò","ô","au","ao"]     
getVowelMappings OpenBackRounded            = -- as in (British) 'pot' -- ɒ
    ["o","ō","ó","ǒ","ò","ô","a","ā","ǎ","à","á","â"]          

-- ignore these for now
--
-- CloseFrontRounded          -- y
-- LoweredCloseFrontRounded   -- ʏ
-- CloseMidFrontUnrounded     -- e
-- CloseMidFrontRounded       -- ø
-- OpenMidFrontRounded        -- œ
-- OpenFrontUnrounded         -- a
-- OpenFrontRounded           -- ɶ
-- CloseCentralUnrounded      -- ɨ
-- CloseCentralRounded        -- ʉ
-- CloseMidCentralUnrounded   -- ɘ
-- CloseMidCentralRounded     -- ɵ
-- OpenMidCentralUnrounded    -- ɜ
-- OpenMidCentralRounded      -- ɞ
-- RaisedOpenCentralUnrounded -- ɐ
-- CloseBackUnrounded         -- ɯ
-- CloseMidBackUnrounded      -- ɤ
-- CloseMidBackRounded        -- o
-- OpenBackUnrounded          -- ɑ

data Phoneme = ConsonantPhoneme Consonant
             | VowelPhoneme Vowel

getPotentialRomanizations :: Phoneme -> [[Char]]
getPotentialRomanizations (ConsonantPhoneme c) = getConsMappings c
getPotentialRomanizations (VowelPhoneme v)     = getVowelMappings v

data Syllable = Syllable Consonant Vowel Consonant 

instance Show Syllable where
    show (Syllable c1 v c2) = "("++show c1 ++" "++ show v ++" "++ show c2++")"
