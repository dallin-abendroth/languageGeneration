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

data Syllable = Syllable Consonant Vowel Consonant
instance Show Syllable where
    show (Syllable c1 v c2) = show c1 ++ show v ++ show c2
