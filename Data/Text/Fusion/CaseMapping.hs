{-# LANGUAGE Rank2Types #-}
module Data.Text.Fusion.CaseMapping where
import Data.Char
import Data.Text.Fusion.Internal

upperMapping :: forall s. Char -> s -> Step (PairS (PairS s Char) Char) Char
{-# INLINE upperMapping #-}
-- LATIN SMALL LETTER SHARP S
upperMapping '\x00df' s = Yield '\x0053' (s :!: '\x0053' :!: '\x0000')
-- LATIN SMALL LIGATURE FF
upperMapping '\xfb00' s = Yield '\x0046' (s :!: '\x0046' :!: '\x0000')
-- LATIN SMALL LIGATURE FI
upperMapping '\xfb01' s = Yield '\x0046' (s :!: '\x0049' :!: '\x0000')
-- LATIN SMALL LIGATURE FL
upperMapping '\xfb02' s = Yield '\x0046' (s :!: '\x004c' :!: '\x0000')
-- LATIN SMALL LIGATURE FFI
upperMapping '\xfb03' s = Yield '\x0046' (s :!: '\x0046' :!: '\x0049')
-- LATIN SMALL LIGATURE FFL
upperMapping '\xfb04' s = Yield '\x0046' (s :!: '\x0046' :!: '\x004c')
-- LATIN SMALL LIGATURE LONG S T
upperMapping '\xfb05' s = Yield '\x0053' (s :!: '\x0054' :!: '\x0000')
-- LATIN SMALL LIGATURE ST
upperMapping '\xfb06' s = Yield '\x0053' (s :!: '\x0054' :!: '\x0000')
-- ARMENIAN SMALL LIGATURE ECH YIWN
upperMapping '\x0587' s = Yield '\x0535' (s :!: '\x0552' :!: '\x0000')
-- ARMENIAN SMALL LIGATURE MEN NOW
upperMapping '\xfb13' s = Yield '\x0544' (s :!: '\x0546' :!: '\x0000')
-- ARMENIAN SMALL LIGATURE MEN ECH
upperMapping '\xfb14' s = Yield '\x0544' (s :!: '\x0535' :!: '\x0000')
-- ARMENIAN SMALL LIGATURE MEN INI
upperMapping '\xfb15' s = Yield '\x0544' (s :!: '\x053b' :!: '\x0000')
-- ARMENIAN SMALL LIGATURE VEW NOW
upperMapping '\xfb16' s = Yield '\x054e' (s :!: '\x0546' :!: '\x0000')
-- ARMENIAN SMALL LIGATURE MEN XEH
upperMapping '\xfb17' s = Yield '\x0544' (s :!: '\x053d' :!: '\x0000')
-- LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
upperMapping '\x0149' s = Yield '\x02bc' (s :!: '\x004e' :!: '\x0000')
-- GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
upperMapping '\x0390' s = Yield '\x0399' (s :!: '\x0308' :!: '\x0301')
-- GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
upperMapping '\x03b0' s = Yield '\x03a5' (s :!: '\x0308' :!: '\x0301')
-- LATIN SMALL LETTER J WITH CARON
upperMapping '\x01f0' s = Yield '\x004a' (s :!: '\x030c' :!: '\x0000')
-- LATIN SMALL LETTER H WITH LINE BELOW
upperMapping '\x1e96' s = Yield '\x0048' (s :!: '\x0331' :!: '\x0000')
-- LATIN SMALL LETTER T WITH DIAERESIS
upperMapping '\x1e97' s = Yield '\x0054' (s :!: '\x0308' :!: '\x0000')
-- LATIN SMALL LETTER W WITH RING ABOVE
upperMapping '\x1e98' s = Yield '\x0057' (s :!: '\x030a' :!: '\x0000')
-- LATIN SMALL LETTER Y WITH RING ABOVE
upperMapping '\x1e99' s = Yield '\x0059' (s :!: '\x030a' :!: '\x0000')
-- LATIN SMALL LETTER A WITH RIGHT HALF RING
upperMapping '\x1e9a' s = Yield '\x0041' (s :!: '\x02be' :!: '\x0000')
-- GREEK SMALL LETTER UPSILON WITH PSILI
upperMapping '\x1f50' s = Yield '\x03a5' (s :!: '\x0313' :!: '\x0000')
-- GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
upperMapping '\x1f52' s = Yield '\x03a5' (s :!: '\x0313' :!: '\x0300')
-- GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
upperMapping '\x1f54' s = Yield '\x03a5' (s :!: '\x0313' :!: '\x0301')
-- GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
upperMapping '\x1f56' s = Yield '\x03a5' (s :!: '\x0313' :!: '\x0342')
-- GREEK SMALL LETTER ALPHA WITH PERISPOMENI
upperMapping '\x1fb6' s = Yield '\x0391' (s :!: '\x0342' :!: '\x0000')
-- GREEK SMALL LETTER ETA WITH PERISPOMENI
upperMapping '\x1fc6' s = Yield '\x0397' (s :!: '\x0342' :!: '\x0000')
-- GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
upperMapping '\x1fd2' s = Yield '\x0399' (s :!: '\x0308' :!: '\x0300')
-- GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
upperMapping '\x1fd3' s = Yield '\x0399' (s :!: '\x0308' :!: '\x0301')
-- GREEK SMALL LETTER IOTA WITH PERISPOMENI
upperMapping '\x1fd6' s = Yield '\x0399' (s :!: '\x0342' :!: '\x0000')
-- GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
upperMapping '\x1fd7' s = Yield '\x0399' (s :!: '\x0308' :!: '\x0342')
-- GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
upperMapping '\x1fe2' s = Yield '\x03a5' (s :!: '\x0308' :!: '\x0300')
-- GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
upperMapping '\x1fe3' s = Yield '\x03a5' (s :!: '\x0308' :!: '\x0301')
-- GREEK SMALL LETTER RHO WITH PSILI
upperMapping '\x1fe4' s = Yield '\x03a1' (s :!: '\x0313' :!: '\x0000')
-- GREEK SMALL LETTER UPSILON WITH PERISPOMENI
upperMapping '\x1fe6' s = Yield '\x03a5' (s :!: '\x0342' :!: '\x0000')
-- GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
upperMapping '\x1fe7' s = Yield '\x03a5' (s :!: '\x0308' :!: '\x0342')
-- GREEK SMALL LETTER OMEGA WITH PERISPOMENI
upperMapping '\x1ff6' s = Yield '\x03a9' (s :!: '\x0342' :!: '\x0000')
-- GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
upperMapping '\x1f80' s = Yield '\x1f08' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
upperMapping '\x1f81' s = Yield '\x1f09' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
upperMapping '\x1f82' s = Yield '\x1f0a' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
upperMapping '\x1f83' s = Yield '\x1f0b' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
upperMapping '\x1f84' s = Yield '\x1f0c' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
upperMapping '\x1f85' s = Yield '\x1f0d' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
upperMapping '\x1f86' s = Yield '\x1f0e' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
upperMapping '\x1f87' s = Yield '\x1f0f' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
upperMapping '\x1f88' s = Yield '\x1f08' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
upperMapping '\x1f89' s = Yield '\x1f09' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
upperMapping '\x1f8a' s = Yield '\x1f0a' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
upperMapping '\x1f8b' s = Yield '\x1f0b' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
upperMapping '\x1f8c' s = Yield '\x1f0c' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
upperMapping '\x1f8d' s = Yield '\x1f0d' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
upperMapping '\x1f8e' s = Yield '\x1f0e' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
upperMapping '\x1f8f' s = Yield '\x1f0f' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
upperMapping '\x1f90' s = Yield '\x1f28' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
upperMapping '\x1f91' s = Yield '\x1f29' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
upperMapping '\x1f92' s = Yield '\x1f2a' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
upperMapping '\x1f93' s = Yield '\x1f2b' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
upperMapping '\x1f94' s = Yield '\x1f2c' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
upperMapping '\x1f95' s = Yield '\x1f2d' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
upperMapping '\x1f96' s = Yield '\x1f2e' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
upperMapping '\x1f97' s = Yield '\x1f2f' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
upperMapping '\x1f98' s = Yield '\x1f28' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
upperMapping '\x1f99' s = Yield '\x1f29' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
upperMapping '\x1f9a' s = Yield '\x1f2a' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
upperMapping '\x1f9b' s = Yield '\x1f2b' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
upperMapping '\x1f9c' s = Yield '\x1f2c' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
upperMapping '\x1f9d' s = Yield '\x1f2d' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
upperMapping '\x1f9e' s = Yield '\x1f2e' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
upperMapping '\x1f9f' s = Yield '\x1f2f' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
upperMapping '\x1fa0' s = Yield '\x1f68' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
upperMapping '\x1fa1' s = Yield '\x1f69' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
upperMapping '\x1fa2' s = Yield '\x1f6a' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
upperMapping '\x1fa3' s = Yield '\x1f6b' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
upperMapping '\x1fa4' s = Yield '\x1f6c' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
upperMapping '\x1fa5' s = Yield '\x1f6d' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
upperMapping '\x1fa6' s = Yield '\x1f6e' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
upperMapping '\x1fa7' s = Yield '\x1f6f' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
upperMapping '\x1fa8' s = Yield '\x1f68' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
upperMapping '\x1fa9' s = Yield '\x1f69' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
upperMapping '\x1faa' s = Yield '\x1f6a' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
upperMapping '\x1fab' s = Yield '\x1f6b' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
upperMapping '\x1fac' s = Yield '\x1f6c' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
upperMapping '\x1fad' s = Yield '\x1f6d' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
upperMapping '\x1fae' s = Yield '\x1f6e' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
upperMapping '\x1faf' s = Yield '\x1f6f' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
upperMapping '\x1fb3' s = Yield '\x0391' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
upperMapping '\x1fbc' s = Yield '\x0391' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
upperMapping '\x1fc3' s = Yield '\x0397' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
upperMapping '\x1fcc' s = Yield '\x0397' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
upperMapping '\x1ff3' s = Yield '\x03a9' (s :!: '\x0399' :!: '\x0000')
-- GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
upperMapping '\x1ffc' s = Yield '\x03a9' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
upperMapping '\x1fb2' s = Yield '\x1fba' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
upperMapping '\x1fb4' s = Yield '\x0386' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
upperMapping '\x1fc2' s = Yield '\x1fca' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
upperMapping '\x1fc4' s = Yield '\x0389' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
upperMapping '\x1ff2' s = Yield '\x1ffa' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
upperMapping '\x1ff4' s = Yield '\x038f' (s :!: '\x0399' :!: '\x0000')
-- GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
upperMapping '\x1fb7' s = Yield '\x0391' (s :!: '\x0342' :!: '\x0399')
-- GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
upperMapping '\x1fc7' s = Yield '\x0397' (s :!: '\x0342' :!: '\x0399')
-- GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
upperMapping '\x1ff7' s = Yield '\x03a9' (s :!: '\x0342' :!: '\x0399')
upperMapping c s = Yield (toUpper c) (s :!: '\0' :!: '\0')
