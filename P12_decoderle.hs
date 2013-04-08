-- Problem 12: Decode the above encoding.
-- > decodeModified
--     [Multiple 4 'a',Single 'b',Multiple 2 'c',
--      Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

module P12_decoderle (decodeModified) where

import P11_modrle (RLEElement(..))

decodeModified :: [RLEElement a] -> [a]
decodeModified xs = decodeModified_ xs []

decodeModified_ :: [RLEElement a] -> [a] -> [a]
decodeModified_ [] acc = acc
decodeModified_ (x:xs) acc = decodeModified_ xs $ acc ++ decodeOne x
  where decodeOne (Multiple n el) = replicate n el
        decodeOne (Single el) = [el]
