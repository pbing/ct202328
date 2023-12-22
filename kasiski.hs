import Data.Char
import Data.List

-- calculate the encryption key with the length n from the cipher text
uncrypt :: Int -> String -> String
uncrypt n = map (cipherChar . findMostFrequentChar) . sliceHorizontal n

-- stolen from utility-ht-0.0.17: Various small helper functions for Lists, Maybes, Tuples, Functions
sliceHorizontal :: Int -> String -> [String]
sliceHorizontal n = map (sieve n) . take n . iterate (drop 1)
  where
    sieve :: Int -> [a] -> [a]
    sieve k = unfoldr (\xs -> toMaybe (not (null xs)) (head xs, drop k xs))

    toMaybe :: Bool -> a -> Maybe a
    toMaybe False _ = Nothing
    toMaybe True  x = Just x

-- find the most frequent character
findMostFrequentChar :: String -> Char
findMostFrequentChar = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

-- Assuming the letter 'E' as to most frequent character in this alphabet
-- calculate the cipher character assuming its position relative to the character 'E'
cipherChar :: Char -> Char
cipherChar c = let x = ord c - ord 'E'
               in if x < 0 then chr (ord 'A' + x + 26) else chr (ord 'A' + x)

-- print an autocorrelation histogram in order to detect the length of the encryption key
printHistogram :: [Int] -> IO ()
printHistogram = putStrLn . unlines . map (`replicate` '*')

autocorrelate :: String -> [Int]
autocorrelate xs = map go [0 .. length xs - 1]
  where
    go n = sum $ map fromEnum $ zipWith (==) xs (rotate n xs)

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) $ drop n $ cycle xs

{-
plain :: String
plain = filter isAlpha $ map toUpper $ concat
  ["Man verabredet mit demjenigen, mit",
    "welchem man im Geheimen correspondieren",
    "will, fuer jeden Buchstaben eine besondere Chiffre und schreibt",
    "dann mit diesen Chiffren in der",
    "gewoehnlichen Art."]

cipher :: String
cipher = encode "UNIX" plain

plain = "MANVERABREDETMITDEMJENIGENMITWELCHEMMANIMGEHEIMENCORRESPONDIERENWILLFUERJEDENBUCHSTABENEINEBESONDERECHIFFREUNDSCHREIBTDANNMITDIESENCHIFFRENINDERGEWOEHNLICHENART"
cipher = "GNVSYEIYLRLBNZQQXRUGYAQDYAUFNJMIWUMJGNVFGTMEYVUBHPWOLRAMIALFYEMKQVTIZHMODRLBHOCZBFBXVRVBCAMYYFWKXRZBWUQCZEMRHQAZBEMFVGLXHAUFNQQBMRVZBVNCLRVFHQMOARELYUVICPPBHNZQ"

printHistogram $ drop 1 $ autocorrelate cipher

uncrypt 4 cipher
"UNIX"
-}
