import Data.List

lineBreak :: Int -> String -> String
lineBreak maxLen = joinedLines . wordJoinedLines . brokenLines maxLen . words

brokenLines :: Int -> [String] -> [[String]]
brokenLines maxLen []       = []
brokenLines maxLen wordList = brokenLine : brokenLines maxLen remWords
  where
    (brokenLine, remWords)  = splitAt (brokenLineWordCount maxLen wordList) wordList

brokenLineWordCount :: Int -> [String] -> Int
brokenLineWordCount maxLen = fittingStringsCount (\m n -> m + 1 + n) maxLen

fittingStringsCount :: (Int -> Int -> Int) -> Int -> [String] -> Int
fittingStringsCount accLen maxLen = length . takeWhile (<= maxLen) . scanl1 accLen . map length

wordJoinedLines :: [[String]] -> [String]
wordJoinedLines = map (intercalate " ")

joinedLines :: [String] -> String
joinedLines = intercalate "\n"
