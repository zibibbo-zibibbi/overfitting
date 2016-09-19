import Data.List

lineBreak :: Int -> String -> String
lineBreak maxLen = joinedLines . wordJoinedLines . brokenLines maxLen . words

brokenLines :: Int -> [String] -> [[String]]
brokenLines maxLen []       = []
brokenLines maxLen wordList = brokenLine : brokenLines maxLen remWords
  where (brokenLine, remWords) = splitAt (brokenLineWordCount maxLen wordList) wordList

brokenLineWordCount :: Int -> [String] -> Int
brokenLineWordCount maxLen = length . takeWhile (<= maxLen) . scanl1 (\m n -> m + 1 + n) . map length

wordJoinedLines :: [[String]] -> [String]
wordJoinedLines = map (intercalate " ")

joinedLines :: [String] -> String
joinedLines = intercalate "\n"
