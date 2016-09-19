import Data.List

lineBreak :: Int -> String -> String
lineBreak maxLen = joinedLines . wordJoinedLines . brokenLines maxLen . words

brokenLines :: Int -> [String] -> [[String]]
brokenLines maxLen []       = []
brokenLines maxLen wordList = brokenLine : brokenLines maxLen remWords
  where (brokenLine, remWords) = splitAt (max 1 $ brokenLineWordCount maxLen wordList) wordList

brokenLineWordCount :: Int -> [String] -> Int
brokenLineWordCount maxLen = length . takeWhile (<= maxLen) . scanl1 (\m n -> m + 2 * n) . map length

wordJoinedLines :: [[String]] -> [String]
wordJoinedLines = map $ foldl1 append
  where whitespace s  = take (length s) (repeat ' ')
        append s1 s2  = s1 ++ whitespace s2 ++ s2

joinedLines :: [String] -> String
joinedLines = intercalate "\n"
