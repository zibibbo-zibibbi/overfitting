import Data.List

lineBreak :: Int -> String -> String
lineBreak maxLen = joinedLines . wordJoinedLines . brokenLines maxLen . words

brokenLines :: Int -> [String] -> [[String]]
brokenLines maxLen []       = []
brokenLines maxLen wordList = brokenLine : brokenLines maxLen remWords
  where (brokenLine, remWords) = hyphenatedLine maxLen $ splitAt (brokenLineWordCount maxLen wordList) wordList

brokenLineWordCount :: Int -> [String] -> Int
brokenLineWordCount maxLen = length . takeWhile (<= maxLen) . scanl1 (\m n -> m + 1 + n) . map length

wordJoinedLines :: [[String]] -> [String]
wordJoinedLines = map (intercalate " ")

joinedLines :: [String] -> String
joinedLines = intercalate "\n"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

hyphenatedLine :: Int ->  ([String], [String]) -> ([String], [String])
hyphenatedLine maxLen (line', rest') = if isOk then (line', rest') else (line, rest)
  where
    lineLen   = length $ intercalate " " line'
    freeSpace = maxLen - lineLen
    isOk      = freeSpace < 5 || null rest'
    nextWord  = head rest'
    sylls     = syllables nextWord
    count'    = fittingStringsCount (+) (freeSpace - 2) sylls
    count     = if null line' && count' == 0 then 1 else count'
    left      = concat $ take count sylls
    right     = concat $ drop count sylls
    line      = line' ++ if left /= "" then [left ++ "-"] else []
    rest      = right : tail rest'

syllables :: String -> [String]
syllables word = if word /= "" then (take 3 word) : syllables (drop 3 word) else []

fittingStringsCount :: (Int -> Int -> Int) -> Int -> [String] -> Int
fittingStringsCount accLen maxLen = length . takeWhile (<= maxLen) . scanl1 accLen . map length

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

testcases = [
    ("aaaa bbbb cccdd",                             "aaaa bbbb\ncccdd"),
    ("aaaa bbb cccdd",                              "aaaa bbb\ncccdd"),
    ("aaa bbb cccdd",                               "aaa bbb ccc-\ndd"),
    ("aaa bb cccdd",                                "aaa bb cccdd"),
    ("aaa bb cccddd",                               "aaa bb ccc-\nddd"),
    ("aa bb cccdddee",                              "aa bb ccc-\ndddee"),
    ("a b cccdddeeeff",                             "a b cccddd-\neeeff"),
    ("aa b cccdddeeeff",                            "aa b cccddd-\neeeff"),
    ("aa bb cccdddeeeff",                           "aa bb ccc-\ndddeeeff"),
    ("aa bb cccdddeeeff ggghhh iiiii",              "aa bb ccc-\ndddeeeff\nggghhh iiiii"),
    ("aa b cccdddeeff ggghhhiiiii jjjkkk",          "aa b cccddd-\neeff ggghhh-\niiiii jjjkkk"),
    ("aa b cccdddeeff ggghhhiiii jjjkkklll mmm",    "aa b cccddd-\neeff ggghhh-\niiii jjjkkk-\nlll mmm"),
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaa",                 "aaaaaaaaa-\naaaaaaaaa-\naaaaaaaaa"),
    ("",                                            ""),
    ("  ",                                          "")
  ]

runTests = [(lineBreak 12 $ fst c) == snd c | c <- testcases] == take (length testcases) (repeat True)
