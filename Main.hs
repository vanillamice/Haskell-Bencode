module Main where
import System.IO
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Text.ParserCombinators.Parsec
import Data.List (takeWhile)


startsWithi :: String -> Bool
startsWithi str = isPrefixOf "i" str

startsWithN :: String -> Bool
startsWithN str = isDigit (head str)

startsWithI :: String -> Bool
startsWithI str = isPrefixOf "I" str

startsWithD :: String -> Bool 
startsWithD str = isPrefixOf "d" str

decodeLine:: String -> String
decodeLine enLine
  |   startsWithi enLine = (init (tail enLine)) --case1:Decoding Integers
  |   startsWithN enLine = (tail (tail enLine))--case2:Decoding Strings
  |   startsWithI enLine = "[" ++ tail(processList (init (tail enLine))) ++ "]"--case3:Decoding lists
  |   otherwise =  processDict (init (tail enLine)) 

encodeLine :: String -> String
encodeLine linesOfFile
  | isNumericString linesOfFile =  "i" ++ linesOfFile ++ "e" --case1: encoding Integers
  | startsWithBracket linesOfFile = "I" ++ processSlices (init (tail linesOfFile)) ++ "e" --case2:encoding lists
  | startsWithCurly linesOfFile =  "d" ++ processCurlySlices(init (tail linesOfFile)) ++ "e"--case3:encoding dictionaries
  | otherwise = show  (length linesOfFile) ++ ":" ++ linesOfFile --case4:encoding Strings


breakCondition :: Char -> Bool
breakCondition c = c == ':' || c == ','

processDict :: String -> String
processDict [] = []
processDict [x] = [x]
processDict (x:y:rest)
  |not (isDigit x) && not (isDigit y) = (x:y:processDict rest)
processDict (x:y:rest)
  |isDigit x && y == 'e' = x:processList rest
processDict (x:y:z:rest)
  | y == ':' = processDict(',':z:rest)
  | x == 'i' && isDigit y && z == 'e' = (y:',':processDict rest)
  | x == 'i' && isDigit y && isDigit z = (',':y:z:processDict rest)
  | isDigit x && isDigit y && isDigit z = (x:y:z:processDict rest)
  | isDigit x && isDigit y && z == 'e' = (x:y:processDict rest)
  | otherwise = (x : processDict (y:z:rest))


processList :: String -> String
processList [] = []
processList [x] = [x]
processList (x:y:rest)
  |not (isDigit x) && not (isDigit y) = (x:y:processList rest)
processList (x:y:rest)
  |isDigit x && y == 'e' =x:processList rest
processList (x:y:z:rest)
  | y == ':' = processList(',':z:rest)
  | x == 'i' && isDigit y && z == 'e' =(y:',':processList rest)
  | x == 'i' && isDigit y && isDigit z =(',':y:z:processList rest)
  | isDigit x && isDigit y && isDigit z = (x:y:z:processList rest)
  | isDigit x && isDigit y && z == 'e' = (x:y:processList rest)
  | otherwise =(x : processList (y:z:rest))



processCurlySlices :: String -> String
processCurlySlices "" = "" 
processCurlySlices str =
  let (slice, rest) = break breakCondition str
      processedCurlySlice = processCurlySlice slice 
  in processedCurlySlice ++ processCurlySlices (drop 1 rest)

processCurlySlice :: String -> String
processCurlySlice slice
    | isNumericString slice =  "i" ++ slice ++ "e"
    | otherwise = show  (length slice) ++ ":" ++ slice

startsWithCurly :: String -> Bool
startsWithCurly str = isPrefixOf "{" str

processSlices :: String -> String
processSlices "" = ""
processSlices str =
  let (slice, rest) = break (== ',') str 
      processedSlice = processSlice slice 
  in processedSlice ++ processSlices (drop 1 re٢st) 

processSlice :: String -> String
processSlice slice
    | isNumericString slice =  "i" ++ slice ++ "e"
    | otherwise = show (length slice) ++ ":" ++ slice


takey:: Char -> Bool
takey x = x /= ','

parser :: Parser String
parser = do
  char '"'
  content <- many anyChar
  char '"'
  return content

--unused
removeBracket :: String -> String
removeBracket str = init(tail str) 

startsWithBracket :: String -> Bool
startsWithBracket str = isPrefixOf "[" str

isNumericString :: String -> Bool
isNumericString [] = True
isNumericString (x:xs)
  | isDigit x = isNumericString xs
  | otherwise = False



main :: IO ()
main = do
    handle <- openFile "encoded.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFile = lines contents
    let processedLines = map encodeLine linesOfFile
    mapM_ putStrLn processedLines
    hClose handle