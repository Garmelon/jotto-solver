module Main where

import           Control.Monad
import           Data.List
import           Data.Char
import           System.Environment
import           System.Exit

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Text                        as T
import qualified System.Console.Haskeline         as H

import           Jotto

rjust :: Int -> Char -> String -> String
rjust n c = T.unpack . T.justifyRight n c . T.pack

numberjust :: String -> String
numberjust = rjust 9 ' '

type Console = H.InputT (StateT GuessState IO)

cmdQuit :: Console ()
cmdQuit = H.outputStrLn "Goodbye."

cmdHelp :: Console ()
cmdHelp = do
  H.outputStrLn "help                  - show this help                   (alias: h, ?)"
  H.outputStrLn "quit                  - close the program                (alias: q)"
  H.outputStrLn "guess <word> <number> - add a guess                      (alias: g)"
  H.outputStrLn "unguess <word>        - remove a previous guess          (alias: u)"
  H.outputStrLn "possible              - show all words not yet ruled out (alias: p)"
  H.outputStrLn "reset                 - reset solve state                (alias: r)"
  H.outputStrLn "status                - show current solve state         (alias: s)"
  H.outputStrLn "what                  - what words to guess next         (alias: w)"
  H.outputStrLn ""

readMaybe :: Read a => String -> Maybe a
readMaybe s =
  case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

cmdGuess :: String -> String -> Console ()
cmdGuess w n = do
  let word = map toLower w
  case readMaybe n of
    Just number -> do
      lift $ modify $ removeGuess word
      lift $ modify $ addGuess (Guess word number)
      H.outputStrLn $ "Added guess: " ++ word ++ " - " ++ show number
    Nothing -> H.outputStrLn "Incorrect number format."
  H.outputStrLn ""

cmdUnguess :: String -> Console ()
cmdUnguess w = do
  let word = map toLower w
  lift $ modify $ removeGuess word
  H.outputStrLn $ "Removed guess: " ++ w
  H.outputStrLn ""

cmdPossible :: Console ()
cmdPossible = do
  g <- lift get
  let classes = sortOn length $ possible g
      showWords = intercalate ", "
  mapM_ (H.outputStrLn . showWords) classes
  H.outputStrLn ""

cmdReset :: Console ()
cmdReset = do
  lift $ modify resetGuesses
  H.outputStrLn "Reset all guesses."
  H.outputStrLn ""

cmdStats :: Console ()
cmdStats = do
  g <- lift get
  let cTotal = numberjust $ show $ classesTotal g
      wTotal = numberjust $ show $ wordsTotal g
      cLeft  = numberjust $ show $ classesLeft g
      wLeft  = numberjust $ show $ wordsLeft g
  H.outputStrLn "Using:"
  H.outputStrLn $ wTotal ++ " words"
  H.outputStrLn $ cTotal ++ " classes"
  H.outputStrLn "Possible: "
  H.outputStrLn $ wLeft ++ " words"
  H.outputStrLn $ cLeft ++ " classes"
  H.outputStrLn ""

  H.outputStrLn "Guesses:"
  let printGuess (Guess word n) = "  " ++ word ++ " - " ++ show n
  mapM_ (H.outputStrLn . printGuess) $ reverse $ guesses g
  H.outputStrLn ""

cmdWhat :: Console ()
cmdWhat = do
  g <- lift get
  let showWords = intercalate ", "
  case possible g of
    [ws] -> do
      let guesswords = map (\(Guess w _) -> w) $ filter (\(Guess w n) -> n == length w) $ guesses g
          possibleWords = ws \\ guesswords
      H.outputStrLn "The word is one of the following words:"
      H.outputStrLn $ "  " ++ showWords possibleWords
      H.outputStrLn "These words were already guessed:"
      H.outputStrLn $ "  " ++ showWords guesswords
    _ -> do
      let next = nextGuesses g
          showNextGuess (score, w) = numberjust (show score) ++ ": " ++ showWords w
      mapM_ (H.outputStrLn . showNextGuess) next
  H.outputStrLn ""

loop :: Console ()
loop = do
  line <- H.getInputLine "jotto> "
  case words <$> line of
    Nothing                 -> cmdQuit
    Just ["quit"]           -> cmdQuit
    Just ["q"]              -> cmdQuit
    Just ["help"]           -> cmdHelp >> loop
    Just ["h"]              -> cmdHelp >> loop
    Just ["?"]              -> cmdHelp >> loop
    Just ["guess", word, n] -> cmdGuess word n >> loop
    Just ["g",     word, n] -> cmdGuess word n >> loop
    Just ["unguess", word]  -> cmdUnguess word >> loop
    Just ["u"      , word]  -> cmdUnguess word >> loop
    Just ["possible"]       -> cmdPossible >> loop
    Just ["p"]              -> cmdPossible >> loop
    Just ["reset"]          -> cmdReset >> loop
    Just ["r"]              -> cmdReset >> loop
    Just ["status"]         -> cmdStats >> loop
    Just ["s"]              -> cmdStats >> loop
    Just ["what"]           -> cmdWhat >> loop
    Just ["w"]              -> cmdWhat >> loop
    Just []                 -> loop
    Just _                  -> H.outputStrLn "Command not recognized." >> H.outputStrLn "" >> loop

runLoop :: GuessState -> IO GuessState
runLoop = execStateT (H.runInputT H.defaultSettings loop)

loadAndRun :: [FilePath] -> IO ()
loadAndRun files = do
  wordlists <- mapM readFile files
  let wordlist = concatMap lines wordlists
      cmap = buildClassMap (Just 5) wordlist
      g = guessState cmap

  let wordCount = numberjust $ show $ length wordlist
  putStrLn "Found:"
  putStrLn $ wordCount ++ " words"
  putStrLn ""

  void $ runLoop g

{-

loadAndRun :: FilePath -> IO ()
loadAndRun file = do
  -- Read file
  content <- lines <$> readFile file
  let words' = map head . group . sort . map (map toLower) $ content
      wordNumber = show (length words')
  putStrLn $ "Found: " ++ rjust 9 ' ' wordNumber ++ " words"

  -- Load words
  let cmap = buildClassMap 5 words'
      wordLoadedNumber = show . sum . M.map length $ cmap
      putStrLn $ "Using: " ++ rjust 9 ' ' wordLoadedNumber ++ " words"

  -- Enter previous guesses
  -- Create GuessState
  -- Receive input
  --  - next -> calculate a few optimal guesses (alias: n)
  --  - guess word number -> add guess (alias: g)
  --  - remove word -> remove word from list of words (alias: rm)
  --  - restart -> reset GuessState back to initial word list (alias: r)
  --  - quit -> quit the program (alias: q)
-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      name <- getProgName
      putStrLn "  USAGE:"
      putStrLn $ name ++ " <dictionary>"
      die "Error: No dictionary given."
    files -> loadAndRun files
