{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
{-# HLINT ignore "Use bimap" #-}
import Data.Char
import System.IO
import Control.Exception

import LTypes
import qualified Lambek
import LExp

import Parser
import PySupport
import Data.Maybe (fromMaybe)
import Text.Read
import Diagrams.Prelude (E(el))

-- "standardize" the capitalization of a word by making the first
-- letter uppercase and the following letters lowercase.
-- (This will make it easier to look up words in the lexicon.)
stdize :: String -> String
stdize (c:cs) = toUpper c : map toLower cs

-- rename positional variables (x1, x2, ...)
prettyLExpWith :: (Var -> String) -> LExp -> String
prettyLExpWith rename = go
  where
    go (V x)      = rename x
    go (L x e)    = "(\\" ++ rename x ++ ". " ++ go e ++ ")"
    go (A f a)    = ppFun f ++ " " ++ ppArg a

    ppFun e@(A _ _) = "(" ++ go e ++ ")"
    ppFun e         = go e

    ppArg e@(L _ _) = "(" ++ go e ++ ")"
    ppArg e@(A _ _) = "(" ++ go e ++ ")"
    ppArg e         = go e

-- build a renaming function x1->Word_1, x2->Word_2, ...
mkRenamer :: [String] -> (Var -> String)
mkRenamer ws =
  let names    = [ "x" ++ show i | i <- [1..] ]
      display  = [ w ++ "_" ++ show i | (i,w) <- zip [1..] ws ]
      env      = zip names display
  in \x -> fromMaybe x (lookup x env)

-- Helper functions

add :: Eq a => a -> [a] -> [a]
add x xs = if x `elem` xs then xs else xs ++ [x]

extendLexicon :: Lexicon PythonCode -> [String] -> Lexicon PythonCode
extendLexicon lex newPeople =
  let existing = [w | (w, _, _) <- items lex]
      missing  = filter (`notElem` existing) newPeople
      newItems = [ (name, Atm "np", PCode name) | name <- missing ]
  in lex { items = items lex ++ newItems}

updateEnv :: Lexicon PythonCode -> Lambek.Env -> String -> [String] -> IO (Lexicon PythonCode, Lambek.Env)
updateEnv elx env@(h,c,s,p,l) key v =
  let vals = map stdize v in
  case map toLower key of
    "happy"    -> pure (extendLexicon elx vals, (vals, c, s, p, l))
    "confused" -> pure (extendLexicon elx vals, (h, vals, s, p, l))
    "sad"      -> pure (extendLexicon elx vals, (h, c, vals, p, l))
    "people"   -> do
      let newH = filter (`elem` vals) h
          newC = filter (`elem` vals) c
          newS = filter (`elem` vals) s
          newL = filter (\(a,b) -> a `elem` vals && b `elem` vals) l
          newLex = elx { items = filter (\(w,_,_) -> w `elem` vals) (items (extendLexicon elx vals))}
      pure (newLex, (newH, newC, newS, vals, newL))
    "likes"    -> case readMaybe (unwords vals) of
                    Just ls -> pure (extendLexicon elx (concatMap (\(a,b) -> [a,b]) ls), (h, c, s, p, ls))
                    Nothing -> putStrLn "Invalid tuple. Use syntax like (\"Awen\",\"Agatha\")." >> pure (elx, env)
    _          -> putStrLn "Unknown field." >> pure (elx, env)

addItem :: Lexicon PythonCode -> Lambek.Env -> String -> [String] -> IO (Lexicon PythonCode, Lambek.Env)
addItem elx (h,c,s,p,l) key v =
  let val = map stdize v in
  case map toLower key of
    "happy"    -> pure (extendLexicon elx val, (foldr add h val, c, s, foldr add p val, l))
    "confused" -> pure (extendLexicon elx val, (h, foldr add c val, s, foldr add p val, l))
    "sad"      -> pure (extendLexicon elx val, (h, c, foldr add s val, foldr add p val, l))
    "people"   -> pure (extendLexicon elx val, (h, c, s, foldr add p val, l))
    "likes"    -> do
      let parsed = mapM (readMaybe :: String -> Maybe (String,String)) val
      case parsed of
        Just tuples -> 
          let stdTuples = map (\(a,b) -> (stdize a, stdize b)) tuples
              flat = concatMap (\(a,b) -> [a,b]) stdTuples
          in pure (extendLexicon elx flat, (h, c, s, foldr add p flat, foldr add l stdTuples))
        Nothing  -> putStrLn "Invalid tuple. Use syntax like (\"Awen\",\"Agatha\")" >> pure (elx, (h,c,s,p,l))
    _          -> putStrLn "Unknown field." >> pure (elx, (h,c,s,p,l))

removeItem :: Lexicon PythonCode -> Lambek.Env -> String -> [String] -> IO (Lexicon PythonCode, Lambek.Env)
removeItem elx (h,c,s,p,l) key v =
  let val = map stdize v in
  case map toLower key of
    "happy"    -> pure (elx, (filter (`notElem` val) h, c, s, p, l))
    "confused" -> pure (elx, (h, filter (`notElem` val) c, s, p, l))
    "sad"      -> pure (elx, (h, c, filter (`notElem` val) s, p, l))
    "people"   -> do
      let newPeople = filter (`notElem` val) p
          newH = filter (`notElem` val) h
          newC = filter (`notElem` val) c
          newS = filter (`notElem` val) s
          newL = filter (\(a,b) -> a `notElem` val && b `notElem` val) l
          newLex = elx { items = filter (\(w,_,_) -> w `notElem` val) (items elx), stp = stp elx }
      pure (newLex, (newH, newC, newS, newPeople, newL))
    "likes"    -> do
      let parsed = mapM (readMaybe :: String -> Maybe (String,String)) val
      case parsed of
        Just tuples -> 
          let stdTuples = map (\(a,b) -> (stdize a, stdize b)) tuples
              newL = filter (`notElem` stdTuples) l
          in pure (elx, (h, c, s, p, newL))
        Nothing -> putStrLn "Invalid tuple format. Use syntax like (\"Awen\",\"Butor\")" >> pure (elx,(h,c,s,p,l))
    _          -> putStrLn "Unknown field." >> pure (elx, (h,c,s,p,l))

-- read-eval-print-loop
repl :: Lexicon PythonCode -> PythonHandle -> Lambek.Env -> IO ()
repl elx phandle env = do
  -- print the prompt
  putStrLn ""
  putStr "> "
  -- get a line of input
  line <- getLine
  -- process commands
  case words line of
    (":quit":_) -> putStrLn "All your variables are belong to us. Exiting..." >> return ()
    (":help":_) -> do
      putStrLn "Commands (multiple inputs through \"f [a,b,c,...]\" or \"f a b c ...\"):"
      putStrLn "  :env                            Show current environment"
      putStrLn "  :set <field> <values>           Replace a field (happy/confused/sad/people/likes)"
      putStrLn "  :add <field> <values>           Add values to a field"
      putStrLn "  :remove <field> <values>        Remove values from a field"
      putStrLn "  :help                           How you got here"
      putStrLn "  :quit                           Exit"
      repl elx phandle env
    (":env":_)  -> do
      putStrLn "Current environment:"
      putStrLn (Lambek.prettyEnv env)
      repl elx phandle env
    (":set":field:vals) -> do
      (newLex, newEnv) <- updateEnv elx env field vals
      repl newLex phandle newEnv
    (":add":field:vals) -> do
      (newLex, newEnv) <- addItem elx env field vals
      repl newLex phandle newEnv
    (":remove":field:vals) -> do
      (newLex, newEnv) <- removeItem elx env field vals
      repl newLex phandle newEnv
    ("thank":"you":_) -> putStrLn "No worries!" >> repl elx phandle env
    ("thanks":_)      -> putStrLn "No problem" >> repl elx phandle env
    ("thx":_)         -> putStrLn "Ofc :)" >> repl elx phandle env
    ("ty":_)          -> putStrLn "np" >> repl elx phandle env
    _ -> do
      -- split it up into "standardized" words
      let ws = map stdize $ words line
          derivs = Lambek.derivation elx ws
          results = Lambek.meaning phandle elx ws env
          pretty = prettyLExpWith (mkRenamer ws)
      -- test if the string of words is a grammatical sentence according to the lexicon
      {- Commented because (when implemented correctly) it is redundant with derivations
      if Lambek.isGrammatical elx ws
        then putStrLn "That is a well-formed sentence!"
        else putStrLn "That is not a well-formed sentence"
      -}
      -- printing derivations
      if null derivs then putStrLn "This is not a well-formed sentence in the grammar we have defined."
      else do
        putStrLn "I can derive your sentence as follows:"
        mapM_ (\expr -> do
          let norm = normalize (Lambek.plugmeaning ws expr)
          val <- if Lambek.isYesNo norm then do
                  let truth = Lambek.yesno phandle norm env
                  pure $ if truth then "Yes" else "No"
                else if Lambek.isWho norm then do
                  let ans = Lambek.answers phandle norm env
                  pure $ if null ans then "Nobody" else unwords ans
                else do
                  let v = Lambek.interpreter norm env
                  pure $ show v
          putStrLn ("  " ++ pretty expr ++ "  -->  " ++ val)
          ) derivs
      -- repeat
      repl elx phandle env



main = do
  -- send console output immediately to the user
  hSetBuffering stdout NoBuffering
  -- input a lexicon file
  elx <- getLex
  -- input a database of facts
  db <- getDB
  -- launch a Python interpreter
  phandle <- launchPython db
  -- start the read-eval-print loop
  putStrLn "Starting chat..."
  putStrLn "Type :help for a list of commands."
  repl elx phandle Lambek.env
  where
    getDB :: IO PythonCode
    getDB = do
      putStr "Facts about the world: "
      dname <- getLine
      mdb <- try (readFile dname) :: IO (Either IOError String)
      case mdb of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getDB
        Right db -> return (PCode db)
      
    getLex :: IO (Lexicon PythonCode)
    getLex = do
      putStr "Lexicon file: "
      lname <- getLine
      res <- parseFile lexicon lname
      case res of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getLex
        Right (Left err) -> putStrLn ("parse error at " ++ lname ++ ":" ++ show err) >> getLex
        Right (Right lex) -> return lex
