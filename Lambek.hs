{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use notElem" #-}

module Lambek where

import LTypes
import LExp
import Data.List ((\\))
import PySupport 
import Control.Monad
import Control.Monad.RWS (MonadState(put))
import Data.Char


{-
____________________________________________________DECISION PROCEDURE_________________________________________________________________________________
-}

dec_rinv :: [LTp] -> LTp -> Bool
dec_rinv gamma (DivL a b) = dec_rinv (a:gamma) b
dec_rinv gamma (DivR b a) = dec_rinv (gamma++[a]) b
dec_rinv gamma (Atm p) = any (\k -> dec_lfoc (take k gamma) (gamma !! k) (drop (k+1) gamma) p) [0..length gamma - 1]
dec_rinv gamma (Neg a) = dec_rinv gamma (DivL a (Atm "s"))

dec_lfoc :: [LTp] -> LTp -> [LTp] -> Atm -> Bool
dec_lfoc gammaL (DivL a b) gammaR y = any (\k -> dec_rinv (drop k gammaL) a && dec_lfoc (take k gammaL) b gammaR y) [0..length gammaL]
dec_lfoc gammaL (DivR b a) gammaR y = any (\k -> dec_rinv (take k gammaR) a && dec_lfoc gammaL b (drop k gammaR) y) [0..length gammaR]
dec_lfoc gammaL (Atm x) gammaR y = null gammaL && null gammaR && x == y
dec_lfoc _ _ _ _ = undefined

isGrammatical :: Lexicon a -> [String] -> Bool
isGrammatical lex ws = not (any null wordTypes) && or [dec_rinv gamma (stp lex) | gamma <- sequence wordTypes] where
    wordTypes = map (\w -> [t | (w', t, _) <- items lex, w'==w]) ws


{-
____________________________________________________DERIVATION_________________________________________________________________________________
-}


der_rinv :: [(Var,LTp)] -> LTp -> [LExp]
der_rinv gamma (DivL a b) = 
    let x = freshvar gamma in
    map (L x) (der_rinv ((x, a):gamma) b)
der_rinv gamma (DivR b a) =
    let x = freshvar gamma in
    map (L x) (der_rinv (gamma++[(x,a)]) b)
der_rinv gamma (Atm p) =
  [ foldl A (V x) args
  | k <- [0 .. length gamma - 1]
  , let (x, a) = gamma !! k
  , args <- der_lfoc (take k gamma) (snd (gamma !! k)) (drop (k+1) gamma) p
  ]
der_rinv gamma (Neg a) =
  let x = freshvar gamma in
  map (L x) (der_rinv ((x, a):gamma) (Atm "s"))


-- get fresh variable not in 'gamma' to start with
-- used chatGPT to help with this one, mimicking structure of fresh in LExp.hs
freshvar :: [(Var, LTp)] -> Var
freshvar gamma =
  case named_vars \\ used of
    (x:_) -> x
    []    ->
      (if null used then "a" else maximum used) ++ "'"
  where
    used       = map fst gamma
    named_vars = [[c] | c <- ['a'..'z']]

der_lfoc :: [(Var,LTp)] -> LTp -> [(Var,LTp)] -> Atm -> [[LExp]]
der_lfoc gammaL (Atm p) gammaR    y = if null gammaL && null gammaR && p==y then [[]] else []
der_lfoc gammaL (DivL a b) gammaR y = 
    [ e:es |
    k  <- [0..length gammaL],
    e  <- der_rinv (drop k gammaL) a,
    es <- der_lfoc (take k gammaL) b gammaR y 
    ]
der_lfoc gammaL (DivR b a) gammaR y = 
    [ e:es | 
    k  <-  [0.. length gammaR],
    e  <- der_rinv (take k gammaR) a,
    es <- der_lfoc gammaL b (drop k gammaR) y 
    ]
der_lfoc _ _ _ _ = undefined

derivation :: Lexicon a -> [String] -> [LExp]
derivation lex ws = 
  let 
    names :: [Var]
    names = [ "x" ++ show i | i <- [1..] ]

    -- all possible type combinations for the sentence
    wordTypes :: [[(Var, LTp)]]
    wordTypes =
      [ [ (x, t) | (w', t, _) <- items lex, w' == w ]
      | (w, x) <- zip ws names
      ]
  in
    concat [ der_rinv gamma (stp lex) | gamma <- sequence wordTypes ]

{-
____________________________________________________MEANING_________________________________________________________________________________
-}


env = (
      ["Awen", "Butor" ], --happy
      ["Butor", "Celeste"], --confused
      ["Celeste"], --sad
      ["Awen", "Butor", "Celeste"], -- people
      [("Awen","Butor"), ("Butor", "Awen"), ("Butor", "Celeste"), ("Celeste", "Celeste") ] --likes
    )

prettyEnv :: Env -> String
prettyEnv (happy, confused, sad, people, likes) =
  unlines
    [ "   happy: " ++ unwords happy
    , "confused: " ++ unwords confused
    , "     sad: " ++ unwords sad
    , "  people: " ++ unwords people
    , "   likes: " ++ show likes
    ]

-- convert a word into it's meaning
toLExp :: String -> LExp
toLExp "Butor"    = V "Butor"
toLExp "Awen"     = V "Awen"
toLExp "Celeste"  = V "Celeste"
toLExp "Happy"    = L "x" (A (A (V "elem") (V "x")) (V "h"))  -- Lx. x in H
toLExp "Confused" = L "x" (A (A (V "elem") (V "x")) (V "c"))  -- Lx. x in C
toLExp "Sad"      = L "x" (A (A (V "elem") (V "x")) (V "s"))  -- Lx. x in S
toLExp "And1"     = L "p" (L "q" (L "x" (A (A (V "and") (A (V "p") (V "x"))) (A (V "q") (V "x")))))  -- adj \ (adj / ajd)
toLExp "And2"     = L "x" (L "y" (A (A (V "and") (V "x")) (V "y")))                                  -- s \ (s / s) 
toLExp "Is"       = L "x" (L "p" (A (V "p") (V "x")))
toLExp "Likes"    = L "x" (L "y" (A (A (V "elem") (pair (V "x") (V "y"))) (V "likes")) )
toLExp "Everyone" = L "p" (L "x" (A (A (V "all") (A (V "p") (V "x"))) (V "people"))) 
toLExp "Someone"  = L "p" (L "x" (A (A (V "any") (A (V "p") (V "x"))) (V "people")))
toLExp "Nobody"   = L "p" (L "x" (A (V "not") (A (A (V "any") (A (V "p") (V "x"))) (V "people"))))
toLExp "Herself"  = V "self"
toLExp "Himself"  = V "self"
toLExp "Really"   = L "f" (V "f")
toLExp "Sometimes"= L "f" (V "f")
toLExp "Deeply"   = L "f" (V "f")
toLExp "Especially"= L "f" (V "f")
toLExp "Probably" = L "f" (V "f")
toLExp "Who"      = V "who"
toLExp "Not"      = L "p" (L "x" (A (V "not") (A (V "p") (V "x"))))
toLExp "If" = L "p" (L "q" (A (A (V "impl") (V "p")) (V "q")))
toLExp "Be"       = L "p" (A (V "p") (V "you"))
toLExp "Be"       = L "x" (L "p" (A (V "p") (V "x")))
-- toLExp "Like"     = L "x" (A (A (V "elem") (pair (V "you") (V "x"))) (V "likes"))
toLExp "Like"     = L "x" (L "y" (A (A (V "elem") (pair (V "x") (V "y"))) (V "likes")) )
toLExp "Tell"     = V "tell"
toLExp "Me"       = V "me"
toLExp "Only"     = V "only"
toLExp "Does"     = V "does"
toLExp x          = V x




--helpers
getIndex :: String -> Int
getIndex ('x':ds) = read ds
getIndex _        = error "Invalid variable name"

pair :: LExp -> LExp -> LExp -- for likes
pair a = A (A (V "pair") a)

-- plugging the meaning of the words of the sentence in the free variables of an LExp 
plugmeaning :: [String] -> LExp -> LExp
plugmeaning ws (V x)     = if head x == 'x' then toLExp (ws!!(i-1)) else toLExp x where i = getIndex x
plugmeaning ws (A e1 e2) = A (plugmeaning ws e1) (plugmeaning ws e2)
plugmeaning ws (L a e)   = L a (plugmeaning ws e) 

-- given a normalized LExp containing the meaning of the sentence, check the meaning is true
interpreter :: LExp -> Env -> Bool  
interpreter (A (V "not") e) env = not (interpreter e env)
interpreter (A (A (V "impl") p) q) env = not (interpreter p env) || interpreter q env
interpreter (A (A (V "elem") (V x)) (V "h"))                              (h, _, _, _, _)     = x `elem` h --happy
interpreter (A (A (V "elem") (V x)) (V "c"))                              (_, c, _, _, _)     = x `elem` c --confused
interpreter (A (A (V "elem") (V x)) (V "s"))                              (_, _, s, _, _)     = x `elem` s --sad
-- interpreter (A (A (V "elem") (A (A (V "pair") (V x)) (V y))) (V "likes")) (_, _, _, _, likes) = (x, y) `elem` likes (original like for souvenir, or backup in case things go completely wrong)
interpreter (A (A (V "elem") (A (A (V "pair") (A (V "only") (V x))) (V y))) (V "likes")) (_, _, _, ppl, likes) = -- Only X likes Y
  let z = if y == "self" then x else y
  in  (x, z) `elem` likes
      && and [ if (u, z) `elem` likes then u == x else True | u <- ppl ]
interpreter (A (A (V "only") (L _ (A (A (V "elem") (A (A (V "pair") (V _)) (V z))) (V "likes")))) (V x)) (_, _, _, ppl, likes) = -- X only likes Y
  let z' = if z == "self" then x else z
  in  (x, z') `elem` likes
      && and [ if (x, w) `elem` likes then w == z' else True | w <- ppl ]
interpreter (A (A (V "elem") (A (A (V "pair") (V x)) (A (V "only") (V y)))) (V "likes")) (_, _, _, peo, likes) = -- X likes only Y
  let z = if y == "self" then x else y
  in  (x, z) `elem` likes
      && and [ if (x, w) `elem` likes then w == z else True | w <- peo ]
interpreter (A (A (V "elem") (A (A (V "pair") (V x)) (V y))) (V "likes")) (_, _, _, _, likes) =
  let z = if y == "self" then x else y
  in (x, z) `elem` likes
interpreter (A (A (V "and") e1) e2)                                       env                 = interpreter e1 env && interpreter e2 env
interpreter (L x (A (A (V "all") e1) (V "people")) )                      (h, c, s, peo, li)  =
    and [interpreter (subst (toLExp y, x) e1) (h, c, s, peo, li) | y <- peo]
interpreter (L x (A (A (V "any") e1) (V "people")) )                      (h, c, s, peo, li)  =
    or [interpreter (subst (toLExp y, x) e1) (h, c, s, peo, li) | y <- peo]
interpreter lexp _                                                                            =
  error $ "No interpretation of " ++ show (prettyLExp lexp) ++ " found"


{- I tried really really hard to implement this but python kept giving me issues so I gave up and used the previous version
lExpToPython :: LExp -> String
lExpToPython (V x) = x
lExpToPython (A f a) = "(" ++ lExpToPython f ++ ")(" ++ lExpToPython a ++ ")"
lExpToPython (L x e) = "lambda " ++ x ++ ": " ++ lExpToPython e

interpreterPython :: PythonHandle -> LExp -> Env -> IO Bool
interpreterPython phandle lexp (h,c,s,people,likes) = do
    let pyEnv = unlines $
          [ "people = " ++ show people
          , "likes  = " ++ show likes
          , "happy  = " ++ show h
          , "confused = " ++ show c
          , "sad = " ++ show s
          , "def pair(x): return lambda y: (x, y)"
          , "def elem(x): return lambda lst: x in lst"
          ] ++ [ name ++ " = " ++ show name | name <- people ]
        expr = lExpToPython lexp
        code = pyEnv ++ "print(bool(" ++ expr ++ "))"
    out <- runPythonCode phandle (PCode code)
    pure $ filter (not . isSpace) out == "True"
-}

-- for imperative sentences (based on construction of interrogative)
unwrap :: LExp -> LExp
unwrap (A (A (V "tell") _) e) = unwrap e
unwrap e                      = e

-- for interrogative sentences
isWho :: LExp -> Bool
isWho e =
  case unwrap e of
    A (V "who") (L _ _) -> True
    _                   -> False
answers :: PythonHandle -> LExp -> Env -> [String]
answers phandle e env@(_,_,_,people,_) =
  case unwrap e of
    A (V "who") (L x body) -> filter (\y -> interpreter (subst (toLExp y, x) body) env) people
    _ -> return []

-- for yes/no
isYesNo :: LExp -> Bool
isYesNo e =
  case unwrap e of
    A (V "does") _ -> True
    _              -> False
yesno :: PythonHandle -> LExp -> Env -> Bool
yesno phandle e env =
  case unwrap e of
    A (V "does") s -> interpreter s env
    _              -> False

type Env = ([String], -- happy
            [String], -- confused
            [String], -- sad
            [String], -- people
            [(String, String)] -- likes
            ) 

meaning :: PythonHandle -> Lexicon a -> [String] -> Env -> IO [(Bool, LExp)]
meaning phandle lex ws envi = do
  let
    names :: [Var]
    names = [ "x" ++ show i | i <- [1..] ]

    wordTypes :: [[(Var, LTp)]]
    wordTypes =[ [ (x, t) | (w', t, _) <- items lex, w' == w ] | (w, x) <- zip ws names]
  results <- forM (associate (stp lex) ws (sequence wordTypes)) $ \(s, gamma) -> do
        let derivs = der_rinv gamma (stp lex)
        forM derivs $ \l -> do
            let val = interpreter (normalize (plugmeaning s l)) envi
            return (val, l)
  return (concat results)




meaningEnv :: PythonHandle -> Lexicon a -> [String] -> IO [(Bool, LExp)] 
meaningEnv phandle lex ws = meaning phandle lex ws env

associate :: LTp -> [String] -> [[(Var, LTp)]] -> [([String], [(Var, LTp)])]
associate _ _ []      = []
associate s ws (x:xs) =  
  ([if w=="And" then if t == DivL s (DivR s s) then "And2" else "And1" else w
   | (w, (v,t)) <- zip ws x], x)  :associate s ws xs

