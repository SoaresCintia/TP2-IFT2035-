-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use shows" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Use putStr" #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- 1ère représentation interne des expressions de notre language         --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire "Lambda"                                 --
---------------------------------------------------------------------------

type Var = String

-- Type Haskell qui décrit les types Psil.
data Ltype = Lint
           | Larw Ltype Ltype   -- Type "arrow" des fonctions.
           deriving (Show, Eq)

-- Type Haskell qui décrit les expressions Psil.
data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lhastype Lexp Ltype -- Annotation de type.
          | Lapp Lexp Lexp      -- Appel de fonction, avec un argument.
          | Llet Var Lexp Lexp  -- Déclaration de variable locale.
          | Lfun Var Lexp       -- Fonction anonyme.
          deriving (Show, Eq)

-- Type Haskell qui décrit les déclarations Psil.
data Ldec = Ldec Var Ltype      -- Déclaration globale.
          | Ldef Var Lexp       -- Définition globale.
          deriving (Show, Eq)
          

-- Conversion de Sexp à Lambda --------------------------------------------

-------------------------------------------------------------------------------
-- Fonction prise du document e2023-soln.hs
-------------------------------------------------------------------------------
sexp2list :: Sexp -> [Sexp]
sexp2list sexp = s2l' sexp []
    where s2l' Snil res = res
          s2l' (Scons ses se) res = s2l' ses (se : res)
          s2l' se _ = error ("Liste inconnue: " ++ showSexp se)
-------------------------------------------------------------------------------

s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint
-- ¡¡COMPLÉTER ICI!!
-------------------------------------------------------------------------------
-- Fonction prise du document e2023-soln.hs
-------------------------------------------------------------------------------
s2t (Scons (Scons ses (Ssym "->")) t2) =
    foldr (\ se -> Larw (s2t se)) (s2t t2) (sexp2list ses)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

s2t se = error ("Type Psil inconnu: " ++ (showSexp se))

s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
-- ¡¡COMPLÉTER ICI!!
-------------------------------------------------------------------------------
-- Fonction prise du document e2023-soln.hs
-------------------------------------------------------------------------------
s2l (se@(Scons _ _)) =
    case sexp2list se of
        -- Annotation de type
        [Ssym ":", e, t] -> Lhastype (s2l e) (s2t t)
        -- Fonction lambda
        [Ssym "fun", Ssym x, e] -> Lfun x  (s2l e)
        -- Let
        [Ssym "let", (Scons Snil (Scons (Scons Snil (Ssym x)) e1)), e2]
            -> Llet x (s2l e1) (s2l e2)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
        -- [Ssym "macro",Scons (Scons (Scons Snil (Ssym "fun")) (Ssym arg)) body]
        --     -> Lfun arg (s2l body)
-------------------------------------------------------------------------------
-- Fonction prise du document e2023-soln.hs
-------------------------------------------------------------------------------
        -- Autres cas syntaxiquement invalides de :, fun et let
        Ssym h : _ | h `elem` [":", "fun", "let"] ->
                        error ("Arguments invalides pour `"
                                ++ h ++ "`: " ++ showSexp se)
        -- Appels
        h : ses -> foldl (\ le arg -> Lapp le (s2l arg)) (s2l h) ses
        -- Erreurs de syntaxe
        [] -> error "Impossible"
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

s2l se = error ("Expression Psil inconnue: " ++ (showSexp se))

s2d :: Sexp -> Ldec
-- s2d (Scons (Scons (Scons Snil (Ssym "def")) (Ssym v)) e) = Ldef v (s2l e)
-- ¡¡COMPLÉTER ICI!!
-------------------------------------------------------------------------------
-- Fonction prise du document e2023-soln.hs
-------------------------------------------------------------------------------
s2d (sexp@(Scons _ _)) =
    case sexp2list sexp of
        -- Définition
        [Ssym "def", Ssym v, e] -> Ldef v (s2l e)
        -- Déclaration
        [Ssym "dec", Ssym v, t] -> Ldec v (s2t t)
        -- Erreurs de syntaxe
        h : _ -> error ("N'introduit pas une déclaration: " ++ showSexp h)
        [] -> error "Impossible"
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
s2d se = error ("Déclaration Psil inconnue: " ++ showSexp se)

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

-- Type des tables indexées par des `α` qui contiennent des `β`.
-- Il y a de bien meilleurs choix qu'une liste de paires, mais
-- ça suffit pour notre prototype.
type Map α β = [(α, β)]

-- Transforme une `Map` en une fonctions (qui est aussi une sorte de "Map").
mlookup :: Map Var β -> (Var -> β)
mlookup [] x = error ("Uknown variable: " ++ show x)
mlookup ((x,v) : xs) x' = if x == x' then v else mlookup xs x'

minsert :: Map Var β -> Var -> β -> Map Var β
minsert m x v = (x,v) : m

type TEnv = Map Var Ltype
type TypeError = String

-- L'environment de typage initial.
tenv0 :: TEnv
tenv0 = [("+", Larw Lint (Larw Lint Lint)),
         ("-", Larw Lint (Larw Lint Lint)),
         ("*", Larw Lint (Larw Lint Lint)),
         ("/", Larw Lint (Larw Lint Lint)),
         ("if0", Larw Lint (Larw Lint (Larw Lint Lint)))]

-- `check Γ e τ` vérifie que `e` a type `τ` dans le contexte `Γ`.
check :: TEnv -> Lexp -> Ltype -> Maybe TypeError
-- ¡¡COMPLÉTER ICI!!
---------------------------------------------------------------------------
-- Fonction prise du document e2023-soln.hs
---------------------------------------------------------------------------
check tenv (Lfun x e) (Larw t1 t2) =
    check (minsert tenv x t1) e t2
check _ (Lfun _ _) t =
    error ("Type invalide pour Lfun (n'est pas de la forme t1->t2): " ++ show t)
---------------------------------------------------------------------------
---------------------------------------------------------------------------

check tenv e t
  -- Essaie d'inférer le type et vérifie alors s'il correspond au
  -- type attendu.
  = let t' = synth tenv e
    in if t == t' then Nothing
       else Just ("Erreur de type: " ++ show t ++ " ≠ " ++ show t')

-- `synth Γ e` vérifie que `e` est typé correctement et ensuite "synthétise"
-- et renvoie son type `τ`.
synth :: TEnv -> Lexp -> Ltype
synth _    (Lnum _) = Lint
synth tenv (Lvar v) = mlookup tenv v
synth tenv (Lhastype e t) =
    case check tenv e t of
      Nothing -> t
      Just err -> error err
-- ¡¡COMPLÉTER ICI!!
---------------------------------------------------------------------------
-- Partie prise du document e2023-soln.hs
---------------------------------------------------------------------------
synth tenv (Lapp e1 e2) =
    case synth tenv e1 of
      Larw t1 t2 ->
          case check tenv e2 t1 of
            Nothing -> t2
            Just err -> error err
      _ -> error ("Not a function: " ++ show e1)
synth tenv (Llet x e1 e2) =
    let t1 = synth tenv e1
        tenv' = minsert tenv x t1
    in synth tenv' e2
---------------------------------------------------------------------------
---------------------------------------------------------------------------
synth _tenv e = error ("Incapable de trouver le type de: " ++ (show e))

        
---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs renvoyées par l'évaluateur.
data Value = Vnum Int
           | Vfun VEnv Var Lexp
           | Vop (Value -> Value)

type VEnv = Map Var Value

instance Show Value where
    showsPrec p  (Vnum n) = showsPrec p n
    showsPrec _p (Vfun _ _ _) = showString "<fermeture>"
    showsPrec _p (Vop _) = showString "<fonction>"

-- L'environnement initial qui contient les fonctions prédéfinies.
venv0 :: VEnv
venv0 = [("+", Vop (\ (Vnum x) -> Vop (\ (Vnum y) -> Vnum (x + y)))),
         ("-", Vop (\ (Vnum x) -> Vop (\ (Vnum y) -> Vnum (x - y)))),
         ("*", Vop (\ (Vnum x) -> Vop (\ (Vnum y) -> Vnum (x * y)))),
         ("/", Vop (\ (Vnum x) -> Vop (\ (Vnum y) -> Vnum (x `div` y)))),
         ("if0", Vop (\ (Vnum x) ->
                       case x of
                         0 -> Vop (\ v1 -> Vop (\ _ -> v1))
                         _ -> Vop (\ _ -> Vop (\ v2 -> v2))))]

-- La fonction d'évaluation principale.
eval :: VEnv -> Lexp -> Value
eval _venv (Lnum n) = Vnum n
eval venv (Lvar x) = mlookup venv x
-- ¡¡COMPLÉTER ICI!!
-------------------------------------------------------------------------------
-- Fonction prise du document e2023-soln.hs
-------------------------------------------------------------------------------
eval venv (Lhastype e _) = eval venv e
eval venv (Lapp e1 e2) =
    let argValue = eval venv e2
    in case eval venv e1 of
        Vop f -> f argValue
        Vfun fenv param body -> eval (minsert fenv param argValue) body
        other -> error ("Trying to call a non-function: " ++ show other)
eval venv (Llet x e1 e2) = eval (minsert venv x (eval venv e1)) e2
eval venv (Lfun x e) = Vfun venv x e
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- État de l'évaluateur.
type EState = ((TEnv, VEnv),       -- Contextes de typage et d'évaluation.
               Maybe (Var, Ltype), -- Déclaration en attente d'une définition.
               [(Value, Ltype)])   -- Résultats passés (en ordre inverse).

-- Évalue une déclaration, y compris vérification des types.
process_decl :: EState -> Ldec -> EState
process_decl (env, Nothing, res) (Ldec x t) = (env, Just (x,t), res)
process_decl (env, Just (x', _), res) (decl@(Ldec _ _)) =
    process_decl (env, Nothing,
                  error ("Manque une définition pour: " ++ x') : res)
                 decl
process_decl ((tenv, venv), Nothing, res) (Ldef x e) =
    -- Le programmeur n'a *pas* fourni d'annotation de type pour `x`.
    let ltype = synth tenv e
        tenv' = minsert tenv x ltype
        val = eval venv e
        venv' = minsert venv x val
    in ((tenv', venv'), Nothing, (val, ltype) : res)
-- ¡¡COMPLÉTER ICI!!
-------------------------------------------------------------------------------
-- Fonction prise du document e2023-soln.hs
-------------------------------------------------------------------------------
process_decl ((tenv, venv), Just (x',ltype), res) (Ldef x e) =
    if x' /= x then
        process_decl ((tenv, venv), Nothing,
                      error ("Manque une définition pour: " ++ x') : res)
                     (Ldef x e)
    else
        -- Le programmeur a fourni une annotation de type pour `x`.
        let tenv' = minsert tenv x ltype
        in case check tenv' e ltype of
             Nothing ->
                 -- `venv'` et `val` sont mutuellement récursifs parce que
                 -- `e` peut être récursif (i.e. faire référence à `x`).
                 let venv' = minsert venv x val
                     val = eval venv' e
                 in ((tenv', venv'), Nothing, (val, ltype) : res)
             Just err -> ((tenv', venv), Nothing, error err : res)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

process_sexps :: EState -> [Sexp] -> IO ()
process_sexps _ [] = return ()
process_sexps es (sexp : sexps) =
    let decl = s2d sexp
        (env', pending, res) = process_decl es decl
    in do (hPutStr stdout)
            (concat
             (map (\ (val, ltyp) ->
                   "  " ++ show val ++ " : " ++ show ltyp ++ "\n")
              (reverse res)))
          process_sexps (env', pending, []) sexps

-- Lit un fichier contenant plusieurs Sexps, les évalue l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename
  = do filestring <- readFile filename
       let sexps = case parse pSexps filename filestring of
                     Left err -> error ("Parse error: " ++ show err)
                     Right es -> es
       process_sexps ((tenv0, venv0), Nothing, []) sexps

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

typeOf :: String -> Ltype
typeOf = synth tenv0 . lexpOf

valOf :: String -> Value
valOf = eval venv0 . lexpOf
