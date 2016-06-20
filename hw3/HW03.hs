module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend initialState newName value = (\x -> if x == newName
                                      then value
                                      else initialState x)

empty :: State
empty name = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var name) = state name
evalE state (Val val) = val
evalE state (Op expr1 bop expr2) = case bop of
  Plus   -> (evalE state expr1) + (evalE state expr2)
  Minus  -> (evalE state expr1) - (evalE state expr2)
  Times  -> (evalE state expr1) * (evalE state expr2)
  Divide -> (evalE state expr1) `div` (evalE state expr2)
  Gt     -> if (evalE state expr1) > (evalE state expr2) then 1 else 0
  Ge     -> if (evalE state expr1) >= (evalE state expr2) then 1 else 0 
  Lt     -> if (evalE state expr1) < (evalE state expr2) then 1 else 0  
  Le     -> if (evalE state expr1) <= (evalE state expr2) then 1 else 0
  Eql    -> if (evalE state expr1) == (evalE state expr2) then 1 else 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign name expr) = DAssign name expr
desugar (Incr name) = DAssign name (Op (Var name) Plus (Val 1))
desugar (If expr stmt1 stmt2) = DIf expr (desugar stmt1) (desugar stmt2)
desugar (While expr stmt) = DWhile expr (desugar stmt)
desugar (For prestmt cond poststmt body) =
  (DSequence (desugar prestmt) (DWhile cond (DSequence (desugar body) (desugar poststmt))))
desugar (Sequence stmt1 stmt2) = DSequence (desugar stmt1) (desugar stmt2)
desugar Skip = DSkip  

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple initSt (DAssign name expr) = extend initSt name (evalE initSt expr)
evalSimple iS (DIf expr ifbody elsebody) =
  if 0 == (evalE iS expr)
   then (evalSimple iS elsebody)
   else (evalSimple iS ifbody)

evalSimple iS (DWhile expr stmt) =
  if 0 == (evalE iS expr)
  then iS
  else evalSimple (evalSimple iS stmt) (DWhile expr stmt)

evalSimple iS (DSequence s1 s2) =
  evalSimple (evalSimple iS s1) s2

evalSimple iS DSkip = iS


run :: State -> Statement -> State
run iS stmt = evalSimple iS (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
