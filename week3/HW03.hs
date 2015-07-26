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
extend state var val = newState state var val
    where newState oldState assignedVar assignedVal checkVar
            | assignedVar == checkVar = assignedVal 
            | otherwise = oldState checkVar

empty :: State
empty = state
    where state _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var str) = state str
evalE state (Val int) = int
evalE state (Op exp bop exp2)
    | bop == Plus = (evalE state exp) + (evalE state exp2)
    | bop == Minus = (evalE state exp) - (evalE state exp2)
    | bop == Times = (evalE state exp) * (evalE state exp2)
    | bop == Divide = (evalE state exp) `div` (evalE state exp2)
    | bop == Gt && (evalE state exp) > (evalE state exp2) = 1
    | bop == Ge && (evalE state exp) >= (evalE state exp2) = 1
    | bop == Lt && (evalE state exp) < (evalE state exp2) = 1
    | bop == Le && (evalE state exp) <= (evalE state exp2) = 1
    | bop == Eql && (evalE state exp) == (evalE state exp2) = 1
    | otherwise = 0
    

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign var exp) = (DAssign var exp)
desugar (Incr var) = (DAssign var (Op (Var var) Plus (Val 1)))
desugar (If condition true false) = (DIf condition (desugar true) (desugar false))
desugar (While condition loop) = (DWhile condition (desugar loop))
desugar (For init condition update loop) = (DSequence (desugar init) (DWhile condition (DSequence (desugar loop) (desugar update))))
desugar (Sequence stmnt1 stmnt2) = (DSequence (desugar stmnt1) (desugar stmnt2))
desugar (Skip) = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign var exp) = extend state var (evalE state exp)
evalSimple state (DIf condition true false)
    | (evalE state condition) == 1 = evalSimple state true
    | otherwise = evalSimple state false
evalSimple state (DWhile condition loop)
    | (evalE state condition == 1) = evalSimple (evalSimple state loop) (DWhile condition loop)
    | otherwise = state
evalSimple state (DSequence stmnt1 stmnt2) = (evalSimple (evalSimple state stmnt1) stmnt2)
evalSimple state (DSkip) = state

run :: State -> Statement -> State
run state stmnt = evalSimple state (desugar stmnt)

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
