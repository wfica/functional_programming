-- Komentarze zawierają testy, które można przeprowadzić w trybie interakcyjnym.

p = 10
k x = (x, p, x+p) 
-- k p
-- W module niedozwolone są wielokrotne deklaracje identyfikatorów.
-- Można to jednak zrobić w trybie interakcyjnym.
-- let p = 1000 
-- k p

sigma :: (Num a, Eq a) => a -> a
sigma n = if n==0 then 0 else n+sigma(n-1) 

-- Funkcje even i odd są zdefiniowane w standardowym preludium Haskella
even :: Integral a => a -> Bool
even n = if n==0 then True else Main.odd(n-1)

odd :: Integral a => a -> Bool
odd n = if n==0 then False else Main.even(n-1)

-- Main.even 128
-- Main.odd 128

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0 

odd' :: (Integral a) => a -> Bool
odd' n =  n `mod` 2 /= 0 


suc :: Int -> Int
suc n = if n == 0 then 1 else 1 + suc (n-1)

-- suc 100000000
-- *** Exception: stack overflow

sucTail'   :: Int -> Int
sucTail' n = sucAux 1 n
             where
               sucAux accum 0 = accum
               sucAux accum n = sucAux (accum+1) (n-1)

-- sucTail' 100000000
-- *** Exception: stack overflow

sucTail   :: Int -> Int
sucTail n = sucAux 1 n
             where
               sucAux accum 0 = accum
               sucAux accum n = (sucAux $! (accum+1)) (n-1)

-- sucTail 100000000
-- 100000001

-- W GHCi można ustawić opcję, powodującą wyprowadzanie ostrzeżenia, 
-- jeśli wzorce w definicji funkcji nie są wyczerpujące
-- :set -fwarn-incomplete-patterns
x = (False,10)
(z,y) = x
-- z
-- y
(False,y') = x
-- y'
(True,y'') = x   -- skompiluje się i wykona (leniwa ewaluacja!)
-- y''      dopiero teraz będzie błąd: Irrefutable pattern failed for pattern (GHC.Types.True,y'') 

l = ["Ala", "ma", "kota"]
[x1, x2, x3] = l
h:t = l

(p1,_) = (False,10)
x' = (("Kowal",25),True)
((n,w),b) = x'
(l'@(n',w'),b') = x'    -- as-pattern

f1 :: ((a, b), c) -> (a, b, (a, b), c)
f1 = \((x,y),z) -> (x,y,(x,y),z)
f2 :: ((a, b), c) -> (a, b, (a, b), c)
f2 = \(p@(x,y),z) -> (x,y,p,z)

-- Wyrażenie "case"
imply :: (Bool, Bool) -> Bool
imply pb =
  case pb of
    (True,True)   -> True
    (True,False)  -> False    
    (False,True)  -> True
    (False,False) -> True    
    
imply2 pb =    
  case pb of
    (True, x)  -> x
    (False, x) -> True
    
imply3 pb =    
  case pb of
    (True, x)  -> x
    (False, _) -> True    

imply4 pb =    
  case pb of
    (True, False) -> False
    _             -> True   
    
imply5 (True, False) = False
imply5      _        = True   

-- Symbol => jest w Haskellu zarezerwowany, patrz Haskell 2010, 2.4 Identifiers and Operators
(==>) True False = False  
(==>)  _     _   = True 
-- False ==> True

-- Kompilacja poniższej funkcji spowoduje bląd: Conflicting definitions for `x'
-- srednia (x,x) = x  
-- srednia (x,y) = (x+y)/2

-- Można użyć równości dozorowanych (ang. guarded equations)
srednia (x,y) | x==y = x  
srednia (x,y)        = (x+y)/2

srednia' (x,y) | x==y      = x  
               | otherwise = (x+y)/2
                             
-- W tym przykładzie najlepiej tak:  
srednia'''(x,y) = if x==y then x else (x+y)/2.0                             

-- Funkcje zip i unzip są zdefiniowane w standardowym preludium Haskella
-- zip :: [a] -> [b] -> [(a, b)]
-- unzip :: [(a, b)] -> ([a], [b])

-- __________________________________________________________________________
-- Leniwa ewaluacja może spowodować subtelne efekty w definiowanych funkcjach.
-- ___________________________________________________________________________
ff 0 _ = True
ff _ 0 = True
ff _ _ = False

ff' _ 0 = True
ff' 0 _ = True
ff' _ _ = False

{-
Funkcje ff i ff' dla argumentów o określonych wartościach zachowują się identycznie. Dla argumentów, których ewaluacja nie doprowadza do obliczenia wartości, ich zachowanie może być różne.

*Main> ff 0 (error "Och!") 
True
*Main> ff' 0 (error "Och!")
*** Exception: Och!
*Main> ff (error "Och!") 0 
*** Exception: Och!
*Main> ff' (error "Och!") 0
True

Funkcja ff jest "bardziej zdefiniowana" względem swojego drugiego argumentu,
natomiast funkcja ff' jest "bardziej zdefiniowana" względem swojego pierwszego argumentu.

Odpowiednie funkcje w OCamlu zachowywałyby się identycznie dla wszystkich argumentów.
-}
