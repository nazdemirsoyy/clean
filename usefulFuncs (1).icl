module usefulFuncs
import StdEnv

/* Written by Zayar Htet
 * ELTE BSc Computer Science
 * 2021-2022 Autumn Semester
I gave you some useful functions. 
Please open usefulFunc.icl and you can just copy and paste that function if you want to use.
*/

//remove elements from the list
remove :: Int [Int] -> [Int]
remove n [] = []
remove n [x:xs]
| n==x = remove n xs
= [x: remove n xs]

//Remove duplicate
duplicrem :: [Int] -> [Int] 
duplicrem [] = []
duplicrem [x:xs] = [x: duplicrem (remove x xs)] // you need to reuse remove function

//Count Duplicate
duplicount :: Int [Int] -> Int
duplicount n [] = 0
duplicount n [x:xs]
| n == x = 1+ (duplicount n xs)
= duplicount n xs

//digits to list
digitsToList :: Int [Int] -> [Int] // input list is an empty list
digitsToList 0 x = x
digitsToList n x = digitsToList (n/10) [n rem 10: x]

//list to digit
listToDigits :: [Int] -> Int
listToDigits [] = 0
listToDigits [x:xs] = x* (10^((length [x:xs])-1)) + listToDigits xs

//Dividing a single list into sublists
listOfLists :: [Int] [Int] -> [[Int]]
listOfLists [] [] = []
listOfLists fibSeq [x:xs] = [ take x fibSeq : listOfLists (drop x fibSeq) xs]

//Divisors of an integer
divisors :: Int Int-> [Int] // second Int must be 1
divisors x n
| x == n = [n]
| x rem n == 0 = [n : divisors x (n+1)]
= divisors x (n+1)

isPrimes :: Int -> Bool
isPrimes x = (length ((divisors x 1)) <= 2)

//First: find the nth term fibonnaci number
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

//counting the number of digits
counter :: Int -> Int
counter 0 = 0
counter x = 1+ counter (x/10)

/*=====================================================================*/

//Second: form a fibonnaci sequence (put the nth term into a list) - this will only give you descending sequence
draftFibInList :: Int -> [Int]
draftFibInList 0 = []
draftFibInList n = [fibo n: draftFibInList (n-1)]


//Third: Reverse a fibonnaci sequence to obtain ascending sequence
fibInList :: Int -> [Int]
fibInList n = reverse (draftFibInList n)

/*=====================================================================*/

//The way to check the number is Integer or not
result == toReal(toInt result) 
// The result will be in Real and change to into Int 
//it will round the number for example ( x = 3.142, toInt x = 3)
//change it again to Real (toReal (toInt x) = 3.00)
//and compare with the original number (x == toReal (toInt x))
//if the number is Integer it will result True, if not, False

/*=====================================================================*/

:: Q = { nom :: Int , den :: Int }

instance + Q where + q1 q2 = {nom = q1.nom*q2.den + q2.nom*q1.den, den = q1.den * q2.den}
instance ~ Q where ~ q1  = {nom = ~q1.nom, den = ~q1.den}
instance * Q where * q1 q2 = {nom = q1.nom*q2.nom, den = q1.den*q2.den}

simplify {nom=n, den=d}
|d==0=abort "denominator is 0"
|d<0 = {nom = (~n/g), den = (~d/g)}
= {nom = n/g, den = d/g}
	where
	g = gcdm n d

gcdm x y = gcdnat (abs x) (abs y)
	where 
	gcdnat x 0 = x
	gcdnat x y = gcdnat y (x rem y)

/*=====================================================================*/

:: Tree a = Node a (Tree a) (Tree a) | Leaf

instance == (Tree a) | == a
where
    == Leaf Leaf = True
    == (Node x1 l1 r1) (Node x2 l2 r2) = and[x1==x2, l1==l2, r1==r2]
    == _ _ = False
