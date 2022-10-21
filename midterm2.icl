module midterm2
import StdEnv 

/* 1. Armstrong number

 If sum of cubes of each digit of the number is equal to the number itself,
 then the number is called an Armstrong number.
 153 = 1^3 + 5^3 + 3^3
 Given a positive integer number, write a function to determine whether it is
 an Armstrong number or not.
*/

toDigits :: Int -> [Int]
toDigits x 
| x < 10 = [x]
= toDigits (x / 10) ++ [x rem 10] 

armstrong::Int -> [Int]
armstrong x = take x [n \\ n <- [1..] | isArm n]

isArm :: Int -> Bool
isArm x = x == sum [n^m\\n<- list]
	where 
	list = toDigits x
	m = length list 

//Start = isArm 153 // True
//Start = isArm 370 // True
//Start = isArm 0 // True
//Start = isArm 12 // False


/* 2. Occurrences

 Given a list of integers, replace every element in the list with its number
 of occurrences in the list.
*/

getFrequency :: [Int] Int -> Int
getFrequency [] num = 0
getFrequency [x:xs] num
| (x == num) = 1 + (getFrequency xs num)
= (getFrequency xs num)

getFrequency2 :: [Int] Int -> Int
getFrequency2 list num = length (filter (\x = (x == num)) list)

//Start = getFrequency [1,1,1,1,2,3,2,5,6,2,2,2,5] 2

auxOccNum :: [Int] [Int] -> [Int]
auxOccNum [] auxList = []
auxOccNum [x:xs] auxList = [ (getFrequency auxList x) : (auxOccNum xs auxList)] 

//Start = auxOccNum [1,1,1,1,2,3,2,5,6,2,2,2,5] [1,1,1,1,2,3,2,5,6,2,2,2,5] 

occNum :: [Int] -> [Int]
occNum list = (auxOccNum list list)

//Start = occNum [1,1,1,1,2,3,2,5,6,2,2,2,5] // [4,4,4,4,5,1,5,2,1,5,5,5,2]
// Start = occNum [1..5] // [1,1,1,1,1]
// Start = occNum ([1..5] ++ [1..7]) // [2,2,2,2,2,2,2,2,2,2,1,1]
// Start = occNum([7..9] ++ [7..9] ++ [7..9]) // [3,3,3,3,3,3,3,3,3]


/* 3. Gap2
 Given a list of numbers, convert the list in such a way that 
 the difference between two consecutive elements is always 2,
 you may have to add numbers in order to achieve that.
 e.g: [1,5,8] = [1,3,5,7,9]
*/

gap2 :: [Int] -> [Int]
gap2 [] = []
gap2 list = [(hd list),((hd list)+2)..((last list)+1)]

//Start = gap2 [1,5,8] // [1,3,5,7,9]
//Start = gap21 [1,5,15] // [1,3,5,7,9,11,13,15]
//Start = gap2 [] 


/* 4. Not Palindrome
 Given a list of lists of integers,
 write a function that gets rid of Palindrome numbers.
 A palindrome number is a number that can be read from left to right or
 from right to left and gets the same number, 
 e.g. 12521 is a palindrome number. 
*/

//toDigits :: Int -> [Int]
//toDigits x 
//| x < 10 = [x]
//= toDigits (x / 10) ++ [x rem 10] 

isPal :: Int -> Bool
isPal x
| x < 10 = False
 = list == reverse list
where
	list = toDigits  x

Polindromes :: [Int] ->[Int]
Polindromes list = filter isPal list 

//Start = Polindromes [141,1,2,34,45678,1111,121]

getRidPal :: [[Int]] -> [[Int]]
getRidPal twoDList = map (\oneDList = (filter (\x = not (isPal x)) oneDList)) twoDList 

////////

filterList :: [Int] -> [Int]
filterList [] = []
filterList [x:xs]
| not (isPal x) = [x : filterList xs]
= filterList xs

getRidPal2 :: [[Int]] -> [[Int]]
getRidPal2 [] = []
getRidPal2 [list:lists] = [(filterList list) : getRidPal2 lists]

//Start = getRidPal2 [[1,2,1111],[151,22,3455]] // [[1,2],[3455]]
//Start = getRidPal [[1,222],[151,202,50505]] // [[1],[]]
//Start = getRidPal [[],[22]] // [[],[]]


/* 5. Not Primes
 Given a list of integers, write a function which removes the prime numbers   from the list.
 There will be no negative integers and consider the number 1 not a prime.
*/

isPrime :: Int -> Bool
isPrime x = length(primeFactorization x 2 [1]) == 2

primeFactorization :: Int Int [Int] -> [Int]
primeFactorization x y list
| x == 1 = list
| x rem y == 0 = primeFactorization (x/y) 2 (list++[y])
= primeFactorization x (y+1) list

removeNotPrime::[Int]->[Int]
removeNotPrime list = [ x \\ x <-list | not(isPrime x)] 

//Start = removeNotPrime [1..10] // [1,4,6,8,9,10]
//Start = removeNotPrime [13..20] // [14,15,16,18,20]
//Start = removeNotPrime [2,4,8,9,10,23] // [4,8,9,10]
//Start = removeNotPrime [] // []

//

myFun :: Int -> Bool 
myFun c 
| c == 1 = True
=  ( [a \\ a <- [2..c - 1] | c rem a == 0])  <> []
 
// \c | c == 1 = True =  ( [a \\ a <- [2..c - 1] | c rem a == 0])  <> []
 
removeNotPrime2 :: [Int] -> [Int]
removeNotPrime2 x = filter (myFun) x

//Start = removeNotPrime2 [1..10] // [1,4,6,8,9,10]
// Start = removeNotPrime2 [13..20] // [14,15,16,18,20]
// Start = removeNotPrime2 [2,4,8,9,10,23] // [4,8,9,10]
// Start = removeNotPrime2 [] // []


//Given a 3x3 matrix of '0's and '1's return if a line is formed from the given '1's.
matrix2Aux :: [Int] [Int] [Int] -> Bool 
matrix2Aux x y z 
| x!!0 + y!!1 + z!!2 == 3 = True     
| x!!2 + y!!1 + z!!0 == 3 = True 
= False

matrix2 :: [[Int]] -> Bool 
matrix2 [x,y,z] 
| sum x == 1 && sum y == 1 && sum z == 1 = matrix2Aux x y z   
= (sum x == 3 &&  sum z == 0 && sum y == 0 )|| (sum y == 3 &&  sum z == 0 && sum x == 0 )|| (sum x == 0 &&  sum z == 3 && sum y == 0)

//Start = matrix2 [[0,0,1], [0,1,0], [1,0,0]]     // True
//Start = matrix2 [[1,1,1], [0,0,0] , [0,0,0]]      // True
//Start = matrix2 [[1,0,0], [0,1,0] , [0,0,1]]     // True
//Start = matrix2 [[1,0,0], [0,0,1], [1,1,0]]     // False

/* 6. zipWith

 Implement the function zipWith that takes a function, 
 and two lists, and combines them in such a way that 
 elements that are in the same positions get the function 
 applied to them.

 E.g: zipWith addTwoNumbers [1,2,3] [5,6,7] = [1+5,2+6,3+7] = [6,8,10]
*/
//DON'T DELETE THESE FUNCTIONS !!!

//addTwoNumber x y = x + y
//prodTwoNumber x y = x * y
//niceTwoNumber x y = x rem y



//zipWith :: (Int Int -> Int) [Int] [Int] -> [Int]


//Start = zipWith addTwoNumber [1,2,3] [5,6,7] // [6,8,10]
//Start = zipWith prodTwoNumber [1,2,3] [5,6,7] // [5,12,21]
//Start = zipWith niceTwoNumber [5,6,7] [1,2,3] // [0,0,1]


/* 7. Collatz conjecture

 Given a positive number greater than 1, return how many iterations does it 
 take for that number to fall down to "2" if we keep applying the
 Collatz equation on it.
 Collatz conjecture equation:
 If the number is even -> x/2
 If the number is odd -> 3x+1
 e.g: input: 10 
      steps: 10 -> 5 -> 16 -> 4 -> 2
      output: 4 recursive calls
*/

//collatz :: Int -> Int

//Start = collatz 10 // 4
//Start = collatz 20000000 // 144
//Start = collatz 5 // 3
//Start = collatz 0 // "The number must be greater than 1"


/* 8. Good Lists

 Given a list of lists, count how many of the sublists are good lists.
 A list is good if it is empty or its 1st number is odd, 2nd is even, 
 3rd is odd, 4th is even and so on.
 E.g: [[],[1,2,3,4],[8,3,4],[9],[3,4,4]] your function should return
 3 as only [], [1,2,3,4] and [9] are "good".
 */

//isGood :: Int [Int] -> Int

// Start = isGood [[],[1,2,3,4],[8,3,4],[9],[3,4,4]] // 3
// Start = isGood [[1,2,3,4],[3,4,4],[3,42],[12,2,1,2]] // 2
// Start = isGood [[],[1,2,3,4],[],[8,3,4],[1],[2],[9],[3,4,4]] // 5
// Start = isGood [] // 0


/* 9. Symmetrical lists
 Given a list of lists of integers, write a function 
 which returns a list of symmetrical lists, 
 if the sum of the sublist is greater than 10.
*/

//symSumGreater10 :: [[Int]] -> [[Int]]

//Start = symSumGreater10 [[1,2,3],[3,4,5,6],[4,5,1,2]] // [[3,4,5,6,6,5,4,3],[4,5,1,2,2,1,5,4]]
//Start = symSumGreater10 [] // []
//Start = symSumGreater10 [[1..10],[1,2]] // [[1,2,3,4,5,6,7,8,9,10,10,9,8,7,6,5,4,3,2,1]]

 
/* 10. Elements in interval

 Given a list of triple tuples consisting of two integer values and 
 and a list of integers (left,right,[Int]),
 for every tuple return only the elements from the list 
 which positions' are inside the interval [left..right]
 Assume that the indexes are all valid.
*/

//elementInInterval :: [(Int ,Int,[Int])]-> [[Int]]

//Start = elementInInterval [(2,5,[1..10])] //[[3,4,5,6]]
//Start = elementInInterval [(5,6,[1..8]), (3,5,[4..9])] //[[6,7],[7,8,9]]
//Start = elementInInterval [(4,7,[1,2,3,4,5,6,7,8,9])] //[[5,6,7,8]]
