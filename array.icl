//Array

/*
	Array = {elem \\ elem <- List}
	List = [elem \\ elem <-: Array]

*/

listToArray::[a]->{a}
listToArray list = {a \\ a <- list }

arrayToList::{a}->[a]
arrayToList arr = [a \\ a <-: arr] 

//{#Char} == String
//{'H', .. } "Hello world"

stringToList :: String -> [Char]
stringToList str = [a \\ a<-:str]

//Start = stringToList "Hello world" // ['H','e','l','l','o',' ','w','o','r','l','d']

listtoString :: [Char] -> String
listtoString list = {a \\ a<- list}

//Start = listtoString ['H','e','l','l','o',' ','w','o','r','l','d'] //"Hello world"

// 1. Create arrays using comprehensions for the followings:

// - powers of 10 from 1st to the 10th
arr :: {Int}
arr = {10^x \\ x<-[1..10]}

//Start :: {Int}
//Start = arr

// - {(0,0),(1,1),..., (10,10)}
arr1 :: {(Int,Int)}
arr1 = {(x,x) \\ x<-[0..10]}

//Start :: {(Int,Int)}
//Start = arr1

//Start :: (Int, Int)
Start = arr1.[3]

// - one number at its half, and so on until is 0 
// e.g. {100, 50, 25, 12, 6, 3, 1} 
f :: Int -> [Int] 
f n = takeWhile ((<)0) (iterate (\ x= x/2) n)

//Start = f 100 
arr2 :: Int -> {Int}
arr2 n = {x \\ x<-f n}

//Start :: {Int}
//Start = arr2 100

// 3. Given an array of lists of integers and an integer, 
// keep the lists whose difference between max and min 
// element squared is greater than the given number
// There are no [] in the array.

cond1 :: [Int] Int -> Bool
cond1 ls n = (a-b)*(a-b) > n
where a = last (sort ls)
	  b = hd (sort ls)
	
minMaxDiff::{[Int]} Int->{[Int]}
minMaxDiff ar n = {t\\ t<-: ar | cond1 t n}

//Start = minMaxDiff {[1,21,2],[1,1,1,1,1],[1]} 5//{[1,21,2]}
//Start = minMaxDiff {[1,21],[1..10],[4,3]} 5//{[1,21],[1,2,3,4,5,6,7,8,9,10]}
//Start = minMaxDiff {[1..10],[5..6]} -3//{[1,2,3,4,5,6,7,8,9,10],[5,6]}

// 5. Given array find max of it and return new array which has 
//    all occurrences of maximum removed.
//	  E.g. {1,4,5,3,3,2,4,5,1,3,4} max is 5 -> {1,4,3,3,2,4,1,3,4}.

rem_max :: {Int} -> {Int}
rem_max ar = {t\\ t<-:ar | t <> n}
where n = last (sort [u \\ u<-: ar])

//Start = rem_max {1,4,5,3,3,2,4,5,1,3,4} //{1,4,3,3,2,4,1,3,4}
//Start = rem_max {1,42,42,52,452,4} // {1,42,42,52,4}
//Start = rem_max {5} // {}
//Start = rem_max {} // {}


// 6. Given two arrays, return new array such that i-th element of it is 
// maximum of i-th element of first and second arrays.
// E.g. when we calculate 5th element of result array, we look at 
// 5th element of first and 5th element of second arrays, and choose maximum of the two.
// You can assume that arrays have same length. 

maxOfTwo :: {Int} {Int} -> {Int}
maxOfTwo ara arb = {max a b \\ a<-: ara & b<-: arb }
//Start = maxOfTwo {} {} // {}
//Start = maxOfTwo {1} {5} // {5}
//Start = maxOfTwo {1,5,4} {2,3,6} // {2,5,6}
//Start = maxOfTwo {1,2,3,4,5} {1,2,3,4,5} // {1,2,3,4,5}

// 7. You are given array of integers.
// Your function should return true if each value appears at least twice 
// in the array, and it should return false
// if any element is distinct.

find :: Int {Int} -> [Int]
find i arr = [b\\ b <-: arr | b == i]

is2 :: Int {Int} -> Bool
is2 i arr = length (find i arr) >= 2

f7 :: {Int} -> Bool
f7 ar = and [is2 i ar \\ i <-: ar]

//Start = f7 {1,2,3,1,3,2,2,2} // True
//Start = f7 {1,2,3,4,3,2,1} // False
//Start = f7 {1,1,1,3,3,4,3,2,4,2} // True

// 8. An array is monotonic if it is either monotone increasing or 
// monotone decreasing
// A is monotone increasing if for all i<=j, A[i]<=A[j]
// A is monotone decreasing if for all i<=j, A[i]>=A[j]
// Given array, your task is to decide if it is monotonic.

isMonotonic :: {Int} -> Bool
isMonotonic arr = (l == x) || (l == reverse x)
where 
	l = [a \\ a <-: arr]
	x = sort l

//Start = isMonotonic {6,5,4,4} // True
//Start = isMonotonic {1,3,2} // False
//Start = isMonotonic {1,2,4,5} // True
//Start = isMonotonic {1,1,1} // True

