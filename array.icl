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
