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
