//INSTANCES--->
//Defining the behaviour of some types for different operators
//Start = "Hello "+"World"

instance + String
where
   (+) s1 s2 = s1 +++ s2
//Start = "Hello "+"World"

//Make an instance of the operator - for 
//lists of Int such that [1,2,3]-[2,2,2,3]=[1]
//Start = [1,2,3]-[2,2,2,3]

instance - [Int]
where
    (-) a b = [x \\ x<-a | not (isMember x b)]
     
//Start :: [Int]
//Start = [1,2,3]-[2,2,2,3]	

//Write an instance of operator + for 
//lists of Int such that [1,2,3]+[2,2,2,3]=[3,4,5]

instance + [Int]
where
    (+) a b = [x+y \\ x<-a & y<-b]

//Start = [1,2,3]+[2,2,2,3]	
