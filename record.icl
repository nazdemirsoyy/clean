module record
import StdEnv

/*
	Record is a way to collect multiple pieces of data 
*/

:: Point = { x :: Int, y :: Int}

p1::Point
p1 = {x=3,y=4}

origin::Point
origin = {x=0,y=0}

distance :: Point Point -> Real
distance a b = sqrt(toReal((x1 - x2)^2 + (y1 - y2)^2))
where
	x1 = a.x
	y1 = a.y
	x2 = b.x
	y2 = b.y
	
//Start = distance origin p1 //5

/// Instances
instance + Point where (+) a b = {x = a.x + b.x, y =a.y + b.y}
instance - Point where (-) a b = {x = a.x - b.x, y =a.y - b.y}
	
//Start = p1 + p1

addPoint::Point Point -> Point
addPoint a b = {x = a.x + b.x, y =a.y + b.y}

//Start = addPoint p1 p1 

(addPoint2)::Point Point -> Point
(addPoint2) a b = {x = a.x + b.x, y =a.y + b.y}

//Start = p1 addPoint2 p1 

//For sum to work we need zero
instance zero Point where zero = {x=0,y=0}
//Start = sum [p1,p1,origin]

//isMember require Eq 
instance == Point where (==) a b = a.x == b.x && a.y == b.y
//Start = isMember p1[p1,origin]

::Date = { month :: Int, day :: Int}
:: Person = { name :: String, age ::Int , birthday::Date}

Evan :: Person
Evan = {name = "Evan", age = 1 , birthday ={month = 12, day = 9}}
Hossan :: Person
Hossan = { name = "Hossan" , age =69, birthday = {month = 3, day = 12}}

Tringa :: Person
Tringa = { name = "Tringa" , age =55, birthday = {month = 11, day = 22}}


instance == Date
where 
     (==) a b = a.month ==b.month && a.day == b.day

instance == Person 
where 
     (==) a b = a.name == b.name && a.age == b.age && a.birthday == b.birthday
     
instance < Person 
where 
     (<) a b =  a.age < b.age  

//Start = Evan == Evan

//Start = sort[Evan,Tringa,Hossan]

newBirth :: Person -> Person
newBirth a = { a & age = a.age+1}

//Start = newBirth Evan

instance toString Person
where
    toString a = a.name
    
Start = toString Evan

instance toInt Person 
where 
    toInt a = a.age


