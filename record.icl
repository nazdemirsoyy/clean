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
:: Vector2 = {xVec :: Real, yVec :: Real}

collinear :: Vector2 Vector2 Vector2 -> Bool
collinear {xVec = x1, yVec=y1} {xVec = x2 , yVec=y2} {xVec = x3,yVec=y3} = (x1 * y2 + x2 * y3 + x3 * y1 - x1 * y3 - x2 * y1 - x3 * y2)/ 2.0 == 0.0


//Start = collinear {xVec = 0.0, yVec = 0.0} {xVec = 1.0, yVec = 0.0} {xVec = 3.0, yVec = 0.0} // True
//Start = collinear {x = 0.0, y = 0.0} {x = 1.0, y = 0.0} {x = 3.0, y = 1.0} // False
// // Start = collinear {x = 0.0, y = -1.0} {x = 2.0, y = 0.0} {x = 3.0, y = 0.0} // False


// Given a list of distinct name and a list of grades.
// Generate a list of `Person` type
// the grades of all Person should be the average of the 2nd list.

:: Person = { name :: String, gpa :: Real }

Generator :: [String] [Int] -> [Person]
Generator list grades = map (\x = {name = x , gpa = average}) list
where
	average = avg (map toReal grades)

// Start = Generator ["p1", "p2"] [1, 4] // [(Person "p1" 2.5), (Person "p2" 2.5)]
// Start = Generator ["p1", "p2", "p3"] [1, 4, 7] // [(Person "p1" 4), (Person "p2" 4), (Person "p3", 4)]


::Student = {id::Int,uni::String,grades::[Int]}

David::Student
David={id=111111,uni="ELTE", grades =[2,3,4]}
David2::Student
David2={id=222222,uni="ELTE", grades =[3,4,5]}
Peter::Student
Peter = {id=123456,uni="ELTE",grades=[1,2,3,1,3,4,2,3,4,5]}
Mark::Student
Mark = {id=134326,uni="ELTE",grades=[1,2,5,5,5,5,3,3,4,2,3,4,5]}
John::Student
John = {id=133526,uni="BME",grades=[1,1,1,1,1,5,3,3,4,2,3,4,5]}
Sara::Student
Sara = {id=444326,uni="BME",grades=[1,2,5,5,5,2,2,3,4,5,1,1,1,1,2,4,5]}
Ana::Student
Ana = {id=134326,uni="Corvinus",grades=[1,1,1,1,1,2,2,2,2,1,1,4,4,4,4,5,5,5,5]}
Leo::Student
Leo = {id=555555,uni="ELTE",grades=[1,2,5,5,5,5,3,3,2,2,2,2,2,2,2,2,3,4,5]}
Jane::Student
Jane = {id=134536,uni="Corvinus",grades=[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]}

//Given an array of students, return an array of the names of 
//the universities(remove the duplicate names)

getUni :: Student -> String
getUni {uni} = uni

uniNames::{Student}->{String}
uniNames arr = {/*getUni a*/ a.uni \\ a<-: arr}

//Start = uniNames {Leo,Jane,Sara,Sara,Mark,John}
//Start = uniNames {Leo\\a<-[1..10]}



//Given an array of Students. Find the id of the student with the lowest gpa. 

studentGpa :: Student -> Real
studentGpa student = avg listOfRealGrades
where
	listOfRealGrades = (map toReal student.grades)

lowestGpaAux :: [Student] Student -> Student
lowestGpaAux [] lowestgpaStudent = lowestgpaStudent
lowestGpaAux [john:xs] mark
|studentGpa mark < studentGpa john = lowestGpaAux xs mark 
= lowestGpaAux xs john

lowestGpa2::{Student} -> Int
lowestGpa2 arr = (lowestGpaAux xs x).id
where
	[x:xs] = arrayToList arr

//Start = lowestGpa2 {Mark,John,Leo,Jane,Ana}//133526
//Start = lowestGpa2 {Leo,Jane,Ana}//134536
//Start = lowestGpa2 {Leo\\a<-[1..10]}//555555


//heap = memory 


//Given an array of Students and an integer n, 
//return a list of tuples like (id,uni) of the students that have more than n grades

moreThanNGrades::{Student} Int ->[(Int,String)]
moreThanNGrades arr n = map (\student = (student.id, student.uni)) (filter (\student = length(student.grades) > n) list)
where
	list = arrayToList arr

//Start = moreThanNGrades {Jane, David, David2, Peter} 3 //[(134536,"Corvinus"),(123456,"ELTE")]


//Write a function which takes a University name and array of students 
//and calculates the total gpa of the University
//Gpa of a University is the sum of all the grades of all the students over 
//the total number of grades

gpaUni::String {Student}->Real
gpaUni ouruni arr = grades
where 
	list = arrayToList arr
	students = filter (\student = student.uni == ouruni) list
	grades =  avg (map toReal (flatten (map (\student = student.grades) students)))
	

//Start = gpaUni "ELTE" {David,David2,Jane,Ana,Sara} // 3.5
//Start = gpaUni "ELTE" {Mark,Jane,Mark} // 3.61538461538462

a :: {Int}
a = {1,2,3,4}


//Start = size  a


//Given an array of Students, return a tuple containing 
//the names of the universities with the lowest and highest gpa
//(LowestGpa,HighestGpa)
//hint: Use the previous function
//The inputted list is not empty

lowest :: [Student] {Student} -> String
lowest [markOrJane] _ = markOrJane.uni
lowest [mark,jane:xs] arr 
| gpaUni mark.uni arr < gpaUni jane.uni arr =  lowest [mark:xs] arr
= lowest [jane:xs] arr

highest :: [Student] {Student} -> String
highest [markOrJane] _ = markOrJane.uni
highest [mark,jane:xs] arr 
| gpaUni mark.uni arr > gpaUni jane.uni arr =  highest [mark:xs] arr
= highest [jane:xs] arr

minMaxUni::{Student}->(String,String)
minMaxUni arr = (lowest list arr , highest list arr)
where
	list = arrayToList arr


//Start = minMaxUni {Mark,Jane,David,David2,Leo,Ana,Sara} // ("Corvinus","ELTE")

//lowest mark.uni {Mark,Jane,David,David2,Leo,Ana,Sara}


::Day = Mon|Tue|Wed|Thu|Fri | Sat|Sun

::Exam =  Midterm|Endterm|MidtermRetake|EndtermRetake

//Given two exams, check if they are the same


compareExams :: Exam Exam ->Bool
compareExams Midterm Midterm = True
compareExams Endterm Endterm = True
compareExams MidtermRetake MidtermRetake = True
compareExams EndtermRetake EndtermRetake = True
compareExams  _ _ = False


//Start = compareExams Midterm Midterm
//Start = compareExams Midterm Endterm

toString::Exam->String
toString Midterm = "Midterm"

::Class={className::String,day::Day}
Math::Class 
Math = {className = "MATH", day=Mon}
IMP::Class
IMP = {className = "IMP", day =Tue}
FP::Class
FP = {className = "FP", day = Fri}
PRO::Class
PRO = {className = "PRO", day = Mon}
CS::Class
CS = {className = "CS", day = Fri}

//5.Given a list of Classes(see above) and a Day n
//Return a list containing only the Classes that are 
//held on Day n
//The compare days function is given

dayEqual :: Day Day -> Bool
dayEqual Fri Fri = True
dayEqual _ _ = False

classesOnDay::[Class] Day->[String]
classesOnDay list day = map (\x= x.className)(filter (\x = dayEqual x.day day) list)

//Start = classesOnDay [Math,IMP,CS,PRO,FP] Fri

:: Student = {name :: String
             ,id :: String
             ,grades :: {Int}}

/* 
 * Write a function oddStudents that takes array of students, and returns the names of the oddStudents.
 * A student is odd if
 * the sum of their grades is smaller than 100 and is an odd number.
 */

oddStudents :: {Student} -> [String]
oddStudents arr = [a.name \\ a<-:arr |sum [b \\ b<-: a.grades] < 100 && isOdd  (sum [b \\ b<-: a.grades])  ]

// Intended for tests. Do not remove!
student1 = {name="a",id="st1",grades={20,40,13}}
student2 = {name="b",id="st2",grades={50,13,10,42}}
student3 = {name="c",id="st3",grades={13,70}}
student4 = {name="d",id="st4",grades={}}

//Start = oddStudents {} // []
// Start = oddStudents {student1} // ["a"]
//Start = oddStudents {student1, student2, student3, student4} // ["a","c"]
//Start = oddStudents {student4} // []
