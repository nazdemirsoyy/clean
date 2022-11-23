module classes
import StdEnv

//Classes + Instances

class PlusMinx a
where 
	(+-) infixl 6 :: !a !a -> a
	(--) infixl 6 :: !a !a -> a
	zerox :: a
	
instance PlusMinx Char
where
	(+-) :: !Char !Char -> Char
	(+-) x y = toChar(toInt(x) + toInt(y))
	(--) x y = toChar(toInt(x) - toInt(y))
	zerox = toChar 0

//Start = 'a' +- 'e'

double1 :: a -> a |PlusMin a
double1 x = x+x

//Start = double1 2

::C = {re ::Real, im ::Real}

mkC n d = {re = n, im = d}
//Start = mkC 1.0 10.0

instance + C
where 
	(+) x y = mkC (x.re + y.re) (x.im + y.im)
	
//Start = mkC 2.2 4.1 + mkC 1.5 6.4 //(C 3.7 10.5)

instance - C
where 
	(-) x y = mkC (x.re - y.re) (x.im - y.im)

//Start = mkC 2.2 4.1 - mkC 1.5 6.4 //(C 0.7 -2.3)

instance * C
where
	(*) x y = mkC (x.re*y.re - x.im*y.im)(x.re*y.im + x.im*y.re)

//Start = mkC 2.2 4.1 - mkC 1.5 6.4 //(C 0.7 -2.3)

instance / C
where
	(/) x y
	| y.im == 0.0 = mkC (x.re/y.re)(x.im/y.re)
	= abort "Division not defined"

//Start = (mkC 2.0 4.0) / (mkC 2.0 0.0) //(C 1 2)

instance fromReal C
where
	fromReal r = mkC r 0.0

//Start :: C
//Start = fromReal 3.0 //(C 3 0)

instance toReal C
where
	toReal x
	| x.im == 0.0 = x.re
	= abort "x has imaginary part"
	
//Start = toReal (mkC 3.0 0.0) //3


instance abs C
where
	abs x = fromReal (sqrt(x.re*x.re + x.im*x.im))

//Start = abs (mkC 3.0 4.0) //(C 5 0)

instance toString C
where 
	toString x
	| x.im == 0.0 = toString x.re
	| otherwise = toString x.re +++ "+" +++ toString x.im +++ "i"

//Start = toString (mkC 3.0 4.0)//"3+4i"

instance == C
where
	(==) x y = x.re == y.re && x.im == y.im

//Start = mkC 1.0 2.0 == mkC 1.0 2.0 // True

/* 1. Given the Vector2 record type implement in a class 
operations with vectors. */

:: Vector2 = { x :: Real, y :: Real}

class Operations a
where 
	 (*==) :: a a -> Bool
	 (*<) :: a a -> Bool
	 (*>) :: a a -> Bool
     (*<=) :: a a -> Bool
     (*>=) :: a a -> Bool
     (!=) :: a a -> Bool
     
instance Operations Vector2
	where  
		(*==) :: Vector2 Vector2 -> Bool 
		(*==) v1 v2 = vlen v1 ==  vlen v2
		(*<) :: Vector2 Vector2 -> Bool 
		(*<) v1 v2 = vlen v1 < vlen v2
		(*>) :: Vector2 Vector2 -> Bool 
		(*>) v1 v2 = vlen v1 > vlen v2
		(*>=) :: Vector2 Vector2 -> Bool 
		(*>=) v1 v2 = vlen v1 >= vlen v2
	    (*<=) :: Vector2 Vector2 -> Bool 
		(*<=) v1 v2 = vlen v1 <= vlen v2
	    (!=) :: Vector2 Vector2 -> Bool 
		(!=) v1 v2 = vlen v1 <> vlen v2
		
vlen :: Vector2 -> Real 		  
vlen v = sqrt( v.x*v.x + v.y*v.y) 
 
//Start = {x = 1.0, y = 1.0} *== {x = 1.0, y = 1.0} // True
//Start = {x = 3.0, y = 4.0} *== {x = 5.0, y = 0.0} // True
//Start = {x = 1.0, y = 1.0} != {x = 2.0, y = 1.0} // True
//Start = {x = 1.0, y = 1.0} *< {x = 2.0, y = 1.0} // True
//Start = {x = 1.0, y = 1.0} *> {x = 2.0, y = 1.0} // False
//Start = {x = 1.0, y = 1.0} *<= {x = 1.0, y = 1.0} // True
//Start = {x = 1.0, y = 1.0} *>= {x = 1.0, y = 1.0} // True

/* 2. Implement in a class operations *+, *-, **, *!
and create instances for list of integers and strings. */

class Relations a
where 
	 (*+) :: a a -> a
	 (*-) :: a a -> a
	 (**) :: a a -> a
     (*!) :: a a -> a
     
instance Relations [Int]
	where  
		(*+) :: [Int] [Int] -> [Int] 
		(*+) l1 l2 = l1 ++ l2
		(*-) :: [Int] [Int] -> [Int] 
		(*-) l1 l2 = drop (length l2) l1
		(**) :: [Int] [Int] -> [Int] 
		(**) l1 l2 = [a*b \\ a <-l1 & b<-l2]
		(*!) :: [Int] [Int] -> [Int]
		(*!) l1 l2 = [a-b \\ a <-l1 & b<-l2]
		
//Start = [1..5] *+ [6..10] // [1,2,3,4,5,6,7,8,9,10]
//Start = [1..5] *- [6..8] // [4,5]
//Start = [1..5] ** [1..5] // [1,4,9,16,25]
//Start = [11..15] *! [1..5] // [10,10,10,10,10]
	
instance Relations String
	where  
		(*+) :: String String -> String
		(*+) s1 s2 = s1 +++ s2
		(*-) :: String String -> String 
		(*-) s1 s2 = {a \\ a<-:s1 & i<-[1..(size s2)]}
		(**) :: String String -> String
		(**) s1 s2 = {toChar(toInt(a)+toInt(b)) \\ a <-:s1 & b<-:s2}
		(*!) :: String String -> String
		(*!) s1 s2 = {toChar(toInt(a)-toInt(b)) \\ a <-:s1 & b<-:s2}

//Start = "hello" *+ "world!" // "helloworld!"
//Start = "funy functions" *- "hye" // "fun"
//Start = "123" ** "222" // "cde"
//Start = "abc" *! "111" // "012"










