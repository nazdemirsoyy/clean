module studyTree
import StdEnv


::Tree a=Node a (Tree a) (Tree a) | Leaf

//Tree1 see link: http://graphonline.ru/en/?graph=RDODcKkbEjpzIbIh
Tree1 ::Tree Int
Tree1 = Node 7 Leaf Leaf

//Tree2 see link: http://graphonline.ru/en/?graph=apYgfCbqYeaQRHNL
Tree2::Tree Int
Tree2 = Node 0 (Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))  (Node 2 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)) 

//Tree3 see link: http://graphonline.ru/en/?graph=YMMkGtZycajcoXEU
Tree3 ::Tree Int
Tree3 = Node 0 (Node 1 (Node 3 Leaf (Node 8 Leaf Leaf)) Leaf)  (Node 2 Leaf Leaf)


//Given a tree, find the number of it's nodes (non leaves)
sizeOfTree::(Tree a)->Int
sizeOfTree Leaf = 0
sizeOfTree (Node a l r) =1 + sizeOfTree l+ sizeOfTree r

//Start = sizeOfTree Tree1 //1
//Start = sizeOfTree Tree2 //7
//Start = sizeOfTree Tree3 //5

//Given a tree, find its depth
depth :: (Tree a) -> Int
depth Leaf = 0
depth (Node _ l r) = (max (depth l) (depth r)) + 1

getDepth :: Int (Tree a) -> Int
getDepth num Leaf = 0
getDepth num (Node x l r) 
| num == x = 1 + (getDepth x l) + (getDepth x r) 
= (getDepth x l) + (getDepth x r) 

Start = getDepth 7 Tree1

//Start = depth Tree1 //1
//Start = depth Tree2 //3
//Start = depth Tree3 //4

//Given a tree with key of type Int, find the sum of its nodes (leaf is 0)
sumNodes :: (Tree Int)->Int
sumNodes Leaf = 0
sumNodes (Node x l r) = x + sumNodes l + sumNodes r

//Start = sumNodes Tree1
//Start = sumNodes Tree2
//Start = sumNodes Tree3

//1 Inorder: Left, Root, Right

inorder::(Tree a)->[a] 
inorder Leaf = []
inorder (Node x le ri) =  inorder le ++ [x] ++ inorder ri

//Start = inorder Tree2

//2 Preorder: Root, Left, Right

preorder::(Tree a)->[a] 
preorder Leaf = []
preorder (Node x le ri) = [x] ++ preorder le ++  preorder ri
//Start = preorder Tree2

//3 Postorder: Left, Right, Root

postorder::(Tree a)->[a] 
postorder Leaf = []
postorder (Node x le ri) = postorder le ++  postorder ri ++ [x]
//Start = postorder Tree2

//1. Given a (Tree Int), and a list of Ints.
//Check if every element from the list is in the tree

task1::(Tree Int) [Int]->Bool
task1 x y = and [ isMember a z \\ a <- y] 
    where z = sort (inorder x)

//Start = task1 Tree2 [1..4]//True
//Start = task1 Tree2 [1..10]//False
//Start = task1 Tree3 [1..10]


//2.Given a (Tree Int) and an Int, 
//write a function which counts how many times
//the given number occurs in the tree
Tree3fiveTimes::Tree Int
Tree3fiveTimes = Node 3 (Node 3 Leaf (Node 3 Leaf (Node 2 Leaf Leaf))) (Node 3 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf))

task2::(Tree Int) Int-> Int
task2 x y = sum [1 \\ a<-(inorder x) | a==y]

task21 :: (Tree Int) Int-> Int
task21 Leaf y = 0
task21 (Node x le ri) y
| x==y = 1 + task21 le y + task21 ri y
= task21 le y + task21 ri y

//Start = task21 Tree3fiveTimes 3
//Start = task21 Tree2 (-10)

//3.Given a (Tree Int), write a function which gives back a list of triple tuples,
//where each tuple contains the value of the node, the left and the rigth child 
//of only the odd numbers from the tree in preorder traversal
//Leaf is considered to have value of -1

task3::(Tree Int)->[(Int,Int,Int)]
task3 Leaf = []
task3 (Node x le ri)
| isOdd x = [(x, extractN le, extractN ri)]++ task3 le ++ task3 ri
= task3 le ++ task3 ri

extractN :: (Tree Int) -> Int
extractN Leaf = -1
extractN (Node x l r) = x

//Start = task3 Tree2 
//Start = task3 Tree3 

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


