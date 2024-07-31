--Cheat Sheet for Haskell Programming language
--created by Adock90

--imports deafult libaries
import Data.Version
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude hiding (map)
import Control.Exception
import Control.Applicative
import qualified Prelude as P

--imports custom module
import LEN

--Monads
--3 basic laws
--Associativity - Can be showm in ( k>==>g ) >==>d =a == (s >=g)
--Right Identity Law - no return function chages the value nor in the monad (the value is immutable). Can be shown by op >=> return = op
--Left Identity Law - return cannot change value can be shown by return == op >=> op
class Monad m where  
   return :: a -> m a 
   (>>=) :: m a -> (a -> m b) -> m b 
   (>>) :: m a -> m b -> m b 
   fail :: String -> m a  
   fail msg = error msg 
--Applicative Functor
funct :: Float -> Float -> Float
funct x y = 45*x-7+y

--monoids
minus:: Float -> Float
minus x = x - x
di :: Float -> Float
di x = x / x

--Data.Map init
m :: Int -> Map Int [Int]
m n = Map.fromList (map makePair [1..n])
    where makePair x = (x, [x])


--function composition
--gets integer and evaluates true or flase in div4
--gets bool and turns it into a string in result
div4 :: Int -> Bool
result :: Bool -> String

div4 x = if x `rem` 4 == 0
    then True 
else False 
result x = if x == True
    then "Divisable by 4"
else "Not divisable"

-- high order function
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap func(x : abc) = func x : mymap func abc

--string declaration
lol = "lol" :: [Char]

--Guards 
jo :: Integer -> Integer
jo n | n == 0 = 1
     | n /= 0 = n * jo (n-1)

--calculates pi
data Area = Circle Float Float Float
surface :: Area -> Float
surface (Circle _ _ r) = pi * r ^ 2

--string print to int
ri :: String -> Int
ri = read
--where function
asa :: (Float, Float, Float) -> (Float, Float)
asa (a,b,c) = (x1, x2) where
    x1 = e + sqrt d / (2 * a)
    x2 = e - sqrt d / (2 * a)
    d = b * b - 2 * a * c 
    e = - b / (2 * a)


--function for doubling number math
double :: Int -> Int
double x = x * 890

nwe :: Int -> Int -> Int
nwe x y = (x * x) * y

qq :: Integer -> Integer
qq x = x * 32312313123123123123

op :: Float -> Float
op x = x / x

main :: IO()
main = do
    -- expressions
    let var1 = 2
    let var2 = 3
    putStrLn "2 + 3 ="
    print(var1 + var2)

    putStrLn "2 - 3 ="
    print(var1 - var2)

    putStrLn "2 * 3"
    print(var1*var2)

    putStrLn "2 / 3"
    print(var1/var2)
    --simple print statement
    print ( double 78 )

    --prints numbers 1 - 10
    print [1..10]
    -- if and else statement
    if (double 89) == (double 89)
        then print ( qq 89 )
    else if (double 89) /= (double 90)
        then putStrLn "OHHHHH"
    else putStrLn "Opll"
    
    --multipilcaiton
    print (nwe 223 233)

    --division
    print (op 56.7 / 879.9)

    --bool
    print ( 56 == 89 )

    print ( show[1..35])

    print (ri "434")

    print (succ 23434)

    --prints max and min in range
    print (maxBound :: Int)
    print (minBound :: Int)

    --prints number as int and float
    print (87 :: Int)
    print (87 :: Float)

    --jo implimentation
    print (jo 15)

    --where implimentation
    print (asa(1,3,-3))

    --higher order function implimentation
    putStrLn $ map toLower "Oww"

    --lambda expression
    putStrLn "Sucessor of 323 is:"
    print ((\x -> x + 1) 323)

    --head functions
    let x = [1..100]
    --prints list
    putStrLn "Our list:"
    print (x)
    --prints first number
    putStrLn "1st number:"
    print (head x)
    --prints the tail/2nd number in list
    putStrLn "The tail is"
    print (tail x)
    --prints last number of the list
    putStrLn "Last number is"
    print (last x)
    --displays last entry
    putStrLn "without last entry"
    print (init x)
    --checks if the list is empty
    putStrLn "is our list empty"
    print (null x)
    --prints it in reverse
    putStrLn "Reversed"
    print (reverse x)
    --prints length of a list
    putStrLn "Length"
    print (length x)

    --take function(ends at 33) 
    print(take 33 ([12..121]))

    --drop function (starts at 21)
    print (drop 21([12..121]))

    --maximum function
    let x = [1,32,432,-132]
    print (maximum x)

    --minimum function
    print (minimum x)

    --sum function(returns summation)
    let x = [2..8]
    putStrLn "summation of list"
    print (sum x)

    --product function (multiplication of all numbers in the list)
    let x = [89..120]
    putStrLn "Product of all elements are"
    print (product x)

    --elem function (returns true or false if a value is in a list)
    let x = [21,323,4234,432,782,7766]
    putStrLn "contains 3231?"
    print (elem 7766 (x))
    --returns true

    --function composition implimentation
    putStrLn "Showcase of Function Composition in Haskell"
    print ((result.div4)(16))

    --Map Module init
    print(m 3132)

    --Data.List init
    print(intersperse '!' "ehwhHIhooudp")
    print(intercalate " " ["help", "me","please"])
    print(splitAt 3 "wquwqoqwouwoqwjoq")
    print (sort [45,23,76,32,9044,432,123,767,1454])


    --Data.Char init
    print(toUpper 'f')
    print(toLower 'F')
    print(words "Please Give Me FOOD")

    --Data.Set Init
    let str1 = "Oh WOW"
    let str2 = "My Goodness"

    let set1 = Set.fromList str1
        set2 = Set.fromList str2
    print(set1)
    print(set2)

    --uses custom module
    print(len "hioo")

    --reads file
    let file = "454.txt"
    contents <- readFile file
    putStrLn contents

    --writes to file
    writeFile file "Hello Again"
    readFile file
    
    --try and except block
    result <- try (evaluate(122 * 212)) :: IO(Either SomeException Int)
    case result of
        Left ex -> putStrLn $ "Opps" ++ show ex
        Right val -> putStrLn $ "ANSWER = " ++ show val

    --built in functor showcase 
    print(map (+1) [3,7,8,90,455])
    print(fmap (+1) [3,7,8,90,455])

    --functor showcase 2
    print(fmap (+21212)(Just 52))
    print(fmap (+21212) Nothing)

    --applicative functior init
    print(show $ funct <$> (Just 3) <*> (Just 98))

    --monoid init
    print(minus 21.32)
    print (di 121.2121)

