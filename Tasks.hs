
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Use map once" #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}



-- Task 1
toString :: Float -> String
toString = printf "%.2f"

compute_average_row :: Row -> Row
compute_average_row m = (head m):(toString ((steps_person m)/8)):[]

compute_average_steps :: Table -> Table
compute_average_steps (x:xs) = ["Name","Average Number of Steps"]:(map compute_average_row xs)


-- Task 2

-- Number of people who have achieved their goal:
steps_person :: Row -> Float
steps_person (x:xs) = foldr (\x acc -> acc + (read x)) 0 xs

get_passed_people_num :: Table -> Int
get_passed_people_num m = foldr f 0 (tail m)
  where f x acc = if steps_person x > 1000 then 1+acc else acc


-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = a/b
  where a = fromIntegral (get_passed_people_num (tail m)) :: Float
        b = fromIntegral (length (tail m)) :: Float


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = (foldr f 0 (tail m))/len
  where f x acc = (steps_person x) + acc
        len = fromIntegral (length (tail m)) :: Float

-- Task 3

avg_steps_per_h :: Table -> [Float]
avg_steps_per_h m = foldr f [0,0,0,0,0,0,0,0] (tail m)
  where f x acc = accumulate (tail x) acc
        accumulate x acc = zipWith (\x y -> y + (read x)) x acc

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"]:foldr f [] (avg_steps_per_h m):[]
  where f x acc = toString (x/len):acc
        len = fromIntegral (length (tail m)) :: Float

-- Task 4
accum1 :: Row -> [[Int]] -> [[Int]]
accum1 l x = zipWith f l x
  where f el [x,y,z]
          | (read el) < 50 = [x+1,y,z]
          | (read el) < 100 = [x,y+1,z]
          | otherwise = [x,y,z+1]


activ_summary :: Table -> [[Int]]
activ_summary m = foldr f [[0,0,0],[0,0,0],[0,0,0]] (tail m)
  where f x acc = accum1 (drop 3 x) acc

toStr1 :: [[Int]] -> Table
toStr1 [x,y,z] = ["VeryActiveMinutes":(go x), "FairlyActiveMinutes":(go y), "LightlyActiveMinutes":(go z)]
  where go = foldr (\x acc -> (show x):acc) []


get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"]:toStr1 (activ_summary m)


-- Task 5

strAux1 :: Integer -> String -> Integer -> String -> Ordering
strAux1 p1 n1 p2 n2
            | p1 < p2 = LT
            | p1 > p2 = GT
            | otherwise = if n1 <= n2 then LT else GT

strcmpT :: Row -> Row -> Ordering
strcmpT [n1,p1] [n2,p2] = strAux1 (read p1) n1 (read p2) n2


get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"]: sortBy strcmpT (map (\x -> take 2 x) (tail m))


-- Task 6

transform :: Row -> Row
transform [n,h1,h2,h3,h4,h5,h6,h7,h8] = [n, toString s1,toString s2,toString s3]
  where s1 = (read h1 + read h2 + read h3 + read h4)/4
        s2 = (read h5 + read h6 + read h7 + read h8)/4
        s3 = abs (s1-s2)

strAux2 :: Float -> String -> Float -> String -> Ordering
strAux2 p1 n1 p2 n2
            | p1 < p2 = LT
            | p1 > p2 = GT
            | otherwise = if n1 <= n2 then LT else GT

strcmpD :: Row -> Row -> Ordering
strcmpD [n1,_,_,d1] [n2,_,_,d2] = strAux2 (read d1) n1 (read d2) n2

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : sortBy strcmpD (map transform (tail m))

-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f = map (\x -> map f x)


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s:map  f m


get_sleep_total :: Row -> Row
get_sleep_total (n:r) = n: (toString (foldr f 0 r) : [])
  where f x1 n = (read x1) + n



{-
    TASK SET 2
-}

-- Task 1

strAux2_1 :: Row -> Row -> Value -> Value -> Ordering
strAux2_1 (x:xs) (y:ys) xv yv
            | n1 < n2 = LT
            | n1 > n2 = GT
            | otherwise = if xv <= yv then LT else GT
            where n1 = (read x)::Double
                  n2 = (read y)::Double

strcmp2_1 :: Int -> Row -> Row -> Ordering
strcmp2_1 n x y = strAux2_1 (drop n x) (drop n y) (head x) (head y)

searchName :: ColumnName -> Row -> Int
searchName s [] = 1
searchName s (x:xs)
            | x == s = 0
            | otherwise = 1 + searchName s xs

tsort :: ColumnName -> Table -> Table
tsort s m = head m : sortBy (strcmp2_1 n) (tail m)
  where n = (searchName s (head m))


-- Task 2
searchTable :: String -> Table -> Bool
searchTable s [] = False
searchTable s (x:xs)
              | s == (head x) = True
              | otherwise = searchTable s xs

vunion :: Table -> Table -> Table
vunion = foldl f
  where f acc x = if searchTable (head x) acc then acc else acc ++ [x]

-- Task 3
voidRowConstruct :: Int -> Row
voidRowConstruct 0 = []
voidRowConstruct n = "" : voidRowConstruct (n-1)

unionAux :: Table -> Table -> Table
unionAux = zipWith (\r1 r2 -> r1 ++ r2)

hunion :: Table -> Table -> Table
hunion t1 t2
            | (length t1) > (length t2) = foldl f1 (unionAux t1 t2) (drop (length t2) t1)
            | (length t1) < (length t2) = foldl f2 (unionAux t1 t2) (drop (length t1) t2)
            | otherwise = unionAux t1 t2
  where f1 acc x = acc ++ [x ++ voidRowConstruct (length (head t2))]
        f2 acc x = acc ++ [voidRowConstruct (length (head t1)) ++ x]

-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = t1

-- Task 5

c1 :: (Row -> Row -> Row) -> Row -> Table -> Table
c1 op r = foldr (\x acc -> (op r x):acc) []

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names:(foldr f [] (tail t1))
  where f x acc = (c1 new_row_function x (tail t2)) ++ acc

-- Task 6

prj :: ColumnName -> Table -> Table
prj s t = foldr f [] t
  where f x acc = [(x !! n)] : acc
        n = searchName s (head t)

projection :: [ColumnName] -> Table -> Table
projection col_names t = foldl f [] col_names
  where f [] s = (prj s t)
        f acc s = unionAux acc (prj s t)

-- Task 7

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t):(foldr f [] (tail t))
  where n = searchName key_column (head t)
        f x acc
          | condition (x !! n) = x:acc
          | otherwise = acc

