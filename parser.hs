import System.Environment
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

main :: IO ()
main = do
    [arg1,arg2] <- getArgs
    src <- readFile arg1
    res <- return (operate (words src))
    _ <- writeFile arg2 res
    return ()

operate :: [String] -> String
operate src = let
  lst = index_name src
  in (printGenders lst) ++ (transform src (Map.fromList lst))


-- вывод предикатов пола
printGenders :: [(String, (String, String))] -> String
printGenders lst
  | lst == [] = ""
  | otherwise = gender (snd (snd (head lst))) (fst (snd (head lst))) ++ printGenders (tail lst)

-- предикаты пола
gender :: String -> String -> String
gender who man = who ++ "(" ++ man ++ ")\n"

-- создание предикатов родителей
transform :: [String] -> Map String (String, String) -> String
transform src lst
  | src == [] = ""
 -- | head src == "0" && length src > 2 = head (tail (tail src)) ++ "\n" ++ transform (tail src) lst
  | head src == "0" && length src > 2 &&
    head (tail (tail src)) == "FAM" = let
      father = parent "HUSB" (tail src) lst
      mother = parent "WIFE" (tail src) lst
      children = child (tail src) lst
      in concat (map (merge "mother" mother) children) ++ concat (map (merge "father" father) children) ++ transform (tail src) lst
  | otherwise = transform (tail src) lst

-- предикаты родителей
merge :: String -> String -> String -> String
merge who parent childs = who ++ "(" ++ parent ++ ", " ++ childs ++ ")\n"

-- список детей в семье
child :: [String] -> Map String (String, String) -> [String]
child src lst
  | src == [] = []
  | head src == "0" = []
  | head src == "CHIL" = let
      index = head (tail src)
      name = what_name index lst
      in [name] ++ child (tail src) lst
  | otherwise = child (tail src) lst

parent :: String -> [String] -> Map String (String, String) -> String
parent who src lst
  | src == [] = ""
  | head src == who = let
      index = head (tail src)
      name = what_name index lst
      in name
  | otherwise = parent who (tail src) lst

-- определение имени по индексу 
what_name :: String -> Map String (String, String) -> String
what_name y xs = fst (fromJust (Map.lookup y xs))

-- определение пола по индексу
what_gender :: String -> Map String (String, String) -> String
what_gender y xs = snd (fromJust (Map.lookup y xs))

-- создание словаря (индекс - имя, пол)
index_name :: [String] -> [(String, (String, String))]
index_name src
  | src == [] = []
  | head src == "0" && length src > 2 &&
    head (tail (tail src)) == "INDI" = let
      index = head (tail src)
      name = findName src
      gend = findGender src
      in [(index, (name, gend))] ++ index_name (tail src)
  | otherwise = index_name (tail src)

findName :: [String] -> String
findName man
  | man == [] = ""
  | head man == "NAME" = let
      first = head (tail man)
      second = head (tail (tail man))
      in dropNonLetters first ++ " " ++ dropNonLetters second
  | otherwise = findName (tail man)

findGender :: [String] -> String
findGender man
  | man == [] = ""
  | head man == "SEX" =
      if head (tail man) == "M" then "male"
        else "female"
  | otherwise = findGender (tail man)

-- убирает лишние символы у фамилий
dropNonLetters :: String -> String
dropNonLetters xs = (filter (\x -> isLetter x || isSpace x)) xs 

