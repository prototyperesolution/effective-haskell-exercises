
--Peano arithmetic stuff
data Peano = Z | S Peano deriving Show

addPeanos :: Peano -> Peano -> Peano
addPeanos p p' =
    case (p, p') of
        (Z,Z) -> Z
        (S xs, Z) -> S (addPeanos xs Z)
        (Z, S xs) -> S (addPeanos Z xs)
        (S xs, S ys) -> S ( S (addPeanos xs ys))

--Tree exercises

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

--show all the strings in a string tree in a nice way

showStringTree :: BinaryTree String -> String
showStringTree tree =
    case tree of
        Leaf -> " "
        Branch tree' str tree'' -> str <> showStringTree tree' <> showStringTree tree''

-- adds a new int to a binary tree of ints

addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree tree num =
    case tree of
        Leaf -> Branch Leaf num Leaf
        Branch tree' val tree'' -> addElementToIntTree tree' num


-- check if an int exists in a binary tree of ints

doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist tree num =
    case tree of
        Leaf -> False
        Branch tree' val tree'' ->
            case val == num of
                True -> True
                False -> findTrue (doesIntExist tree' num) (doesIntExist tree'' num)
                where
                    findTrue :: Bool -> Bool -> Bool
                    findTrue True _ = True
                    findTrue _ True = True
                    findTrue _ _ = False


