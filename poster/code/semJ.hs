count :: BinaryTree a -> Int
[hanus|
    a :: BinaryTree Int;
    procedure main() {
        createNode a;
        a.value += (count a.left) + a.value
    }
|]
