[hanus|
procedure divide'(x :: Int, y :: Int, z :: Int){
    from x < y loop
        x += y;
        z -= 1;
    until x >= y && z == 0;
}|]
