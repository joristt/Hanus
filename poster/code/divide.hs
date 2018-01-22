[hanus|
procedure divide(x :: Int, y :: Int, z :: Int){
    from x >= y && z == 0 loop
        z += 1;
        x -= y;
    until x < y;
}|]

