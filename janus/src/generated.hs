glob_var1
glob_var2
run_0 = do 
{
  let {
    glob_var1 :: Int = 10; 
    glob_var2 :: Int = 1
  };
  main glob_var1 glob_var2
}

main (glob_var1 :: Int) (glob_var2 :: Int) = do {
  let {
    tmp_0 = (\(StdLib.Operator.Operator fwd_1 _) -> fwd_1) (+=) glob_var2 10
  };
  let {
    glob_var2 = tmp_0
  };
  
  let {
    tmp_2 = substract glob_var1 glob_var2 (\(glob_var1 :: Int, glob_var2 :: Int) -> glob_var1, \v_3 (glob_var1 :: Int, glob_var2 :: Int) -> (v_3, glob_var2))
  };
  let {
    (glob_var1 :: Int, glob_var2 :: Int) = tmp_2
  };
  (glob_var1, glob_var2)
}

substract (glob_var1 :: Int) (glob_var2 :: Int) (arg1 :: Int) = do {
  let {
    tmp_0 = (\(StdLib.Operator.Operator fwd_1 _) -> fwd_1) (-=) glob_var1 glob_var2
  };
  let {
    glob_var1 = tmp_0
  };
  (glob_var1, glob_var2, arg1)
}
