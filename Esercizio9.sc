//FOLD RIGHT
List(0, 1, 1, 1).foldRight(0)((el, acc) => if (el == 0) acc * 2 else acc * 2 + 1)


//FOLD LEFT x^n
val potenzaCalc: (Int, Int) => Int = (x, n) => if (n == 0) 1 else x * potenzaCalc(x, n - 1)

//el -> valore corrente
List(0, 1, 1, 1).foldLeft(0, 0)((acc, el) => {
  if (el == 1) (acc._1 + potenzaCalc(2, acc._2), acc._2 + 1) else (acc._1, acc._2 + 1)
})._1

/*
* if el == 1 then (acc._1 + 2^acc._2, acc._2+1)
* else (acc._1, acc._2 + 1)
* */