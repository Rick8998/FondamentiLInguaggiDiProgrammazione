val differenze: List[Int] => List[Int] = lista => {
  lista.zip(lista.tail).map(el => el._1 - el._2)
}

differenze(List(3, 2, 7, 4, 4))