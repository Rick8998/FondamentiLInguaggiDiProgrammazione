val satisfiesPropr: (List[Int], List[Int]) => (Int => Boolean) => List[Int] = (list, acc) => propr => list match {
  case Nil => acc
  case _ :: tail =>
    val l = tail.takeWhile(propr)       //lista più lunga di elementi che soddisfano propr
    if (l.length > acc.length) {
      satisfiesPropr(tail, l)(propr)          //metto in acc la nuova lista trovata con il takewhile
    } else {
      satisfiesPropr(tail, acc)(propr)        //la lista contenuta in acc è migliore di quella trovata
    }
}

satisfiesPropr(List(1, 2, 3, 6, 4, 9, 12, 15), Nil)(x => x % 3 == 0)