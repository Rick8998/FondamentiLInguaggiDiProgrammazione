def findSeq : (List[Option[Int]], List[Int]) => Option[List[Int]] = (lista, acc) => lista match{
  case Nil => Some(acc.reverse)
  case None :: _ => None
  case Some(el) :: tail => findSeq(tail, el :: acc)
}

val sequenza: List[Option[Int]] => Option[List[Int]] = lista => {
  findSeq(lista, Nil)
}

sequenza(List(Some(1), Some(2), Some(77)))
sequenza(List(Some(1), None, Some(77)))


val tentaVal: (=> Int) => Option[Int] = a =>
  try Some(a)
  catch {
    case _: Exception => None
  }


val parseInts:List[String]=>Option[List[Int]] = lista =>{
  sequenza(lista.map(el => tentaVal(el.toInt)))
}

parseInts(List("1", "2", "77"))
parseInts(List("1", "due", "77"))