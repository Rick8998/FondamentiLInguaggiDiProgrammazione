import scala.annotation.tailrec

@tailrec
def numvocals(list:List[Char], countA:Int, countE:Int, countI:Int, countO:Int, countU:Int) : (Int, Int, Int, Int, Int) = list match {
  case 'a' :: tail => numvocals(tail, countA+1, countE, countI, countO, countU)
  case 'e' :: tail => numvocals(tail, countA, countE+1, countI, countO, countU)
  case 'i' :: tail => numvocals(tail, countA, countE, countI+1, countO, countU)
  case 'o' :: tail => numvocals(tail, countA, countE, countI, countO+1, countU)
  case 'u' :: tail =>  numvocals(tail, countA, countE, countI, countO, countU+1)
  case _ :: tail =>  numvocals(tail, countA, countE, countI, countO, countU)
  case Nil =>  (countA, countE, countI, countO, countU)
}

val chars : List[Char] => (Int, Int, Int, Int, Int) = list => numvocals(list, 0, 0, 0, 0, 0)

chars(List('a', 'b', 'c', 'd', 'e', 'f', 'h', 'i', 'a'))
//chars(List())