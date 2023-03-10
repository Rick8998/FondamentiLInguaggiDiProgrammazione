val insertInOrder : (List[Int], Int) => List[Int] = (list, num) => {
  (list.takeWhile(_ < num):+ num).appendedAll(list.dropWhile(_ < num))
}

insertInOrder(List(1,4,6,9,10,24), 7)