val fact : Int => Int = num => {
  if(num == 0) 1 else 1.to(num).reduce(_ * _)
}

fact(10)