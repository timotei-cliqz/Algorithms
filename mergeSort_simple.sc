
def mergeSort(list: List[Int]): List[Int] = {
  def merge(l: List[Int], r: List[Int]): List[Int] = {
    (l,r) match {
      case (x,Nil) => x
      case (Nil,y) => y
      case (x::xs, y::ys) =>
        if (x > y) x :: merge(xs, y::ys)
        else y :: merge(x::xs, ys)
    }
  }

  val n = list.length/2
  if (n == 0) list
  else{
    val (l,r) = list splitAt n
    merge(mergeSort(l), mergeSort(r))
  }
}

val test_list = List(4,3,6,7,1,8,1,4,5,2,8,31)
mergeSort(test_list)