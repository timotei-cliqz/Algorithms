
def mergeSort[T](list: List[T])(implicit predicate: (T,T) => Boolean) : List[T] = {
  val n = list.length/2
  if (n == 0) list
  else {
    def merge(l: List[T], r: List[T]) : List[T] = {
      (l,r) match {
        case (Nil, x) => x
        case (y, Nil) => y
        case (x::xs, y::ys) =>
          if(predicate(x,y)) x :: merge(xs,r)
          else y :: merge(l, ys)
      }
    }
    val(fst,snd) = list splitAt n
    merge(mergeSort(fst), mergeSort(snd))
  }
}

val test_list = List(4,3,6,7,1,8,1,4,5,2,8,31)
mergeSort(test_list)((l,r) => l > r)
