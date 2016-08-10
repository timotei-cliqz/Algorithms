val l = List(1,2,3,4,5)

def allCombinations(list:List[Int]): List[List[Int]] = {
  def combinations(k:Int) : List[List[List[Int]]] = {
    if (k == 0) List(List(List()))
    else {
      val co = for {
        c <- combinations(k - 1).head
        i <- list
      } yield i :: c
      List(co) ++ combinations(k-1)
    }
  }
  combinations(list.length) reduce((l1,l2) => l1 ++ l2)
}

allCombinations(l)