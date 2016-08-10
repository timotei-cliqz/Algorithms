//Source: Martin Odersky's course on Coursera: "Functional Programming Principles in Scala"
//the source code was not provided in the course, so this might be helpful to those who want to look at it without having to write it down from the video
//TO DO: add a brief explanation of how it works
def queens (n:Int) : Set[List[Int]] = {

  def isSafe(col:Int, queens: List[Int]) : Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r,c) => col != c && math.abs(col - c) != row - r
    }
  }

  def placeQueens(k:Int) : Set[List[Int]] = {
    if (k==0) Set(List())
    else{
      for {
        queens <- placeQueens(k-1)
        col <- 0 until n
        if isSafe(col,queens)
      }yield  col :: queens
    }
  }
  placeQueens(n)
}

def show(queens : List[Int]) = {
  val lines =
    for(col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col,"X ").mkString
  "\n" + (lines mkString "\n")
}

(queens(4) map show) mkString "\n"
