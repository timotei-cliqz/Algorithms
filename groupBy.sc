
val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' ->"WXYZ")

val charCode : Map[Char,Char] = for ((dig,str) <- mnem; ltr <-str) yield ltr -> dig

def wordCode(word: String): String = word map charCode

val wordsForNum: Map[String, Seq[String]]

def numToStr(num:String) :String = {
  num flatMap(dig => mnem(dig).toList)
}

val list = List("ARE", "ASE", "CRD","KN","LO") //"273" AND "56"

(list map wordCode).toSet

list filter(word => wordCode(word) == "273")

list partition(str => str contains('a'))

def group(list:List[String])(f: String => String) : Map[String, Seq[String]] = {
  val setOfAllNumbersInList = (list map f).toSet
  val set =  for {
    num <- setOfAllNumbersInList
    list_num = list filter (word => f(word) == num)
  } yield (num, list_num)
  set toMap
}

group(list)(wordCode)