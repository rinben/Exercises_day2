object ExercisesDay02 {
  def main(args: Array[String]): Unit = {
    //Exercise 01
    println(palindrome("Sit on a potato pan, Otis."))
    println(palindrome("Napa racecar a pan"))
    println(Tiffany_palindrome("Sit on a potato pan, Otis."))
    println(Tiffany_palindrome("Napa racecar a pan"))
    println()
    //Exercise 02
    println(fib_seq(20))
    println(fib_seq(5))
    println(fib_seq(1))
    println(fib_seq(71))
    println()
    //Exercise 03
    println(reversedString("hello world it is I Ben Rinaldo"))
    println(reversedString("My name is Jim"))
    println()
    //Exercise 04
    println(memberNames())
    //Exercise 05

    //Exercise 06

    //Exercise 07

    //Exercise 08

    //Exercise 09

    //Exercise 10

  }
  //Exercise 01
  def palindrome(str:String):String = {
    val a = List(',', '.', '?', '!', ' ', ';', ':','\'')
    val newStr = str.toLowerCase()
    var start_list=List[Char]()
    for (i<-newStr if !(a contains i)){
      start_list=i::start_list
    }
    var end=""
    val ret_list = start_list.reverse
    for (j<-0 until ret_list.length){
      if (start_list(j)!=ret_list(j)){
        end="The line"+" '"+str+"'"+" is not a palindrome"
      }
      else {
        end="The line"+" '"+str+"'"+" is a palindrome"
      }
    }
    end
  }
  //Tiffany's palindrome function:
  def Tiffany_palindrome(str:String): Boolean = {

    var delimArray = Array(',',' ', '.', '/', '"')
    var newArray = str.split(delimArray)
    var newStr = new String()
    for (i <- newArray){
      newStr = newStr + i.toLowerCase
    }

    isPalindrome(newStr)

  }

  def isPalindrome(newStr: String): Boolean = {
    if (newStr.length == 1 || newStr.length == 0) {
      true
    }
    else {
      var lastIndex = newStr.length - 1
      if (newStr(0) == newStr(lastIndex)) {
        isPalindrome(newStr.substring(1, lastIndex - 1))
        return true
      }
      false
    }
  }
  //Exercise 02
  def fib_seq(limit:Int):String={
    var Fib=List(1,1)
    var n=1
    do {
      if (limit==1){
        return "The Fibonacci sequence is: %s %d\n".format(1,1)
      }
      else if (limit==0){
        return "The Fibonacci sequence is: \n"
      }
      Fib=Fib:+(Fib(n)+Fib(n-1))
      n=n+1
    } while ((Fib(n)+Fib(n-1))<=limit)
    print("The Fibonacci sequence is: ")
    for (i<-Fib){
      print(i+" ")
    }
    "\n"
  }

  //Exercise 03
  def reversedString(str:String):String={
    val badStr=List(',','.','!',';','?',':')
    var newstring=""
    for (j<-str if !(badStr contains j)){
      newstring=newstring+j
    }
    newstring=newstring.toLowerCase()
    val sub=newstring.split(" ")
    var nsub=""
    for (i<-sub){
      nsub=i+" "+nsub
    }
    nsub+"\n"
  }
  //Exercise 04
  def memberNames():Any={
    val name=scala.io.StdIn.readLine("Member name (When Finished with names, press enter): ").toString
    var nameList=List[String]()
    nameList = nameList :+ name
    if (!name.isEmpty){
      memberNames()
    }
    "The members are: %s".format(nameList)
  }
  //Exercise 05

  //Exercise 06

  //Exercise 07

  //Exercise 08

  //Exercise 09

  //Exercise 10


}
