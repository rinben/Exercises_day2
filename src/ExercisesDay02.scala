object ExercisesDay02 {
  def main(args: Array[String]): Unit = {
    //Exercise 01
    println(palindrome("Sit on a potato pan, Otis."))
    println(palindrome("Napa racecar a pan"))
    println(palindrome("Pusssy"))
    //Exercise 02
    println(fib_seq(20))
    println(fib_seq(5))
    println(fib_seq(1))
    println(fib_seq(71))
    //Exercise 03

    //Exercise 04

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


  //Exercise 04

  //Exercise 05

  //Exercise 06

  //Exercise 07

  //Exercise 08

  //Exercise 09

  //Exercise 10


}
