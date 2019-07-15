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
    //println(memberNames())
    println()
    //Exercise 05
    println(validPass("Rgba1717@"))
    println(validPass("rgba1717"))
    println()
    //Exercise 06
    println(numAvg(10))
    println()
    //Exercise 07
    println(evenFinder(133))
    println()
    //Exercise 08
    //println(nextDay())
    println()
    //Exercise 09
    println(distinctVal(Map("hello"->3,"world"->5,"I"->5,"am"->3,"Ben"->3)))
    println()
    //Exercise 10
    println(additionVector(Map(1->3,2->3),Map(1->4,3->5)))
    println(dotProduct(Map(1->3,2->3,6->2),Map(1->4,3->5,6->2)))
    println()
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

    val delimArray = Array(',', ' ', '.', '/', '"')
    val newArray = str.split(delimArray)
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
      val lastIndex = newStr.length - 1
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
    var newString=""
    for (j<-str if !(badStr contains j)){
      newString=newString+j
    }
    newString=newString.toLowerCase()
    val sub=newString.split(" ")
    var nSub=""
    for (i<-sub){
      nSub=i+" "+nSub
    }
    nSub+"\n"
  }
  //Exercise 04
  var nameList:List[String]=List[String]()
  var count:Int=1
  def memberNames():Any={
    val name=scala.io.StdIn.readLine("Member name (First Last): ").toString
    if (!name.isEmpty){
      if (count==1) {
        println("Thank You. When Finished with member names press enter.")
      }
      nameList = name::nameList
      count=count+1
      memberNames()
    }
    "There are %d members and they are: %s".format(count-1,nameList)
  }
  //Exercise 05
  def validPass(str:String):String= {
    val strSet = str.toSet[Char]
    var b1 = false
    var b2 = false
    var b3 = false
    var b4 = false
    val lowercase = ('a' to 'z').toSet[Char]
    val uppercase = ('A' to 'Z').toSet[Char]
    val numbers = ('0' to '9').toSet[Char]
    val specialChar = Set[Char]('@', '#', '$')
    for (i <- strSet) {
      if (lowercase.contains(i)) {
        b1 = true
      }
      else if (uppercase.contains(i)) {
        b2 = true
      }
      else if (numbers.contains(i)) {
        b3 = true
      }
      else if (specialChar.contains(i)) {
        b4 = true
      }
      else {
        return "Not a valid password. Please change password"
      }
    }
    if (b1 && b2 && b3 && b4) {
      return "Valid password!"
    }
    else{
      return "Not a valid password. Please change password"
    }
    "\n"
  }
  //Exercise 06
  def numAvg(limit:Int):Float={
    var sum=0
    var i=1
    while (i<=limit){
      sum=sum+i
      i=i+1
    }
    val avg=sum/i
    avg
  }
  //Exercise 07
  def evenFinder(limit:Int):String={
    var numList=List[String]()
    for (i<- 2 to limit by 2){
      numList=numList:+i.toString
    }
    for (j<-numList) {
      for (l <- 1 until 10 by 2) {
        for (h<-0 until j.length) {
          if (j(h).toString == l.toString) {
            numList = numList.filter(_ != j)
          }
        }
      }
    }
    for (t<-0 until numList.length-1){
      print(numList(t)+", ")
    }
    print(numList(numList.length-1))
    "\n"
  }
  //Exercise 08
  def nextDay():String={
    val longMonths=List(1,3,5,7,8,10,12)
    var year=scala.io.StdIn.readLine("Input year: ").toInt
    var month=scala.io.StdIn.readLine("Input month: ").toInt
    var day=scala.io.StdIn.readLine("Input day: ").toInt
    if ((1<=day && day<=31) && (1<=month && month<=12) && !((day>=30 && day<=31 && month==2)||(day==29 && month==2 && year%4!=0))) {
      if (day == 31 || (day == 30 && !longMonths.contains(month))) {
        if (month != 12) {
          month = month + 1
          day = 1
        }
        else {
          month = 1
          day = 1
          year = year + 1
        }
      }
      else if (day == 28 && month == 2 && year % 4 == 0) {
        day = day + 1
      }
      else if (day == 28 && month == 2 && year % 4 != 0 || (day == 29 && month == 2 && year % 4 == 0)) {
        day = 1
        month = month + 1
      }
      else {
        day = day + 1
      }
    }
    else if ((day>=30 && day<=31 && month==2)||(day==29 && month==2 && year%4!=0)){
      return "Please enter a correct date."
    }
    else{
      return "Please enter a correct date."
    }
    s"The next day is $month/$day/$year."
  }
  //Exercise 09
  def distinctVal(m:Map[String, Int]):Int={
    var total=0
    for (i<-m.values){
      var count=0
      for (j<-m.values){
        if (j==i){
          count=count+1
        }
      }
      if (!(count>1)){
        total=total+1
      }
    }
    total
  }
  //Exercise 10
  def additionVector(m1:Map[Int,Int],m2:Map[Int,Int]):Map[Int,Int]={
    var m3=Map[Int,Int]()
    for (i<-m1.keys){
      if (m2.contains(i)) {
        m3 = m3 + (i -> (m1(i) + m2(i)))
      }
      else {
        m3 = m3 + (i -> m1(i))
      }
    }
    for (j<-m2.keys) {
      if (!m3.contains(j)){
        m3 = m3 + (j -> m2(j))
      }
    }
    m3
  }
  def dotProduct(m1:Map[Int,Int],m2:Map[Int,Int]):Int={
    var m3=Map[Int,Int]()
    for (i<-m1.keys) {
      if (m2.contains(i)) {
        m3 = m3 + (i -> (m1(i) * m2(i)))
      }
    }
    m3.values.sum
  }

}
