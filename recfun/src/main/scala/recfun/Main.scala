package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = { // 1-open

    def counter(chars: List[Char], count : Int): Boolean = { // 2-open

    	// handle the empty char case
      if (chars.isEmpty) {
        if (count == 0) true
        else false
      }
      else { // 3-open
        
        // handle the cases that the first char is not '(' or ')'
        if (chars.head != '(' && chars.head != ')') counter(chars.tail, count)
        else { // 4-open
          
          // if the first char is '('
          if (chars.head == '(') counter(chars.tail, count + 1)
            
          // if the first char is ')'
          else {
            if (count == 0) false
            else counter(chars.tail, count - 1)
          }
          
        } //4-close
      } //3-close
    } // 2-close
            
    counter(chars, 0) 
  } // 1-close
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
