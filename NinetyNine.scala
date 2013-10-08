object NinetyNine extends App {

  
    def last[T](l: List[T]): T = l match {
      case Nil => throw new Error("boo")
      case x :: Nil => x
      case x :: ys => last(ys)
    } 
    assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
    
    def penultimate[T](l: List[T]): T = l match {      
      case x :: y :: Nil => x
      case x :: y :: ys => penultimate(l.tail)
      case _ => throw new Error("boo") 
    } 
    assert(penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
    
    def nth[T](n: Int, l: List[T]): T = {
      val res = 
        l.foldLeft[Pair[Int, Option[T]]]((-1, None))( 
            (p, i) => if (p._1 < n) Pair(p._1 + 1, Some(i)) else p )
    
      if (res._1 == n) res._2.get else throw new Error("boo")      
    }      
    assert(nth(2, List(1, 1, 2, 3, 5, 8)) == 2)
    
    def length[T](l: List[T]): Int = l.foldLeft(0)((len, _) => len + 1)
    assert(length(List(1, 1, 2, 3, 5, 8)) == 6)
    
    def reverse[T](l: List[T]): List[T] = l.foldLeft[List[T]](Nil)((res, i) => i :: res)
    assert(reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
    
    def isPalindrome[T](l: List[T]): Boolean = reverse(l) == l
    assert(isPalindrome(List(1, 2, 3, 2, 1)))
    
    def flatten(l: List[Any]): List[Any] = 
      l.foldLeft[List[Any]](Nil)((res, i) => i match {
        case i: List[Any] => flatten(i).reverse ::: res
        case _ => i :: res
      }).reverse        
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8) )
    
    def compress[T](l: List[T]): List[T] = l match {
      case Nil => Nil
      case x :: Nil => l
      case _ => l.tail.foldLeft(l.head :: Nil)( (res, i) => if (i == res.head) res else i :: res).reverse 
    }
    assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
      
    def pack[T](l: List[T]): List[List[T]] = l match {
      case Nil => Nil
      case x :: Nil => List(l)
      case _ => l.tail.foldLeft(List(List(l.head)))( (res, i) => 
        if (i == res.head.head) (i :: res.head) :: res.tail else List(i) :: res).reverse 
    }
      
    assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) 
        == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))   
    
      
    println("hello world")
}
