def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a>b) acc
    else loop(a+1, acc + f(a))
  }
  loop(a, 0)
}

sum ((x:Int)=>x*x, 1,5)

def mapReduce (f:Int=>Int, combine:(Int, Int)=>Int, zero:Int) (a:Int , b:Int):Int ={
  if (a>b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1,b))
}

def product(f:Int=>Int) (a:Int, b:Int) : Int =
  mapReduce(f, (x,y)=>x*y, 1)(a,b)

def facto(n:Int) : Int= product(x=>x)(1,n)
facto(5)
mapReduce(x=>x, (x,y)=>x*y, 1)(1,5)

