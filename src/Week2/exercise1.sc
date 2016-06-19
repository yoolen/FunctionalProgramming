def sumInts(a: Int, b: Int): Int =
  if (a > b) 0 else a + sumInts(a + 1, b)

def cube(x: Int): Int = x * x * x

def sumCubes(a: Int, b: Int): Int =
  if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

def fact(x: Int): Int = {
  if(x <= 1) 1 else x * fact (x - 1)
}

def sumFact(a: Int, b: Int): Int =
  if (a > b) 0 else fact(a) + sumFact(a + 1, b)

def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f, a + 1, b)

//sum((x: Int) => x*x*x, 1, 2)

def sum2(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if(a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}