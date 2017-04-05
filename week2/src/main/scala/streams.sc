// The Sieve of Eratosthenes

def from(n: Int): Stream[Int] = n #:: from(n + 1)

def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

from(2).take(10).toList

val primes = sieve(from(2))

primes.take(10).toList

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

val sqrts = sqrtStream(2)

sqrts.take(10).toList.drop(7)