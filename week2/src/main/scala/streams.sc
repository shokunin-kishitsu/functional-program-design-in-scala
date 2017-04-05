// The Sieve of Eratosthenes

def from(n: Int): Stream[Int] = n #:: from(n + 1)

def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

from(2).take(10).toList

val primes = sieve(from(2))

primes.take(10).toList