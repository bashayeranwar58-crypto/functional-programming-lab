// Блок 0. Инфраструктура (монады)

// просто трейт для монад
trait Monad[M[_]]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => pure(f(a)))

// Reader - читает конфиг
case class Reader[E, A](run: E => A):
  def map[B](f: A => B): Reader[E, B] = Reader(env => f(run(env)))
  def flatMap[B](f: A => Reader[E, B]): Reader[E, B] =
    Reader(env => f(run(env)).run(env))

object Reader:
  def pure[E, A](a: A): Reader[E, A] = Reader(_ => a)

// Writer - хранит лог
case class Writer[A](value: A, log: Vector[String]):
  def map[B](f: A => B): Writer[B] = Writer(f(value), log)
  def flatMap[B](f: A => Writer[B]): Writer[B] =
    val next = f(value)
    Writer(next.value, log ++ next.log)

object Writer:
  def pure[A](a: A): Writer[A] = Writer(a, Vector.empty)

// State - хранит состояние
case class State[S, A](run: S => (A, S)):
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, newS) = run(s)
      (f(a), newS)
    })
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, newS) = run(s)
      f(a).run(newS)
    })

object State:
  def pure[S, A](a: A): State[S, A] = State(s => (a, s))

// IO - для ввода/вывода
case class IO[A](unsafeRun: () => A):
  def map[B](f: A => B): IO[B] = IO(() => f(unsafeRun()))
  def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(unsafeRun()).unsafeRun())

object IO:
  def pure[A](a: A): IO[A] = IO(() => a)
  def println(line: String): IO[Unit] = IO(() => Predef.println(line))
  def readLine: IO[String] = IO(() => scala.io.StdIn.readLine().trim())

// делаем IO монадой
given Monad[IO] with
  def pure[A](a: A): IO[A] = IO.pure(a)
  def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)