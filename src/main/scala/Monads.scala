import scala.io.StdIn


// Блок 0. Монады


trait Monad[M[_]]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => pure(f(a)))

final case class Reader[E, A](run: E => A):
  def map[B](f: A => B): Reader[E, B] =
    Reader(e => f(run(e)))
  def flatMap[B](f: A => Reader[E, B]): Reader[E, B] =
    Reader(e => f(run(e)).run(e))

object Reader:
  def pure[E, A](a: A): Reader[E, A] = Reader(_ => a)
  def ask[E]: Reader[E, E] = Reader(e => e)

final case class Writer[L, A](log: Vector[L], value: A):
  def map[B](f: A => B): Writer[L, B] =
    Writer(log, f(value))
  def flatMap[B](f: A => Writer[L, B]): Writer[L, B] =
    val next = f(value)
    Writer(log ++ next.log, next.value)

object Writer:
  def pure[L, A](a: A): Writer[L, A] = Writer(Vector.empty, a)
  def tell[L](msg: L): Writer[L, Unit] = Writer(Vector(msg), ())

final case class State[S, A](run: S => (S, A)):
  def map[B](f: A => B): State[S, B] =
    State { s =>
      val (s1, a) = run(s)
      (s1, f(a))
    }
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (s1, a) = run(s)
      f(a).run(s1)
    }

object State:
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))
  def get[S]: State[S, S] = State(s => (s, s))
  def put[S](s: S): State[S, Unit] = State(_ => (s, ()))
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))

final case class IO[A](unsafeRun: () => A):
  def map[B](f: A => B): IO[B] =
    IO(() => f(unsafeRun()))
  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(() => f(unsafeRun()).unsafeRun())

object IO:
  def pure[A](a: A): IO[A] = IO(() => a)
  def delay[A](a: => A): IO[A] = IO(() => a)
  def putStrLn(s: String): IO[Unit] = IO(() => println(s))
  def readLine: IO[String] = IO(() => StdIn.readLine())

given readerMonad[E]: Monad[[A] =>> Reader[E, A]] with
  def pure[A](a: A): Reader[E, A] = Reader.pure(a)
  def flatMap[A, B](ma: Reader[E, A])(f: A => Reader[E, B]): Reader[E, B] = ma.flatMap(f)

given writerMonad[L]: Monad[[A] =>> Writer[L, A]] with
  def pure[A](a: A): Writer[L, A] = Writer.pure(a)
  def flatMap[A, B](ma: Writer[L, A])(f: A => Writer[L, B]): Writer[L, B] = ma.flatMap(f)

given stateMonad[S]: Monad[[A] =>> State[S, A]] with
  def pure[A](a: A): State[S, A] = State.pure(a)
  def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)

given ioMonad: Monad[IO] with
  def pure[A](a: A): IO[A] = IO.pure(a)
  def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)
