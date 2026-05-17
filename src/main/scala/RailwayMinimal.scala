



import java.util.concurrent.atomic.AtomicReference
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


// Блок 1. Предметная область


enum TicketClass:
  case Plackart, Coupe, Lux

final case class Route(from: String, to: String)
final case class Train(id: String, route: Route, totalSeats: Int)
final case class Ticket(
    id: Int,
    trainId: String,
    seat: Int,
    cls: TicketClass,
    baggageKg: Int,
    price: Int
)

final case class Config(
    tariffs: Map[Route, Int],
    classMultiplier: Map[TicketClass, Double],
    freeBaggageKg: Int,
    baggagePerKg: Int,
    refundFee: Int
)
// залупа полная так что правила какие-то странные
object Config:
  val default: Config = Config(
    tariffs = Map(
      Route("Москва", "Питер")  -> 3000,
      Route("Москва", "Нижний") -> 2000,
      Route("Питер",  "Нижний") -> 4000
    ),
    classMultiplier = Map(
      TicketClass.Plackart -> 1.0,
      TicketClass.Coupe    -> 1.5,
      TicketClass.Lux      -> 3.0
    ),
    freeBaggageKg = 20,
    baggagePerKg  = 100,
    refundFee     = 200
  )

final case class World(
    trains: Map[String, Train],
    occupiedSeats: Map[String, Set[Int]],
    tickets: Map[Int, Ticket],
    revenue: Int,
    nextTicketId: Int,
    day: Int
)

object World:
  val initial: World = World(
    trains = Map(
      "001" -> Train("001", Route("Москва", "Питер"),  10),
      "002" -> Train("002", Route("Москва", "Нижний"),  8)
    ),
    occupiedSeats = Map.empty,
    tickets = Map.empty,
    revenue = 0,
    nextTicketId = 1,
    day = 1
  )


// Блок 2. Reader — функции по конфигурации


object Tariff:
  def ticketPrice(route: Route, cls: TicketClass): Reader[Config, Int] =
    Reader.ask[Config].map { cfg =>
      val base = cfg.tariffs.getOrElse(route, 0)
      val mult = cfg.classMultiplier.getOrElse(cls, 1.0)
      (base * mult).toInt
    }

  def baggageCost(weight: Int): Reader[Config, Int] =
    Reader.ask[Config].map { cfg =>
      val extra = math.max(0, weight - cfg.freeBaggageKg)
      extra * cfg.baggagePerKg
    }

  def seatAvailable(train: Train, seat: Int): Reader[Config, Boolean] =
    Reader.pure(seat >= 1 && seat <= train.totalSeats)

  def refundAmount(ticket: Ticket): Reader[Config, Int] =
    Reader.ask[Config].map { cfg =>
      math.max(0, ticket.price - cfg.refundFee)
    }


// Блок 3. Writer — поясняющие логи


type LogLine = String

object Explain:
  def route(r: Route): Writer[LogLine, Route] =
    Writer.tell(s"выбран маршрут ${r.from} => ${r.to}").map(_ => r)

  def price(r: Route, cls: TicketClass, baggageKg: Int, total: Int): Writer[LogLine, Int] =
    Writer.tell(s"итого ${r.from} => ${r.to}, $cls, багаж ${baggageKg}кг: $total Тугриков").map(_ => total)

  def seat(trainId: String, seat: Int): Writer[LogLine, Int] =
    Writer.tell(s"назначено место $seat в поезде $trainId").map(_ => seat)

  def refund(ticketId: Int, amount: Int): Writer[LogLine, Int] =
    Writer.tell(s"возврат билета #$ticketId, сумма $amount Тугриков").map(_ => amount)


// Блок 4. State — переходы по состоянию

object Cash:
  def addTrain(train: Train): State[World, Unit] =
    State.modify(w => w.copy(trains = w.trains.updated(train.id, train)))

  def bookTicket(
      trainId: String,
      seat: Int,
      cls: TicketClass,
      baggageKg: Int,
      price: Int
  ): State[World, Option[Ticket]] =
    State.get[World].flatMap { w =>
      w.trains.get(trainId) match
        case None        => State.pure(None)
        case Some(train) =>
          val taken = w.occupiedSeats.getOrElse(trainId, Set.empty)
          val bad   = taken.contains(seat) || seat < 1 || seat > train.totalSeats
          if bad then State.pure(None)
          else
            val id     = w.nextTicketId
            val ticket = Ticket(id, trainId, seat, cls, baggageKg, price)
            val nextW  = w.copy(
              occupiedSeats = w.occupiedSeats.updated(trainId, taken + seat),
              tickets       = w.tickets.updated(id, ticket),
              revenue       = w.revenue + price,
              nextTicketId  = id + 1
            )
            State.put(nextW).map(_ => Some(ticket))
    }

  def cancelTicket(ticketId: Int, refund: Int): State[World, Boolean] =
    State.get[World].flatMap { w =>
      w.tickets.get(ticketId) match
        case None    => State.pure(false)
        case Some(t) =>
          val taken = w.occupiedSeats.getOrElse(t.trainId, Set.empty)
          val nextW = w.copy(
            occupiedSeats = w.occupiedSeats.updated(t.trainId, taken - t.seat),
            tickets       = w.tickets.removed(ticketId),
            revenue       = w.revenue - refund
          )
          State.put(nextW).map(_ => true)
    }

  def nextDay: State[World, Unit] =
    State.modify(w => w.copy(day = w.day + 1))


// Блок 5. IO-сценарий


object Program:
  private val ref: AtomicReference[World] = new AtomicReference(World.initial)
  private val cfg: Config                  = Config.default

  // Склейка миров: чистые Reader/Writer/State запускаются здесь, в IO.
  private val getWorld: IO[World] =
    IO.delay(ref.get())

  private def runReader[A](r: Reader[Config, A]): A =
    r.run(cfg)

  private def runState[A](st: State[World, A]): IO[A] =
    IO.delay {
      val (newWorld, a) = st.run(ref.get())
      ref.set(newWorld)
      a
    }

  private def runStateWithLog[A](st: State[World, Writer[LogLine, A]]): IO[A] =
    IO.delay {
      val (newWorld, w) = st.run(ref.get())
      ref.set(newWorld)
      w.log.foreach(line => println(s"  * $line"))
      w.value
    }

  private def ask(prompt: String): IO[String] =
    for
      _ <- IO.putStrLn(prompt)
      s <- IO.readLine
    yield Option(s).getOrElse("").trim

  private def askInt(prompt: String): IO[Int] =
    ask(prompt).map(_.toIntOption.getOrElse(0))

  private def parseClass(s: String): TicketClass = s.toLowerCase match
    case "lux"   | "люкс" => TicketClass.Lux
    case "coupe" | "купе" => TicketClass.Coupe
    case _                 => TicketClass.Plackart

  val showState: IO[Unit] = IO.delay {
    val w = ref.get()
    println(s"\n--- день ${w.day}, выручка ${w.revenue} Тугрики ---")
    w.trains.values.toList.sortBy(_.id).foreach { t =>
      val taken = w.occupiedSeats.getOrElse(t.id, Set.empty).size
      println(s"  поезд ${t.id} (${t.route.from} => ${t.route.to}): $taken/${t.totalSeats}")
    }
    if w.tickets.nonEmpty then
      println("  билеты:")
      w.tickets.values.toList.sortBy(_.id).foreach { t =>
        println(s"    #${t.id}: поезд ${t.trainId}, место ${t.seat}, ${t.cls}, ${t.price} Тугрики")
      }
  }

  private def doBook(t: Train, seat: Int, cls: TicketClass, bag: Int): IO[Unit] =
    val seatOk = runReader(Tariff.seatAvailable(t, seat))
    if !seatOk then
      IO.putStrLn(s"место $seat вне диапазона 1..${t.totalSeats}")
    else
      val ticketP = runReader(Tariff.ticketPrice(t.route, cls))
      val bagP    = runReader(Tariff.baggageCost(bag))
      val total   = ticketP + bagP
      // склейка State и Writer: внутри State.map возвращаем Writer с логами
      val program: State[World, Writer[LogLine, Option[Ticket]]] =
        Cash.bookTicket(t.id, seat, cls, bag, total).map { opt =>
          for
            _ <- Explain.route(t.route)
            _ <- Explain.seat(t.id, seat)
            _ <- Explain.price(t.route, cls, bag, total)
          yield opt
        }
      for
        opt <- runStateWithLog(program)
        _   <- opt match
          case Some(tk) => IO.putStrLn(s" билет #${tk.id} куплен на $total Тугрики")
          case None     => IO.putStrLn(" место уже занято")
      yield ()

  val bookFlow: IO[Unit] =
    for
      trainId <- ask("ID поезда:")
      seat    <- askInt("Номер места:")
      cls     <- ask("Класс (plackart/coupe/lux):").map(parseClass)
      bag     <- askInt("Багаж (кг):")
      world   <- getWorld
      _       <- world.trains.get(trainId) match
                   case None    => IO.putStrLn(s"поезд $trainId не найден")
                   case Some(t) => doBook(t, seat, cls, bag)
    yield ()

  private def doCancel(t: Ticket): IO[Unit] =
    val refund = runReader(Tariff.refundAmount(t))
    val program: State[World, Writer[LogLine, Boolean]] =
      Cash.cancelTicket(t.id, refund).map { ok =>
        Explain.refund(t.id, refund).map(_ => ok)
      }
    for
      ok <- runStateWithLog(program)
      _  <- if ok then IO.putStrLn(s" возвращено $refund тугрики")
            else      IO.putStrLn(" ошибка возврата")
    yield ()

  val cancelFlow: IO[Unit] =
    for
      tid   <- askInt("ID билета для возврата:")
      world <- getWorld
      _     <- world.tickets.get(tid) match
                 case None    => IO.putStrLn(s"билет #$tid не найден")
                 case Some(t) => doCancel(t)
    yield ()

  val addTrainFlow: IO[Unit] =
    for
      id    <- ask("ID нового поезда:")
      from  <- ask("Откуда:")
      to    <- ask("Куда:")
      seats <- askInt("Сколько мест:")
      _     <- runState(Cash.addTrain(Train(id, Route(from, to), seats)))
      _     <- IO.putStrLn(s" добавлен поезд $id")
    yield ()

  val nextDayFlow: IO[Unit] =
    for
      _ <- runState(Cash.nextDay)
      _ <- IO.putStrLn(" новый день начался")
    yield ()

  private val items: List[(String, IO[Unit])] = List(
    "показать состояние" -> showState,
    "купить билет" -> bookFlow,
    "вернуть билет" -> cancelFlow,
    "добавить поезд" -> addTrainFlow,
    "следующий день" -> nextDayFlow
  )

  private val showMenu: IO[Unit] = IO.delay {
    println("\n=== Касса ===")
    items.zipWithIndex.foreach { case ((label, _), i) =>
      println(s" ${i + 1}) $label")
    }
    println(" 0) выход")
  }

  private def dispatch(cmd: String): IO[Unit] = cmd.toIntOption match
    case Some(0) => IO.pure(())
    case Some(i) if i >= 1 && i <= items.size => items(i - 1)._2
    case _ => IO.putStrLn(s"неизвестная команда '$cmd'")

  val loop: IO[Unit] =
    for
      _ <- showMenu
      cmd <- IO.readLine.map(s => Option(s).getOrElse("0").trim)
      _ <- if cmd == "0" then IO.putStrLn("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      else dispatch(cmd).flatMap(_ => loop)
    yield ()

  val run: IO[Unit] = loop

@main def runRailway(): Unit =
  Program.run.unsafeRun()
