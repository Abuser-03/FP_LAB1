import java.util.concurrent.atomic.AtomicReference


// Блок 5. IO-сценарий


object Program:
  private val ref: AtomicReference[World] = new AtomicReference(World.initial)
  private val cfg: Config                  = Config.default

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
    "купить билет"       -> bookFlow,
    "вернуть билет"      -> cancelFlow,
    "добавить поезд"     -> addTrainFlow,
    "следующий день"     -> nextDayFlow
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
      _   <- showMenu
      cmd <- IO.readLine.map(s => Option(s).getOrElse("0").trim)
      _   <- if cmd == "0" then IO.putStrLn("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
             else dispatch(cmd).flatMap(_ => loop)
    yield ()

  val run: IO[Unit] = loop

@main def runRailway(): Unit =
  Program.run.unsafeRun()
