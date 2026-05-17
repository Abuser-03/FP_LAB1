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
