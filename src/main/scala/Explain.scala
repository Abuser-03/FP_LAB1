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
