// Блок 4. State — переходы по состоянию


object Cash:
  def addTrain(train: Train): State[World, Unit] =
    State.modify(w => w.copy(trains = w.trains.updated(train.id, train)))

  private def bookSeat(w: World, train: Train, seat: Int, cls: TicketClass, baggageKg: Int, price: Int): State[World, Option[Ticket]] =
    val taken = w.occupiedSeats.getOrElse(train.id, Set.empty)
    val bad   = taken.contains(seat) || seat < 1 || seat > train.totalSeats
    if bad then State.pure(None)
    else
      val id     = w.nextTicketId
      val ticket = Ticket(id, train.id, seat, cls, baggageKg, price)
      val nextW  = w.copy(
        occupiedSeats = w.occupiedSeats.updated(train.id, taken + seat),
        tickets       = w.tickets.updated(id, ticket),
        revenue       = w.revenue + price,
        nextTicketId  = id + 1
      )
      State.put(nextW).map(_ => Some(ticket))

  private def performBooking(
      trainId: String,
      seat: Int,
      cls: TicketClass,
      baggageKg: Int,
      price: Int
  )(w: World): State[World, Option[Ticket]] =
    w.trains.get(trainId) match
      case None        => State.pure(None)
      case Some(train) => bookSeat(w, train, seat, cls, baggageKg, price)

  def bookTicket(
      trainId: String,
      seat: Int,
      cls: TicketClass,
      baggageKg: Int,
      price: Int
  ): State[World, Option[Ticket]] =
    State.get[World].flatMap(performBooking(trainId, seat, cls, baggageKg, price))

  private def performCancellation(ticketId: Int, refund: Int)(w: World): State[World, Boolean] =
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

  def cancelTicket(ticketId: Int, refund: Int): State[World, Boolean] =
    State.get[World].flatMap(performCancellation(ticketId, refund))

  def nextDay: State[World, Unit] =
    State.modify(w => w.copy(day = w.day + 1))
