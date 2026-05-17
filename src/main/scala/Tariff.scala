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
