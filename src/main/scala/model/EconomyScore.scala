package model

trait EconomyScore:
  def score(h: HotelBooking): Double

class PriceBased extends EconomyScore:
  override def score(h: HotelBooking): Double =
    h.bookingPrice // lower better

class DiscountBased extends EconomyScore:
  override def score(h: HotelBooking): Double =
    -h.discount // higher discount better

class MarginBased extends EconomyScore:
  override def score(h: HotelBooking): Double =
    h.profitMargin // lower better
