package model


trait EconomyScore:
  def score(h: HotelBooking): Double

  def normalize(value: Double, min: Double, max: Double): Double =
    if max - min == 0 then 0.0
    else (value - min) / (max - min)

class PriceNormalized(min: Double, max: Double) extends EconomyScore:
  override def score(h: HotelBooking): Double =
    normalize(h.actualPrice, min, max)

class DiscountNormalized(max: Double) extends EconomyScore:
  override def score(h: HotelBooking): Double =
    1.0 - normalize(h.discount, 0, max)  // reverse

class MarginNormalized(min: Double, max: Double) extends EconomyScore:
  override def score(h: HotelBooking): Double =
    normalize(h.profitMargin, min, max)