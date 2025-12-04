package model


trait EconomyScore:
  def score(a: AverageData): Double

  def normalize(value: Double, min: Double, max: Double): Double =
    if max - min == 0 then 0.0
    else (value - min) / (max - min)

class PriceNormalized(min: Double, max: Double) extends EconomyScore:
  override def score(h: AverageData): Double =
    1.0 - normalize(h.averagePrice, min, max)

class DiscountNormalized(max: Double) extends EconomyScore:
  override def score(h: AverageData): Double =
    normalize(h.averageDiscount, 0, max)

class MarginNormalized(min: Double, max: Double) extends EconomyScore:
  override def score(h: AverageData): Double =
    1.0 - normalize(h.averageProfitMargin, min, max)

class VisitorNormalized(min: Double, max: Double) extends EconomyScore:
  override def score(h: AverageData): Double =
    normalize(h.sumOfVisitor, min, max)