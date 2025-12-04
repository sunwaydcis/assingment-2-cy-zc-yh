package function

import model.{AverageData, DiscountNormalized, EconomyScore, HotelBooking, MarginNormalized, PriceNormalized, VisitorNormalized}

object Answer:

  def answer1(rawData: List[HotelBooking]): Unit =
    val grouped = rawData.groupBy(_.country)
    val counts = grouped.mapValues(_.size)
    val topCountry = counts.maxBy(_._2)
    println(topCountry)

    println(s"1. Country with highest bookings: ${topCountry._1} (${topCountry._2})")
    println("-" * 60)

  def answer2(rawData: List[AverageData]): Unit =
    println("2. Hotels and their normalized economy scores:")

    // find min & max to normalize correctly
    val prices = rawData.map(_.averagePrice)
    val discounts = rawData.map(_.averageDiscount)
    val margins = rawData.map(_.averageProfitMargin)

    val minPrice = prices.min
    val maxPrice = prices.max
    val maxDiscount = discounts.max
    val minMargin = margins.min
    val maxMargin = margins.max

    // scoring rules (polymorphic)
    val scoringMethods: List[EconomyScore] = List(
      new PriceNormalized(minPrice, maxPrice),
      new DiscountNormalized(maxDiscount),
      new MarginNormalized(minMargin, maxMargin)
    )

    // total score for each hotel (NOW uses AverageData)
    def totalScore(a: AverageData): Double =
      scoringMethods.map(_.score(a)).sum / scoringMethods.size

    // find best hotel
    val best = rawData.maxBy(totalScore)

    val score = totalScore(best)

    println(
      f"Best Hotel: ${best.hotelName}\n" +
        f"Score: $score%.2f\n" +
        f"Average Price: ${best.averagePrice}%.2f\n" +
        f"Average Discount: ${best.averageDiscount}%.2f\n" +
        f"Average Profit Margin: ${best.averageProfitMargin}%.2f"
    )

    println("-" * 60)

  // Question 3:
  def answer3(rawData: List[AverageData]): Unit =
    val visitors = rawData.map(_.averageVisitor)
    val margins = rawData.map(_.averageProfitMargin)

    val minMargin = margins.min
    val maxMargin = margins.max
    val minVisitors = visitors.min
    val maxVisitors = visitors.max

    // Normalized scorers for AverageData
    val scoringMethods: List[AverageData => Double] = List(
      h => if maxMargin - minMargin == 0 then 0.0 else (h.averageProfitMargin - minMargin) / (maxMargin - minMargin),
      h => if maxVisitors - minVisitors == 0 then 0.0 else (h.averageVisitor - minVisitors) / (maxVisitors - minVisitors)
    )

    // Total score for a hotel
    def totalScore(h: AverageData): Double =
      scoringMethods.map(f => f(h)).sum / scoringMethods.size

    // Since each hotel is already one row, just map directly
    val hotelScores = rawData.map(h => h.hotelName -> totalScore(h))

    val mostProfitableHotel = hotelScores.maxBy(_._2)

    println(
      f"Most Profitable Hotel: ${mostProfitableHotel._1}\n" +
        f"Average Score: ${mostProfitableHotel._2}%.2f"
    )


