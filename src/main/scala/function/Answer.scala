package function

import model.{AverageData, DiscountNormalized, EconomyScore, HotelBooking, MarginNormalized, PriceNormalized}

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
  def answer3(rawData: List[HotelBooking]): Unit =
    val mostProfitable = rawData
      .groupBy(_.hotel)
      .map((HotelName, Info) => (HotelName, Info.map(b => b.bookingPrice * b.visitors * b.profitMargin).sum))
      .maxBy( _._2 )

    println(s"3. Most Profitable Hotel: ${mostProfitable._1}")
    println(f"   Total Profit: ${mostProfitable._2}%.2f")
    println("-" * 60)