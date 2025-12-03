package answer

import model.HotelBooking
import model.{EconomyScore, PriceNormalized,DiscountNormalized,MarginNormalized}

object Answer:

  def answer1(rawData: List[HotelBooking]): Unit =
    val grouped = rawData.groupBy(_.country)
    val counts = grouped.mapValues(_.size)
    val topCountry = counts.maxBy(_._2)
    println(topCountry)

    println(s"1. Country with highest bookings: ${topCountry._1} (${topCountry._2})")
    println("-" * 60)

  def answer2(rawData: List[HotelBooking]): Unit =
    println("2. Hotels and their normalized economy scores:")

    // find min & max to normalize correctly
    val prices = rawData.map(_.bookingPrice)
    val discounts = rawData.map(_.discount)
    val margins = rawData.map(_.profitMargin)

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

    // calculate total score for a hotel
    def totalScore(h: HotelBooking): Double =
      scoringMethods.map(_.score(h)).sum

    // print score for all hotels
    rawData.foreach { hotel =>
      val score = totalScore(hotel)
      println(f"Hotel: ${hotel.hotel}, Score: $score%.2f, Price: ${hotel.bookingPrice}, Discount: ${hotel.discount}, Margin: ${hotel.profitMargin}")
    }

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