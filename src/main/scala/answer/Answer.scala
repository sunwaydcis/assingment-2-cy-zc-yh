package answer

import model.HotelBooking
import model.{EconomyScore, PriceBased, DiscountBased, MarginBased}

object Answer:

  def answer1(rawData: List[HotelBooking]): Unit =
    val grouped = rawData.groupBy(_.country)
    val counts = grouped.mapValues(_.size)
    val topCountry = counts.maxBy(_._2)
    println(topCountry)

    println(s"1. Country with highest bookings: ${topCountry._1} (${topCountry._2})")
    println("-" * 60)

  def answer2(rawData: List[HotelBooking]): Unit =
    println("2. Most economical value using polymorphic scoring system")

    val scoringMethods: List[EconomyScore] =
      List(new PriceBased, new DiscountBased, new MarginBased)

    def totalScore(hotel: HotelBooking): Double =
      scoringMethods.map(_.score(hotel)).sum   // Works due to subtype polymorphism

    val bestHotel = rawData.minBy(totalScore)

    println(s"Most Economical Hotel: ${bestHotel.hotel}")
    println(s"Booking Price: ${bestHotel.bookingPrice}")
    println(s"Discount: ${bestHotel.discount}")
    println(s"Profit Margin: ${bestHotel.profitMargin}")
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