package answer

import model.HotelBooking

object Answer:

  def answer1(rawData: List[HotelBooking]): Unit =
    val topCountry = rawData
      .groupBy(_.country)
      .map((c, b) => (c, b.size))
      .maxBy(_._2)

    println(s"1. Country with highest bookings: ${topCountry._1} (${topCountry._2})")
    println("-" * 60)