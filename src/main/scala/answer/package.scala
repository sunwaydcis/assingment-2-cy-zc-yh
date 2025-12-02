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

  def answer2(rawData: List[HotelBooking]): Unit =
    println("2. Which hotel offers the most economical overall value to customers based on booking price, discount, and profit margin?")
    println("-" * 60)

    def minMax[T](list: List[T], f: T => Double): (Double, Double) =
      val values = list.map(f)
      (values.min, values.max)

    val (minPrice, maxPrice) = minMax(rawData, _.bookingPrice)
    val (minDiscount, maxDiscount) = minMax(rawData, _.discount)
    val (minMargin, maxMargin) = minMax(rawData, _.profitMargin)

    def normalize(value: Double, min: Double, max: Double): Double =
      if max == min then 0.0
      else
        (value - min) / (max - min)

    val bestEconomical = rawData.minBy { h =>
      val priceScore = normalize(h.bookingPrice, minPrice, maxPrice) // lower better
      val discountScore = 1.0 - normalize(h.discount, 0, maxDiscount) // higher better
      val marginScore = normalize(h.profitMargin, minMargin, maxMargin) // lower better
      priceScore + discountScore + marginScore
    }

    println("Most Economical Hotel Overall:")
    println(s"   - Hotel: ${bestEconomical.hotel}")
    println(f"   - Booking Price: ${bestEconomical.bookingPrice}%.2f")
    println(f"   - Discount: ${bestEconomical.discount}%.2f%%")
    println(f"   - Profit Margin: ${bestEconomical.profitMargin}%.2f")
    println("-" * 60)

  // Question 3:
  // Profit = visitor * profitMargin
  def answer3(rawData: List[HotelBooking]): Unit =
    val mostProfitable = rawData
      .groupBy(_.hotel)
      .map((HotelName, Info) => (HotelName, Info.map(b => b.bookingPrice * b.visitors * b.profitMargin).sum))
      .maxBy( _._2 )


    println(s"3. Most Profitable Hotel: ${mostProfitable._1}")
    println(f"   Total Profit: ${mostProfitable._2}%.2f")
    println("-" * 60)