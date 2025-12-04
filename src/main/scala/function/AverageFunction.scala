package function

import model.HotelBooking
import model.AverageData

object AverageFunction:
  def computeAverages(rawData: List[HotelBooking]): List[AverageData] =
    rawData
      .groupBy(b => (b.hotel, b.hotelCity))   // group by hotel name + city
      .map { case ((hotelName, hotelCity), bookings) =>

        val totalDays = bookings.map(_.days).sum
        val totalRooms = bookings.map(_.rooms).sum
        val totalVisitor = bookings.map(_.visitors).sum

        val avgPrice = bookings.map(_.bookingPrice).sum / (totalDays * totalRooms)
        val avgDiscount = bookings.map(_.discount).sum / bookings.size
        val avgMargin = bookings.map(_.profitMargin).sum / bookings.size

        AverageData(
          hotelName = hotelName,
          hotelCity = hotelCity,
          sumOfDays = totalDays,
          sumOfRoom = totalRooms,
          sumOfVisitor = totalVisitor,
          averagePrice = avgPrice,
          averageDiscount = avgDiscount,
          averageProfitMargin = avgMargin
        )
      }
      .toList
