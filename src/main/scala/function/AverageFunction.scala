package function
import model.HotelBooking
import model.AverageData

object AverageFunction:
  def computeAverages(rawData: List[HotelBooking]): List[AverageData] =
    rawData
      .groupBy(_.hotel) // group rows by hotel name
      .map { (hotelName, bookings) =>

        val totalDays = bookings.map(_.days).sum
        val totalRooms = bookings.map(_.rooms).sum
        val totalVisitor = bookings.map(_.visitors).sum

        val avgPrice = bookings.map(_.bookingPrice).sum / (totalDays * totalRooms)
        val avgDiscount = bookings.map(_.discount).sum / bookings.size
        val avgMargin = bookings.map(_.profitMargin).sum / bookings.size
        val avgVisitor = bookings.map(_.visitors * rawData.head.days).sum / (totalDays * totalRooms)

        AverageData(
          hotelName = hotelName,
          sumOfDays = totalDays,
          sumOfRoom = totalRooms,
          sumOfVisitor = totalVisitor,
          averagePrice = avgPrice,
          averageDiscount = avgDiscount,
          averageProfitMargin = avgMargin,
          averageVisitor = avgVisitor
        )
      }.toList

