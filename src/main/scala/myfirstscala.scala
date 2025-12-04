import scala.io.Source
import scala.util.{Try, Success, Failure}
import function.AverageFunction
import model.HotelBooking
import function.Answer

@main def HotelAnalysis(): Unit =

  def parseDouble(s: String): Double = Try(s.replace("%", "").toDouble).getOrElse(0.0)
  def parseInt(s: String): Int = Try(s.toInt).getOrElse(0)

  val bookingsResult = Try {
    Source.fromResource("resource/Hotel_Dataset.csv")
      .getLines()
      .drop(1)
      .map { line =>
        val cols = line.split(",", -1).map(_.trim)
        HotelBooking(
          days = parseInt(cols(13)),
          rooms = parseInt(cols(15)),
          hotel = cols(16),
          country = cols(9),
          hotelCity = cols(10),
          bookingPrice = parseDouble(cols(20)),
          discount = parseDouble(cols(21)),
          profitMargin = parseDouble(cols(23)),
          visitors = parseInt(cols(11))
        )
      }
      .toList
  }

  bookingsResult match
    case Success(bookings) =>
      println(s"Loaded ${bookings.size} bookings successfully.")

      // Compute average results
      val avgData = AverageFunction.computeAverages(bookings)

      // Now call answers correctly
      Answer.answer1(bookings)
      Answer.answer2(avgData)
      Answer.answer3(avgData)

    case Failure(ex) =>
      println("ERROR: Unable to load Hotel_Dataset.csv file.")
      println("Please ensure the file exists in: /resources/resource/Hotel_Dataset.csv")
