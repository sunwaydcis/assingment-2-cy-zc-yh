import scala.io.Source
import scala.util.Try
import model.HotelBooking
import answer.Answer

@main def HotelAnalysis(): Unit =
  def parseDouble(s: String): Double = Try(s.replace("%", "").toDouble).getOrElse(0.0)
  def parseInt(s: String): Int = Try(s.toInt).getOrElse(0)

  val bookings: List[HotelBooking] =
    Source.fromResource("resource/Hotel_Dataset.csv")
      .getLines()
      .drop(1)
      .map { line =>
        val cols = line.split(",", -1).map(_.trim)
        HotelBooking(
          hotel = cols(16),
          country = cols(9),
          bookingPrice = parseDouble(cols(20)),
          discount = parseDouble(cols(21)),
          profitMargin = parseDouble(cols(23)),
          visitors = parseInt(cols(11))
        )
      }.toList

  println(s"Loaded ${bookings.size} bookings successfully.")
  Answer.answer1(bookings)
