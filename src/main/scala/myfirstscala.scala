import scala.io.Source
import scala.util.{Try, Success, Failure}
import model.HotelBooking
import answer.Answer

@main def HotelAnalysis(): Unit =
  val filename = "Hotel_Dataset.csv"

  val bufferedSource = Source.fromFile(filename)("ISO-8859-1")
  

  bufferedSource.close()

  println(s"Successfully loaded ${rawData.size} records.\n")
  println("=" * 60)

  Answer.answer1(rawData)
  Answer.answer2(rawData)
  Answer.answer3(rawData)