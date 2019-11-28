package example.a.model

case class Timestamp(seconds:Int)

object Timestamp {
  val secondsInHour: Int= 60*60
  val secondsInMinutes: Int = 60

  def apply(hours: Int, minutes: Int, seconds: Int): Timestamp =
    Timestamp(seconds = secondsInHour * hour + secondsInMinutes * minutes + seconds)


}