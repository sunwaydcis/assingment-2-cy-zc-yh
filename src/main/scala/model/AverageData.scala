package model

case class AverageData(
                        hotelName:String,
                        sumOfDays:Int,
                        sumOfRoom:Int,
                        sumOfVisitor:Int,
                        averagePrice:Double,
                        averageDiscount:Double,
                        averageProfitMargin:Double,
                        averageVisitor:Double
                      )