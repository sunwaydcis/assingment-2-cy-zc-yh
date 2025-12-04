package model

case class AverageData(
                        hotelName:String,
                        hotelCity:String,
                        sumOfDays:Int,
                        sumOfRoom:Int,
                        sumOfVisitor:Int,
                        averagePrice:Double,
                        averageDiscount:Double,
                        averageProfitMargin:Double,
                      )