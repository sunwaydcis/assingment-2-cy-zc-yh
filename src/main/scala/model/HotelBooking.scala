package model

// This puts it inside the "model" package

case class HotelBooking(
                        days: Int,           
                        rooms: Int,           
                        hotel: String,
                        country: String,
                        hotelCity: String,
                        bookingPrice: Double,  
                        discount: Double,      
                        profitMargin: Double,  
                        visitors: Int          
                       ):
  