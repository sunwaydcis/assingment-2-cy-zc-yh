package model

// This puts it inside the "model" package

case class HotelBooking(
                         hotel: String,         // Col Q
                         country: String,       // Col J
                         bookingPrice: Double,  // Col U
                         discount: Double,      // Col V
                         profitMargin: Double,  // Col X
                         visitors: Int          // Col L
                       )
