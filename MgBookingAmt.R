dt.booking = data.table(read.csv("C:/Users/wgeyer/Downloads/Dealer Booking Amt by Location.csv"))
setnames(dt.booking,old=1:5,new=c("purchasezip","store","brand","yr_booking","amt"))

# booking amt and unique stores per zipcode
dt.booking_zip = dt.booking[yr_booking==2023,list(amt=sum(amt)),by=list(purchasezip,store)][amt>0,list(amt_23ytd=sum(amt),Nstores=.N),by=purchasezip][order(-amt_23ytd)]

write.csv(dt.booking_zip,"C:/Users/wgeyer/Downloads/Sum23_Dealer Booking Amt by Location.csv")
