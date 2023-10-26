library(Hmisc)
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)

## 2023 LTM Pull ##
dt.ZipDealerProduct23 = data.table(read.csv("C:/Users/hbelton/Downloads/Zip x Store x Product Large Dataset_data.csv"))
dt.dataset_23 = dt.ZipDealerProduct23

setnames(dt.dataset_23,old=colnames(dt.dataset_23),new=c("Store_Name","Purchase_Zip","Product_Lvl4","Motorization","Measure_Name",
                                                         "Measure_Values","Booking_Amt","Booking_Units"))
dt.dataset_23 = dt.dataset_23[,list(`Store_Name`,`Purchase_Zip`,`Product_Lvl4`,`Motorization`,`Measure_Name`,`Measure_Values`)]

dt.dataset_23 = dcast(dt.dataset_23, Store_Name + Purchase_Zip + Product_Lvl4 + 
        Motorization ~ Measure_Name, value.var = "Measure_Values")
colnames(dt.dataset_23)
setnames(dt.dataset_23,old=colnames(dt.dataset_23),new=c("Store_Name","Purchase_Zip","Product_Lvl4","Motorization","Booking_2023",
                                                         "Client_Ext_23","Booking_Units_23","Std_Cost_23","MSRP_23"))
sum(dt.dataset_23$Booking_2023, na.rm = TRUE)

## 2022 LTM Pull ##
dt.ZipDealerProduct22 = data.table(read.csv("C:/Users/hbelton/Downloads/Zip x Store x Product Large Dataset_data (1).csv"))
dt.dataset_22 = dt.ZipDealerProduct22

setnames(dt.dataset_22,old=colnames(dt.dataset_22),new=c("Store_Name","Purchase_Zip","Product_Lvl4","Motorization","Measure_Name",
                                                         "Measure_Values","Booking_Amt","Booking_Units"))
dt.dataset_22 = dt.dataset_22[,list(`Store_Name`,`Purchase_Zip`,`Product_Lvl4`,`Motorization`,`Measure_Name`,`Measure_Values`)]

dt.dataset_22 = dcast(dt.dataset_22, Store_Name + Purchase_Zip + Product_Lvl4 + 
                        Motorization ~ Measure_Name, value.var = "Measure_Values")
colnames(dt.dataset_22)
setnames(dt.dataset_22,old=colnames(dt.dataset_22),new=c("Store_Name","Purchase_Zip","Product_Lvl4","Motorization","Booking_2022",
                                                         "Client_Ext_22","Booking_Units_22","Std_Cost_22","MSRP_22"))
sum(dt.dataset_23$Booking_2023, na.rm = TRUE)

dt.dealers_products = merge(dt.dataset_23,dt.dataset_22,by = c("Store_Name","Purchase_Zip","Product_Lvl4","Motorization"),all = T)
sum(dt.dealers_products$Booking_2023, na.rm = TRUE)

write.csv(dt.dealers_products,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/10.26.23 Zip x Store x Product.CSV")
"C:\Users\hbelton\OneDrive - Mars & Co Consulting LLC\Documents\Hunter Douglas\Tableau Overall Metrics"

columns_to_update <- c("Booking_2023","Client_Ext_23","Booking_Units_23","Std_Cost_23","MSRP_23","Booking_2022","Client_Ext_22","Booking_Units_22","Std_Cost_22","MSRP_22")
for (col in columns_to_update) {dt.dealers_products[is.na(dt.dealers_products[[col]]), (col) :=0]}

dt.dealers_products_excluding0_2023 = dt.dealers_products[Booking_2023 != 0]

#### Cost Factors By Product ####

dt.cf_product = dt.dealers_products[,list(Booking_2023=sum(Booking_2023),MSRP_2023=sum(MSRP_23)),by=c("Product_Lvl4","Motorization")]
dt.cf_product[, Cost_Factor := Booking_2023 / MSRP_2023]
dt.cf_product[,Cost_Factor := ifelse(is.na(as.numeric(Cost_Factor)),0,Cost_Factor)]
dt.cf_product = dt.cf_product[,list(Product_Lvl4,Motorization, Cost_Factor)]

## Merge onto Data## 

dt.dealers_products_cf_index = merge(dt.dealers_products_excluding0_2023,dt.cf_product,by = c("Product_Lvl4","Motorization"),all = T)
dt.dealers_products_cf_index = dt.dealers_products_cf_index[,list(Store_Name,Product_Lvl4,Motorization,Purchase_Zip,Booking_2023,Client_Ext_23,
                                   Booking_Units_23,Std_Cost_23,MSRP_23,Booking_2022,
                                   Client_Ext_22,Booking_Units_22,Std_Cost_22,MSRP_22,Cost_Factor)][complete.cases(dt.dealers_products_cf_index$Store_Name)]
dt.dealers_products_cf_index[, CF_Index := (Booking_2023 / MSRP_23) / Cost_Factor]


#### Cost Factors into POE's ####
dt.dealers_products_POE = data.table(read.csv("C:/Users/hbelton/Downloads/Unified External Data v2 (2).csv"))
install.packages("Hmisc")
library(data.table)
library(Hmisc)
fn.cut2_wtd <- function(var,wt,cuts=3) {
  dt.var = data.table(x=var,y=wt)[,og_order:=1:.N]
  dt.var = dt.var[order(x)][,wt_cumu := cumsum(y)/sum(y)]
  dt.var[,group := cut2(wt_cumu,cuts=seq(0,1,length.out=cuts+1))]
  xcuts = dt.var[,max(x),by=group]$V1
  dt.var[order(og_order),cut2(x=x,cuts=xcuts)]
}
dt.dealers_products_POE[, home_value_group := fn.cut2_wtd(SFH_Price_vF,Pop_2021,3)]
