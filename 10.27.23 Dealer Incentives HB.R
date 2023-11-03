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
dt.cf_product[, Cost_Factor_Product := Booking_2023 / MSRP_2023]
dt.cf_product[,Cost_Factor_Product := ifelse(is.na(as.numeric(Cost_Factor_Product)),0,Cost_Factor_Product)]
dt.cf_product = dt.cf_product[,list(Product_Lvl4,Motorization, Cost_Factor_Product)]

## Merge onto Data## 

dt.dealers_products_cf_index = merge(dt.dealers_products_excluding0_2023,dt.cf_product,by = c("Product_Lvl4","Motorization"),all = T)
dt.dealers_products_cf_index = dt.dealers_products_cf_index[,list(Store_Name,Product_Lvl4,Motorization,Purchase_Zip,Booking_2023,Client_Ext_23,
                                   Booking_Units_23,Std_Cost_23,MSRP_23,Booking_2022,
                                   Client_Ext_22,Booking_Units_22,Std_Cost_22,MSRP_22,Cost_Factor_Product)][complete.cases(dt.dealers_products_cf_index$Store_Name)]
dt.dealers_products_cf_index[, CF_Index := (Booking_2023 / MSRP_23) / Cost_Factor_Product]

dt.dealers_products_cf_sd_acrossdealers = dt.dealers_products_cf_index[Booking_2023 > 0,list(total_booking=sum(Booking_2023),simple_avg=mean(Cost_Factor_Product),
                                            sd=sd(Cost_Factor_Product),N_dealers=.N),by=Product_Lvl4][order(-total_booking)]
write.csv(dt.dealers_products_cf_sd_acrossdealers,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/10.27.23 cf sd across products.csv")


#### Cost Factors into POE's ####
dt.dealers_products_POE = data.table(read.csv("C:/Users/hbelton/Downloads/Unified External Data v6.csv"))
install.packages("Hmisc")
library(data.table)
library(Hmisc)

#### Cut 2 ####
cut2 <- function(x, cuts, m=150, g, levels.mean=FALSE, digits, minmax=TRUE,
                 oneval=TRUE, onlycuts=FALSE, formatfun = format, ...)
{
  if (inherits(formatfun, "formula")) {
    if (!requireNamespace("rlang"))
      stop("Package 'rlang' must be installed to use formula notation")
    formatfun <- getFromNamespace('as_function', 'rlang')(formatfun)
    
  }
  
  method <- 1 ## 20may02
  x.unique <- sort(unique(c(x[!is.na(x)],if(!missing(cuts))cuts)))
  min.dif <- min(diff(x.unique))/2
  min.dif.factor <- 1
  
  ## Make formatted values look good
  if(missing(digits))
    digits <- if(levels.mean) 5 else 3
  
  ## add digits to formatfun's arguments if relevant
  format.args <- 
    if (any(c("...","digits") %in%  names(formals(args(formatfun))))) {
      c(digits = digits, list(...))
    } else {
      list(...)
    }
  
  oldopt <- options('digits')
  options(digits=digits)
  on.exit(options(oldopt))
  
  xlab <- attr(x, 'label')
  
  if(missing(cuts)) {
    nnm <- sum(!is.na(x))
    if(missing(g)) g <- max(1,floor(nnm/m))
    if(g < 1)
      stop('g must be >=1, m must be positive')
    
    options(digits=15)
    n <- table(x)
    xx <- as.double(names(n))
    options(digits=digits)
    cum <- cumsum(n)
    m <- length(xx)
    
    y <- as.integer(ifelse(is.na(x),NA,1))
    labs <- character(g)
    cuts <- approx(cum, xx, xout=(1:g)*nnm/g,
                   method='constant', rule=2, f=1)$y
    cuts[length(cuts)] <- max(xx)
    lower <- xx[1]
    upper <- 1e45
    up <- low <- double(g)
    i <- 0
    for(j in 1:g) {
      cj <- if(method==1 || j==1) cuts[j] else {
        if(i==0)
          stop('program logic error')
        s <- if(is.na(lower)) FALSE else xx >= lower
        cum.used <- if(all(s)) 0 else max(cum[!s])
        if(j==m) max(xx) else if(sum(s)<2) max(xx) else
          approx(cum[s]-cum.used, xx[s], xout=(nnm-cum.used)/(g-j+1),
                 method='constant', rule=2, f=1)$y
      }
      
      if(cj==upper) next
      
      i <- i + 1
      upper <- cj
      y[x >= (lower-min.dif.factor*min.dif)]  <- i
      low[i] <- lower
      lower <- if(j==g) upper else min(xx[xx > upper])
      
      if(is.na(lower)) lower <- upper
      
      up[i]  <- lower
    }
    
    low  <- low[1:i]
    up   <- up[1:i]
    variation <- logical(i)
    for(ii in 1:i) {
      r <- range(x[y==ii], na.rm=TRUE)
      variation[ii] <- diff(r) > 0
    }
    if(onlycuts) return(unique(c(low, max(xx))))
    flow <- do.call(formatfun,c(list(low), format.args))
    fup  <- do.call(formatfun,c(list(up),  format.args))
    bb   <- c(rep(')',i-1),']')
    labs <- ifelse(low==up | (oneval & !variation), flow,
                   paste('[',flow,',',fup,bb,sep=''))
    ss <- y==0 & !is.na(y)
    if(any(ss))
      stop(paste('categorization error in cut2.  Values of x not appearing in any interval:\n',
                 paste(format(x[ss],digits=12),collapse=' '),
                 '\nLower endpoints:',
                 paste(format(low,digits=12), collapse=' '),
                 '\nUpper endpoints:',
                 paste(format(up,digits=12),collapse=' ')))
    
    y <- structure(y, class='factor', levels=labs)
  } else {
    if(minmax) {
      r <- range(x, na.rm=TRUE)
      if(r[1]<cuts[1]) cuts <- c(r[1], cuts)
      if(r[2]>max(cuts)) cuts <- c(cuts, r[2])
    }
    
    l <- length(cuts)
    k2 <- cuts-min.dif
    k2[l] <- cuts[l]
    y <- cut(x, k2)
    
    if(!levels.mean) {
      brack <- rep(")",l-1)
      brack[l-1] <- "]"
      fmt <- do.call(formatfun,c(list(cuts), format.args))
      ## If any interval has only one unique value, set label for
      ## that interval to that value and not to an interval
      labs <- paste("[",fmt[1:(l-1)],",",fmt[2:l],
                    brack,sep="")   
      
      if(oneval) {
        nu <- table(cut(x.unique,k2))
        
        if(length(nu)!=length(levels(y)))
          stop('program logic error')
        levels(y) <- ifelse(nu==1,c(fmt[1:(l-2)],fmt[l]),labs)
      } else
        levels(y) <- labs
    }
  }
  
  if(levels.mean) {
    means <- tapply(x, y, function(w)mean(w,na.rm=TRUE))
    levels(y) <- do.call(formatfun,c(list(means), format.args))
  }
  attr(y,'class') <- "factor"
  if(length(xlab)) label(y) <- xlab
  y
}
fn.cut2_wtd <- function(var,wt,cuts=3) {
  dt.var = data.table(x=var,y=wt)[,og_order:=1:.N]
  dt.var = dt.var[order(x)][,wt_cumu := cumsum(y)/sum(y)]
  dt.var[,group := cut2(wt_cumu,cuts=seq(0,1,length.out=cuts+1))]
  xcuts = dt.var[,max(x),by=group]$V1
  dt.var[order(og_order),cut2(x=x,cuts=xcuts)]
}
var=dt.dealers_home_value$Median_census_home_value
wt=dt.dealers_home_value$Pop_2021
cuts=3
#### Resume with Zip Code Classifications####

dt.dealers_home_value = dt.dealers_products_POE[,list(X,ZCTA,Zip.Code,Median_census_home_value,Pop_2021,Grouping)]

dt.dealers_home_value = dt.dealers_home_value[complete.cases(dt.dealers_home_value$Median_census_home_value)]
dt.dealers_home_value = dt.dealers_home_value[complete.cases(dt.dealers_home_value$Pop_2021)]

dt.dealers_home_value[,home_value_group := fn.cut2_wtd(Median_census_home_value,Pop_2021,3)]
dt.dealers_home_value[home_value_group == "[   9999, 183800)", home_value_group := "Low"][home_value_group == "[ 183800, 331900)", home_value_group := "Medium"][home_value_group == "[ 331900,2000001]", home_value_group := "High"]

write.csv(dt.dealers_home_value,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/10.26.23 Zip x Store x HomeValue Pick.CSV")

#### Metging the Value Groups onto ht HD Data
setnames(dt.dealers_home_value,old=colnames(dt.dealers_home_value),new=c("X","ZCTA","Purchase_Zip","Median_census_home_value","Pop_2021","Grouping","home_value_group"))
dt.dealers_costfactor_groupings_predrop = merge(dt.dealers_products_cf_index,dt.dealers_home_value,by=c("Purchase_Zip"),all=TRUE)

## Remove NA's ##
## Before Zip Drop: '23 Booking is 1,181,733,847; After: 1,178,394,949 -- 4M loss ##
dt.dealers_costfactor_groupings = dt.dealers_costfactor_groupings_predrop[complete.cases(Store_Name)]
dt.dealers_costfactor_groupings = dt.dealers_costfactor_groupings[complete.cases(Purchase_Zip)]
dt.dealers_costfactor_groupings = dt.dealers_costfactor_groupings[complete.cases(Grouping)]
sum(dt.dealers_costfactor_groupings$Booking_2023)
## Remove those with no booking amt ltm or tmp
dt.dealers_costfactor_groupings = dt.dealers_costfactor_groupings[Booking_2023 >= 0 & Booking_2022 > 0]
dt.dealers_costfactor_groupings[, Booking_Growth := (Booking_2023 / Booking_2022) - 1]

#### Grouping Growth and HD Size ####
dt.dealers_costfactor_groupings[,Booking_Growth_Group := fn.cut2_wtd(Booking_Growth,Pop_2021,3)]
summary(dt.dealers_costfactor_groupings$Booking_Growth)
dt.dealers_costfactor_groupings[Booking_Growth_Group == "[-1.00e+00,-3.84e-01)", Booking_Growth_Group := "Low"][Booking_Growth_Group == "[-3.84e-01, 3.60e-01)", Booking_Growth_Group := "Medium"][Booking_Growth_Group == "[ 3.60e-01, 4.83e-01)", Booking_Growth_Group := "High"][Booking_Growth_Group == "[ 4.83e-01, 2.52e+18]", Booking_Growth_Group := "High"]

dt.dealers_costfactor_groupings[,Booking_Size_Group := fn.cut2_wtd(Booking_2023,Pop_2021,3)]
summary(dt.dealers_costfactor_groupings$Booking_Size_Group)
dt.dealers_costfactor_groupings[Booking_Size_Group == "[8.88e-16,1.36e+03)", Booking_Size_Group := "Low"][Booking_Size_Group == "[1.36e+03,6.14e+03)", Booking_Size_Group := "Medium"][Booking_Size_Group == "[6.14e+03,7.08e+03)", Booking_Size_Group := "High"][Booking_Size_Group == "[7.08e+03,2.46e+07]", Booking_Size_Group := "High"]

#### Output ####
dt.std_dev_CF_Index = dt.dealers_costfactor_groupings[,list(Purchase_Zip,Store_Name,Product_Lvl4,Cost_Factor_Product,CF_Index,Booking_2023,MSRP_23,Grouping,home_value_group,Booking_Growth_Group,Booking_Size_Group)]

## eliminate the NA's and the Infinite's
dt.std_dev_CF_Index_quick_example = dt.std_dev_CF_Index[home_value_group == "Medium" & Grouping == "Sub_Urban"]
dt.std_dev_CF_Index_quick_example = dt.std_dev_CF_Index_quick_example[complete.cases(CF_Index)]
dt.std_dev_CF_Index_quick_example = dt.std_dev_CF_Index_quick_example[is.finite(CF_Index)]
dt.std_dev_CF_Index_quick_example_output = dt.std_dev_CF_Index_quick_example[,.(SD = sd(CF_Index)), by = .(Booking_Growth_Group,Booking_Size_Group)]

dt.std_dev_CF_Index[home_value_group == "Medium" & Grouping == "Sub_Urban" & Booking_Growth_Group == "Medium" & Booking_Size_Group == "Medium"][order(CF_Index)][complete.cases(CF_Index)][is.finite(CF_Index)]



dt.std_dev_CF_Index = dt.std_dev_CF_Index[is.finite(CF_Index)]
dt.std_dev_CF_Index = dt.std_dev_CF_Index[complete.cases(CF_Index)]
## Throwing out any Dealers where the index is > 2 ##
dt.std_dev_CF_Index_subset = dt.std_dev_CF_Index[CF_Index < 2]
dt.full_sd_output = dt.std_dev_CF_Index_subset[,.(SD = sd(CF_Index),simple_avg = mean(Cost_Factor)), by = .(Grouping, home_value_group,Booking_Growth_Group,Booking_Size_Group)]
summary(dt.std_dev_CF_Index)

write.csv(dt.full_sd_output,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/10.26.23 full_sd_output.csv")

dt.dealers_costfactor_groupings_excladvertising = dt.dealers_costfactor_groupings[!(Product_Lvl4 %in% "Remotes") & !(Product_Lvl4 %in% "Advertising & Promotional Material")]
dt.dealers_avgcostfactor_dealerbookingsize = dt.dealers_costfactor_groupings_excladvertising[,list(Booking_2023=sum(Booking_2023),Booking_2022=sum(Booking_2022),
                                                                                   MSRP_23=sum(MSRP_23)),by=c("Purchase_Zip","Store_Name")]
dt.dealers_avgcostfactor_dealerbookingsize[, Cost_Factor := (Booking_2023 / MSRP_23)]
dt.dealers_avgcostfactor_dealerbookingsize = dt.dealers_avgcostfactor_dealerbookingsize[is.finite(Cost_Factor)]
dt.dealers_avgcostfactor_dealerbookingsize[,CF_Index_byDealer := (Cost_Factor / ((sum(Booking_2023) / sum(MSRP_23))))]

### Method 2 ###
dt.dealers_costfactor_groupings_excladvertising_finite = dt.dealers_costfactor_groupings_excladvertising[!is.na(Cost_Factor_Product)]
dt.dealers_costfactor_groupings_excladvertising_finite = dt.dealers_costfactor_groupings_excladvertising_finite[!is.na(CF_Index)]
dt.dealers_costfactor_groupings_excladvertising_finite = dt.dealers_costfactor_groupings_excladvertising_finite[is.finite(Cost_Factor_Product)]
dt.dealers_costfactor_groupings_excladvertising_finite = dt.dealers_costfactor_groupings_excladvertising_finite[is.finite(CF_Index)]
dt.dealer_avgcost_by_dealer = dt.dealers_costfactor_groupings_excladvertising_finite[,list(Booking_2023=sum(Booking_2023),Booking_2022=sum(Booking_2022),
                                                              MSRP_23=sum(MSRP_23),Avg_CF_Index=mean(CF_Index)),by=c("Purchase_Zip","Store_Name")]

## Method 3 ##
dt.dealer_avgcost_by_dealer_weighted = dt.dealers_costfactor_groupings_excladvertising_finite[,list(Booking_2023=sum(Booking_2023),Booking_2022=sum(Booking_2022),
                                                              MSRP_23=sum(MSRP_23),Avg_CF_Index=weighted.mean(CF_Index,Booking_2023)),by=c("Purchase_Zip","Store_Name")]
dt.Pop_by_Zip = dt.dealers_costfactor_groupings_excladvertising_finite[,list(Purchase_Zip,Pop_2021,ZCTA,home_value_group,Grouping)][!duplicated(Purchase_Zip)]
dt.dealer_avgcost_by_dealer_weighted = merge(dt.dealer_avgcost_by_dealer_weighted,dt.Pop_by_Zip,all = T)

dt.dealer_avgcost_by_dealer_weighted[,Booking_Size_Group := fn.cut2_wtd(Booking_2023,Pop_2021,3)]
summary(dt.dealer_avgcost_by_dealer_weighted$Booking_Size_Group)
dt.dealer_avgcost_by_dealer_weighted[Booking_Size_Group == "[5.68e-14,6.80e+03)", Booking_Size_Group := "Low"][Booking_Size_Group == "[6.80e+03,4.28e+04)", Booking_Size_Group := "Medium"][Booking_Size_Group == "[4.28e+04,3.87e+07]", Booking_Size_Group := "High"]

dt.dealer_avgcost_by_dealer_weighted[,Booking_Growth := Booking_2023/Booking_2022 - 1]

dt.dealer_avgcost_by_dealer_weighted[,Booking_Growth_Group := fn.cut2_wtd(Booking_Growth,Pop_2021,3)]
summary(dt.dealer_avgcost_by_dealer_weighted$Booking_Growth_Group)
dt.dealer_avgcost_by_dealer_weighted[Booking_Growth_Group == "[  -1.000,  -0.285)", Booking_Growth_Group := "Low"][Booking_Growth_Group == "[  -0.285,   0.154)", Booking_Growth_Group := "Medium"][Booking_Growth_Group == "[   0.154,4749.000]", Booking_Growth_Group := "High"]

## Breaking the CF into segments
summary(dt.dealer_avgcost_by_dealer_weighted$Avg_CF_Index)
intervals <- c(0,0.9,1.1,150000)
dt.dealer_avgcost_by_dealer_weighted[,Avg_Range := cut(Avg_CF_Index, breaks = intervals, labels = c("Below Avg", "Avg. Range", "Above Avg."))]
summary(dt.dealer_avgcost_by_dealer_weighted$Avg_Range)

dt.dealer_avgcostfactor_final = dt.dealer_avgcost_by_dealer_weighted

summary(dt.dealer_avgcostfactor_final[home_value_group == "High"])

dt.dealer_avgcostfactor_final[Avg_CF_Index < 5,.(SD = sd(Avg_CF_Index))]
dt.dealer_avgcostfactor_final[home_value_group == "High" & Avg_CF_Index < 5,.(SD = sd(Avg_CF_Index))]



dt.dealer_avgcostfactor_final_over100 = dt.dealer_avgcostfactor_final[Booking_2023 > 1000]

summary(dt.dealer_avgcostfactor_final_over100[Booking_Size_Group == "High"])

dt.dealer_avgcostfactor_final_over100[Avg_CF_Index < 5,.(SD = sd(Avg_CF_Index))]
dt.dealer_avgcostfactor_final_over100[Booking_Size_Group == "High" & Avg_CF_Index < 5,.(SD = sd(Avg_CF_Index))]


#### excluding those with < 1000 sales in 2023 ####

dt.dealer_avgcost_by_dealer_weighted_500 = dt.dealers_costfactor_groupings_excladvertising_finite[,list(Booking_2023=sum(Booking_2023),Booking_2022=sum(Booking_2022),
                                                                                                    MSRP_23=sum(MSRP_23),Avg_CF_Index=weighted.mean(CF_Index,Booking_2023)),by=c("Purchase_Zip","Store_Name")]
dt.Pop_by_Zip = dt.dealers_costfactor_groupings_excladvertising_finite[,list(Purchase_Zip,Pop_2021,ZCTA,home_value_group,Grouping)][!duplicated(Purchase_Zip)]
dt.dealer_avgcost_by_dealer_weighted_500 = merge(dt.dealer_avgcost_by_dealer_weighted_500,dt.Pop_by_Zip,all = T)

dt.dealer_avgcost_by_dealer_weighted_500 = dt.dealer_avgcost_by_dealer_weighted_500[Booking_2023 > 1000]

dt.dealer_avgcost_by_dealer_weighted_500[,Booking_Size_Group := fn.cut2_wtd(Booking_2023,Pop_2021,3)]
summary(dt.dealer_avgcost_by_dealer_weighted_500$Booking_Size_Group)
dt.dealer_avgcost_by_dealer_weighted_500[Booking_Size_Group == "[    1003,    9773)", Booking_Size_Group := "Low"][Booking_Size_Group == "[    9773,   52023)", Booking_Size_Group := "Medium"][Booking_Size_Group == "[   52023,38675548]", Booking_Size_Group := "High"]

dt.dealer_avgcost_by_dealer_weighted_500[,Booking_Growth := Booking_2023/Booking_2022 - 1]

dt.dealer_avgcost_by_dealer_weighted_500[,Booking_Growth_Group := fn.cut2_wtd(Booking_Growth,Pop_2021,3)]
summary(dt.dealer_avgcost_by_dealer_weighted_500$Booking_Growth_Group)
dt.dealer_avgcost_by_dealer_weighted_500[Booking_Growth_Group == "[ -0.996, -0.238)", Booking_Growth_Group := "Low"][Booking_Growth_Group == "[ -0.238,  0.188)", Booking_Growth_Group := "Medium"][Booking_Growth_Group == "[  0.188,126.369]", Booking_Growth_Group := "High"]

## Breaking the CF into segments
summary(dt.dealer_avgcost_by_dealer_weighted_500$Avg_CF_Index)
intervals <- c(0,0.9,1.1,150000)
dt.dealer_avgcost_by_dealer_weighted_500[,Avg_Range := cut(Avg_CF_Index, breaks = intervals, labels = c("Below Avg", "Avg. Range", "Above Avg."))]
summary(dt.dealer_avgcost_by_dealer_weighted_500$Avg_Range)

dt.dealer_avgcostfactor_final_500 = dt.dealer_avgcost_by_dealer_weighted_500

summary(dt.dealer_avgcostfactor_final_500[Booking_Size_Group == "Low"])

dt.dealer_avgcostfactor_final_500[Avg_CF_Index < 5,.(SD = sd(Avg_CF_Index))]
dt.dealer_avgcostfactor_final_500[home_value_group == "High" & Avg_CF_Index < 5,.(SD = sd(Avg_CF_Index))]












#### Margin Analysis ####

dt.dealers_margin = dt.dealers_products_excluding0_2023[,list(Store_Name,Purchase_Zip,Product_Lvl4,Motorization,Booking_2023,Client_Ext_23)]
dt.dealers_margin_quick = dt.dealers_margin[,list(Booking_2023=sum(Booking_2023),Client_Ext_23=sum(Client_Ext_23)),by=c("Store_Name","Purchase_Zip")]
dt.dealers_margin_quick[, Margin := (Client_Ext_23 - Booking_2023) / Client_Ext_23]
dt.margin_noclientext = dt.dealers_margin_quick
dt.margin_noclientext[Client_Ext_23 == 0,sum(Booking_2023)]
dt.dealers_margin_quick = dt.dealers_margin_quick[Client_Ext_23 > 0]

dt.dealers_margin_quick[is.na(Margin)]

summary(dt.dealers_margin_quick$Margin)
intervals <- c(-55701,0,.25,.5,.75,3000)
dt.dealers_margin_quick[,Avg_Range := cut(Margin, breaks = intervals, labels = c("Well Below Avg", "Below", "About Average","Above Average","Well Above Average"))]
summary(dt.dealers_margin_quick)

dt.dealers_margin_quick[,sum(Booking_2023),by="Avg_Range"]
dt.dealers_margin_quick[Avg_Range == "Well Above Average",sum(Booking_2023)]
dt.dealers_margin_quick[c]
dt.total_margin = dt.dealers_margin[,list(Booking_2023=sum(Booking_2023),Client_Ext_23=sum(Client_Ext_23))]
dt.total_margin[, Margin := (Client_Ext_23 - Booking_2023) / Client_Ext_23]
dt.total_margin_removezeores = dt.dealers_margin[,list(Booking_2023=sum(Booking_2023),Client_Ext_23=sum(Client_Ext_23)),by=c("Store_Name","Purchase_Zip")]
dt.total_margin_removezeores = dt.total_margin_removezeores[Client_Ext_23 > 0]
dt.total_margin_removezeores = dt.total_margin_removezeores[,list(Booking_2023=sum(Booking_2023),Client_Ext_23=sum(Client_Ext_23))]
dt.total_margin_removezeores[, Margin := (Client_Ext_23 - Booking_2023) / Client_Ext_23]

dt.dealerlocations_zip = dt.dealers_margin_quick[,list(Store_Name,Purchase_Zip)]

dt.dealerlocations_zip
write.csv(dt.dealerlocations_zip,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/Dealers and Locations.csv")


#### Example Dealer Data Issues ####

dt.data_issues = data.table(read.csv("C:/Users/hbelton/Downloads/Data Issues Check_data.csv"))

setnames(dt.data_issues,old=colnames(dt.data_issues),new=c("Store_Name","Purchase_Zip","Measure_Name",
                                                         "Measure_Values","Booking_Amt","Booking_Units"))
dt.data_issues = dt.data_issues[,list(`Store_Name`,`Purchase_Zip`,`Measure_Name`,`Measure_Values`)]

dt.data_issues = dcast(dt.data_issues, Store_Name + Purchase_Zip
                     ~ Measure_Name, value.var = "Measure_Values")
colnames(dt.data_issues)
setnames(dt.data_issues,old=colnames(dt.data_issues),new=c("Store_Name","Purchase_Zip","Booking_2023",
                                                         "Client_Ext_23","Booking_Units_23","Effective_Cost_Factor","Std_cost_23","MSRP_23"))
sum(dt.data_issues$Booking_2023, na.rm = TRUE)

dt.data_issues_check = dt.data_issues[,list(Store_Name,Purchase_Zip,Booking_2023,Client_Ext_23,MSRP_23)]

dt.data_issues_check = dt.data_issues_check[Booking_2023 > 100000][order(-Booking_2023)][, Dealer_Margin := (Client_Ext_23 - Booking_2023) / Client_Ext_23]
dt.data_issues_check = dt.data_issues_check[Dealer_Margin < 0]


#### Dealer Locations Number of Spots ####
dt.dealerlocations_zip_ownercount = data.table(read.csv("C:/Users/hbelton/Downloads/Dealer Locations Count_data.csv"))
dt.dealerlocations_zip_ownercount[,N := 1]
setnames(dt.dealerlocations_zip_ownercount,old=colnames(dt.dealerlocations_zip_ownercount),new=c("Company","Store_Name","Purchase_Zip","Booking_2023","Count"))
dt.dealerlocations_zip_ownercount[Company == "HD Brand" & Booking_2023 == 0,sum(Count)]
length(unique(dt.dealerlocations_zip_ownercount$Store_Name))

write.csv(dt.dealerlocations_zip_ownercount,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/Dealers and Locations.csv")

#### Dealer Ownership Rollup #### 
dt.dealerlocations_owner_1 = data.table(read.csv("C:/Users/hbelton/Downloads/Owner Rollup_data (1).csv"))
setnames(dt.dealerlocations_owner_1,old=colnames(dt.dealerlocations_owner_1),new=c("Owner","Store_Name","Booking_2023"))
dt.dealerlocations_owner_2 = data.table(read.csv("C:/Users/hbelton/Downloads/Owner Rollup_data (2).csv"))
setnames(dt.dealerlocations_owner_2,old=colnames(dt.dealerlocations_owner_2),new=c("Owner","Store_Name","Booking_2022"))
dt.dealerlocations_owner = merge(dt.dealerlocations_owner_1,dt.dealerlocations_owner_2,by=c("Owner","Store_Name"),all = TRUE)
dt.dealerlocations_owner[, N := 1]
dt.dealerlocations_owner_count = dt.dealerlocations_owner[,list(Booking_2023=sum(Booking_2023,na.rm = TRUE),Booking_2022=sum(Booking_2022,na.rm = TRUE),N=sum(N,na.rm = TRUE)),by="Owner"][order(-N)]

intervals <- c(0,1,100,1300)
dt.dealerlocations_owner_count[,Store_Count := cut(N, breaks = intervals, labels = c("One", "Two to 100", "More than 100"))]
summary(dt.dealerlocations_owner_count)
dt.dealerlocations_owner_count[Store_Count == "More than 100"]
dt.dealerlocations_owner_count = dt.dealerlocations_owner_count[Booking_2022>0][Booking_2023<0,Booking_2023:=0][,Booking_Growth := Booking_2023/Booking_2022 - 1]
dt.dealerlocations_owner_count[, N2 := 1]
intervals <- c(0,-5,0,5,100000)
dt.dealerlocations_owner_count[,Growth_Group := cut(N, breaks = intervals, labels = c("Less than -5", "-5 to flat", "flat to 5","Greater than 5"))]
dt.dealerlocations_owner_groups = dt.dealerlocations_owner_count[,list(Booking_2023=sum(Booking_2023),Booking_2022=sum(Booking_2022),Stores=sum(N),Owners=sum(N2)),by="Store_Count"]
write.csv(dt.dealerlocations_owner_groups,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/Ownership Store Count.csv")

intervals <- c(-100000,-5,0,5,1000000)
dt.dealerlocations_owner_count[,Growth_Group := cut(N, breaks = intervals, labels = c("Less than 5", "-5 to 0", "0 to 5","Greater than 5"))]

dt.dealerlocations_owner_count_rolled_to_owners = 
  
#### Dealer Ownership Rollup by Customer ID ####
dt.dealerlocations_owner_1_ID = data.table(read.csv("C:/Users/hbelton/Downloads/Owner Rollup_data (3).csv"))
setnames(dt.dealerlocations_owner_1_ID,old=colnames(dt.dealerlocations_owner_1_ID),new=c("Owner","ID","Store_Name","Booking_2023"))
dt.dealerlocations_owner_2_ID = data.table(read.csv("C:/Users/hbelton/Downloads/Owner Rollup_data (4).csv"))
setnames(dt.dealerlocations_owner_2_ID,old=colnames(dt.dealerlocations_owner_2_ID),new=c("Owner","ID","Store_Name","Booking_2022"))
dt.dealerlocations_owner_ID = merge(dt.dealerlocations_owner_1_ID,dt.dealerlocations_owner_2_ID,by=c("Owner","ID","Store_Name"),all = TRUE)
dt.dealerlocations_owner_ID[, N := 1]
dt.dealerlocations_owner_count_ID = dt.dealerlocations_owner_ID[,list(Booking_2023=sum(Booking_2023,na.rm = TRUE),Booking_2022=sum(Booking_2022,na.rm = TRUE),N=sum(N,na.rm = TRUE)),by="ID"][order(-N)]

intervals <- c(0,1,100,1300)
dt.dealerlocations_owner_count_ID[,Store_Count := cut(N, breaks = intervals, labels = c("One", "Two to 100", "More than 100"))]
summary(dt.dealerlocations_owner_count_ID)
dt.dealerlocations_owner_count_ID[Store_Count == "Two to 100"]
dt.dealerlocations_owner_count_ID = dt.dealerlocations_owner_count_ID[Booking_2022>0][Booking_2023<0,Booking_2023:=0][,Booking_Growth := Booking_2023/Booking_2022 - 1]
dt.dealerlocations_owner_count_ID[, N2 := 1]
intervals <- c(-10000,-.05,0,.05,100000)
dt.dealerlocations_owner_count_ID[,Growth_Group := cut(N, breaks = intervals, labels = c("Less than -5", "-5 to flat", "flat to 5","Greater than 5"))]
dt.dealerlocations_owner_groups_ID = dt.dealerlocations_owner_count_ID[,list(Booking_2023=sum(Booking_2023),Booking_2022=sum(Booking_2022),Stores=sum(N),Owners=sum(N2)),by="Store_Count"]

intervals <- c(-100000,-.05,0,.05,1000000)
dt.dealerlocations_owner_count_ID_noNA = dt.dealerlocations_owner_count_ID[complete.cases(Store_Count)]
dt.dealerlocations_owner_count_ID_noNA[,Growth_Group := cut(Booking_Growth, breaks = intervals, labels = c("Less than 5", "-5 to 0", "0 to 5","Greater than 5"))]
summary(dt.dealerlocations_owner_count_ID_noNA$Growth_Group)
dt.dealerlocations_owner_count_ID_noNA_pull = dt.dealerlocations_owner_count_ID_noNA[,list(Booking_2023=sum(Booking_2023),Booking_2022=sum(Booking_2022)),by="Growth_Group"]
dt.dealerlocations_owner_count_ID_Over100 = dt.dealerlocations_owner_count_ID_noNA[Store_Count == "More than 100"][,list(.N,Booking_2023=sum(Booking_2023),Booking_2022=sum(Booking_2022)),by="Growth_Group"]

write.csv(dt.dealerlocations_owner_count_ID_Over100,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/Ownership Store Count Over 100.csv")








dt.dealers_highvslowgrowth = data.table(read.csv("C:/Users/hbelton/Downloads/Unified External Data v7 (1).csv"))
dt.dealers_highvslowgrowth = dt.dealers_highvslowgrowth[,list(X,ZCTA,Zip.Code,Median_Household_Income,Median_Age_Total,Pop_2021,Pop_2018,Own_vs_rent_own_Perc,Median_census_home_value,households)]
setnames(dt.dealers_highvslowgrowth,old=colnames(dt.dealers_highvslowgrowth),new=c("X","ZCTA","Purchase_Zip","HH_Income","Age","Pop_2021","Pop_2018","Own_Perc","Home_Value","Households"))

intervals <- c(-10000,-.05,0,.05,100000)
dt.dealer_avgcost_by_dealer_weighted[,Booking_Growth_Group := cut(Booking_Growth, breaks = intervals, labels = c("Less than -5", "-5 to flat", "flat to 5","Greater than 5"))]

dt.dealer_density = data.table(read.csv("C:/Users/hbelton/Downloads/henry.csv"))
dt.dealer_density = dt.dealer_density[,list(Purchase_Zip = purchasezip, Dealers_per_10k_HHs_bucket)]

dt.dealers_highvslowgrowth = merge(dt.dealer_avgcost_by_dealer_weighted,dt.dealers_highvslowgrowth,by=c("Purchase_Zip"))
dt.dealers_highvslowgrowth = dt.dealers_highvslowgrowth[,list(Purchase_Zip,Store_Name,Booking_2023,Pop_2021=Pop_2021.x,Booking_Growth,Booking_Growth_Group,HH_Income,Age,Pop_2018,Own_Perc,Home_Value)]
dt.dealer_density = dt.dealer_density[!duplicated(Purchase_Zip)]
dt.dealers_highvslowgrowth = merge(dt.dealers_highvslowgrowth,dt.dealer_density,by=c("Purchase_Zip"))
dt.dealers_highgrowth = dt.dealers_highvslowgrowth[Booking_Growth_Group == "Greater than 5"]
write.csv(dt.dealers_highgrowth,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/Growth over 5 breakout.csv")


dt.dealers_lowgrowth = dt.dealers_highvslowgrowth[Booking_Growth_Group == "Less than -5"]
write.csv(dt.dealers_lowgrowth,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/Growth under 5 breakout.csv")



setnames(dt.dealers_home_value,old=colnames(dt.dealers_home_value),new=c("X","ZCTA","Purchase_Zip","Median_census_home_value","Pop_2021","Grouping","home_value_group"))
dt.dealers_costfactor_groupings_predrop = merge(dt.dealers_products_cf_index,dt.dealers_home_value,by=c("Purchase_Zip"),all=TRUE)
