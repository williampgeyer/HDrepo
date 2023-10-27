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
dt.dealers_costfactor_groupings = dt.dealers_costfactor_groupings[Booking_2023 > 0 & Booking_2022 > 0]
dt.dealers_costfactor_groupings[, Booking_Growth := (Booking_2023 / Booking_2022) - 1]

#### Grouping Growth and HD Size ####
dt.dealers_costfactor_groupings[,Booking_Growth_Group := fn.cut2_wtd(Booking_Growth,Pop_2021,3)]
summary(dt.dealers_costfactor_groupings$Booking_Growth_Group)
dt.dealers_costfactor_groupings[Booking_Growth_Group == "[-1.00e+00,-3.84e-01)", Booking_Growth_Group := "Low"][Booking_Growth_Group == "[-3.84e-01, 3.60e-01)", Booking_Growth_Group := "Medium"][Booking_Growth_Group == "[ 3.60e-01, 4.83e-01)", Booking_Growth_Group := "High"][Booking_Growth_Group == "[ 4.83e-01, 2.52e+18]", Booking_Growth_Group := "High"]

dt.dealers_costfactor_groupings[,Booking_Size_Group := fn.cut2_wtd(Booking_2023,Pop_2021,3)]
summary(dt.dealers_costfactor_groupings$Booking_Size_Group)
dt.dealers_costfactor_groupings[Booking_Size_Group == "[8.88e-16,1.36e+03)", Booking_Size_Group := "Low"][Booking_Size_Group == "[1.36e+03,6.14e+03)", Booking_Size_Group := "Medium"][Booking_Size_Group == "[6.14e+03,7.08e+03)", Booking_Size_Group := "High"][Booking_Size_Group == "[7.08e+03,2.46e+07]", Booking_Size_Group := "High"]

## Output ##
dt.std_dev_CF_Index = dt.dealers_costfactor_groupings[,list(Purchase_Zip,Store_Name,Product_Lvl4,Cost_Factor,CF_Index,Grouping,home_value_group,Booking_Growth_Group,Booking_Size_Group)]


















