library(Hmisc)
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(writexl)

## 2023 LTM Pull ##
dt.ZipDealerProduct9mo = data.table(read.csv("C:/Users/hbelton/Downloads/Zip x Store x Product Large Dataset_data (2).csv"))
dt.dataset_23_9mo = dt.ZipDealerProduct9mo

setnames(dt.dataset_23_9mo,old=colnames(dt.dataset_23_9mo),new=c("Store_Name","Purchase_Zip","Product_Lvl4","Motorization","Measure_Name",
                                                         "Measure_Values","Booking_Amt","Booking_Units"))
dt.dataset_23_9mo = dt.dataset_23_9mo[,list(`Store_Name`,`Purchase_Zip`,`Product_Lvl4`,`Motorization`,`Measure_Name`,`Measure_Values`)]

dt.dataset_23_9mo = dcast(dt.dataset_23_9mo, Store_Name + Purchase_Zip + Product_Lvl4 + 
                        Motorization ~ Measure_Name, value.var = "Measure_Values")
colnames(dt.dataset_23_9mo)
setnames(dt.dataset_23_9mo,old=colnames(dt.dataset_23_9mo),new=c("Store_Name","Purchase_Zip","Product_Lvl4","Motorization","Booking_2023",
                                                         "Client_Ext_23","Booking_Units_23","Std_Cost_23","MSRP_23"))
sum(dt.dataset_23_9mo$Booking_2023, na.rm = TRUE)

## 2022 LTM Pull ##
dt.ZipDealerProduct22_9mo = data.table(read.csv("C:/Users/hbelton/Downloads/Zip x Store x Product Large Dataset_data (3).csv"))
dt.dataset_22_9mo = dt.ZipDealerProduct22_9mo

setnames(dt.dataset_22_9mo,old=colnames(dt.dataset_22_9mo),new=c("Store_Name","Purchase_Zip","Product_Lvl4","Motorization","Measure_Name",
                                                         "Measure_Values","Booking_Amt","Booking_Units"))
dt.dataset_22_9mo = dt.dataset_22_9mo[,list(`Store_Name`,`Purchase_Zip`,`Product_Lvl4`,`Motorization`,`Measure_Name`,`Measure_Values`)]

dt.dataset_22_9mo = dcast(dt.dataset_22_9mo, Store_Name + Purchase_Zip + Product_Lvl4 + 
                        Motorization ~ Measure_Name, value.var = "Measure_Values")
colnames(dt.dataset_22_9mo)
setnames(dt.dataset_22_9mo,old=colnames(dt.dataset_22_9mo),new=c("Store_Name","Purchase_Zip","Product_Lvl4","Motorization","Booking_2022",
                                                         "Client_Ext_22","Booking_Units_22","Std_Cost_22","MSRP_22"))
sum(dt.dataset_22_9mo$Booking_2022, na.rm = TRUE)

dt.dealers_products_9mo = merge(dt.dataset_23_9mo,dt.dataset_22_9mo,by = c("Store_Name","Purchase_Zip","Product_Lvl4","Motorization"),all = T)
sum(dt.dealers_products_9mo$Booking_2023, na.rm = TRUE)

columns_to_update <- c("Booking_2023","Client_Ext_23","Booking_Units_23","Std_Cost_23","MSRP_23","Booking_2022","Client_Ext_22","Booking_Units_22","Std_Cost_22","MSRP_22")
for (col in columns_to_update) {dt.dealers_products_9mo[is.na(dt.dealers_products_9mo[[col]]), (col) :=0]}

dt.dealers_products_excluding0_2023_9mo = dt.dealers_products_9mo[Booking_2023 != 0]

dt.dealers_products_9mo = dt.dealers_products_9mo[,list(Booking_2023 = sum(Booking_2023),Booking_2022=sum(Booking_2022)),by=c("Store_Name","Purchase_Zip")]
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

setnames(dt.dealers_home_value,old=colnames(dt.dealers_home_value),new=c("X","ZCTA","Purchase_Zip","Median_census_home_value","Pop_2021","Grouping","home_value_group"))
dt.dealers_9mo = merge(dt.dealers_products_9mo,dt.dealers_home_value,by=c("Purchase_Zip"),all=TRUE)

## Remove NA's ##
dt.dealers_9mo = dt.dealers_9mo[complete.cases(Store_Name)]
dt.dealers_9mo = dt.dealers_9mo[complete.cases(Purchase_Zip)]
dt.dealers_9mo = dt.dealers_9mo[complete.cases(Grouping)]
sum(dt.dealers_9mo$Booking_2023)
## Remove those with no booking amt ltm or tmp
dt.dealers_9mo = dt.dealers_9mo[Booking_2023 < 0, Booking_2023 := 0][Booking_2022 > 0]
dt.dealers_9mo[, Booking_Growth := (Booking_2023 / Booking_2022) - 1]

dt.9months_output = dt.dealers_9mo[Grouping == "Sub_Urban",list(N_dealers = .N,Booking_2023 = sum(Booking_2023)),by=list(Booking_Growth=cut2(Booking_Growth,cuts=c(-0.05,0,0.05)))]

write.csv(dt.9months_output,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/10.28.23 9 months pull_Suburban.csv")
sum(dt.dealers_9mo$Booking_2023)





#### Grouping Growth and HD Size ####
dt.dealers_costfactor_groupings[,Booking_Growth_Group := fn.cut2_wtd(Booking_Growth,Pop_2021,3)]
summary(dt.dealers_costfactor_groupings$Booking_Growth)
dt.dealers_costfactor_groupings[Booking_Growth_Group == "[-1.00e+00,-3.84e-01)", Booking_Growth_Group := "Low"][Booking_Growth_Group == "[-3.84e-01, 3.60e-01)", Booking_Growth_Group := "Medium"][Booking_Growth_Group == "[ 3.60e-01, 4.83e-01)", Booking_Growth_Group := "High"][Booking_Growth_Group == "[ 4.83e-01, 2.52e+18]", Booking_Growth_Group := "High"]

dt.dealers_costfactor_groupings[,Booking_Size_Group := fn.cut2_wtd(Booking_2023,Pop_2021,3)]
summary(dt.dealers_costfactor_groupings$Booking_Size_Group)
dt.dealers_costfactor_groupings[Booking_Size_Group == "[8.88e-16,1.36e+03)", Booking_Size_Group := "Low"][Booking_Size_Group == "[1.36e+03,6.14e+03)", Booking_Size_Group := "Medium"][Booking_Size_Group == "[6.14e+03,7.08e+03)", Booking_Size_Group := "High"][Booking_Size_Group == "[7.08e+03,2.46e+07]", Booking_Size_Group := "High"]

#### Output ####
dt.std_dev_CF_Index = dt.dealers_costfactor_groupings[,list(Purchase_Zip,Store_Name,Product_Lvl4,Cost_Factor_Product,CF_Index,Booking_2023,MSRP_23,Grouping,home_value_group,Booking_Growth_Group,Booking_Size_Group)]

dt.owner_rollup = data.table(read.csv("C:/Users/hbelton/Downloads/Owner Rollup_data.csv"))

setnames(dt.owner_rollup,old=colnames(dt.owner_rollup),new=c("Level_1","Level_2","Level_3","Booking_2023","Booking_Units_2023"))

length(unique(dt.owner_rollup$Level_3))


#### Trended Sales 2020-2023 ####
dt.trended_sales = data.table(read.csv("C:/Users/hbelton/Downloads/Trended Sales YoY 20-23_data.csv"))
setnames(dt.trended_sales,old=colnames(dt.trended_sales),new=c("Year","Month","Company","Booking"))
write.csv(dt.trended_sales,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/Trended Sales 20-23.csv")

#### More Buckets of Growth ####
dt.dealers_growth_buckets = dt.dealers_costfactor_groupings[,list(Booking_2023=sum(Booking_2023),Booking_2022=sum(Booking_2022)),by=c("Purchase_Zip","Store_Name")]
dt.dealers_growth_buckets[, Booking_Growth := (Booking_2023 / Booking_2022) - 1]
intervals <- c(-1,-.5,-.2,-.1,0,.1,.2,.5,1)
dt.dealers_growth_buckets[,Growth_Group := cut(Booking_Growth, breaks = intervals, labels = c("-100 to -50", "-50 to -20", "-20 to -10","-10 to 0","0 to 10","10 to 20","20 to 50","50 to 100"))]
summary(dt.dealers_growth_buckets$Booking_Growth)
dt.dealers_growth_buckets_pull = dt.dealers_growth_buckets[,list(.N,Booking_2023=sum(Booking_2023)),by=Growth_Group]
dt.dealers_growth_buckets_pull = dt.dealers_growth_buckets_pull[order(Growth_Group)]

write.table(dt.dealers_growth_buckets_pull,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

## By Owner ##
dt.owners_growth_buckets_23 = data.table(read.csv("C:/Users/hbelton/Downloads/Owner Rollup_data (7).csv"))
setnames(dt.owners_growth_buckets_23,old=colnames(dt.owners_growth_buckets_23),new=c("Owner","Booking_2023"))
dt.owners_growth_buckets_22 = data.table(read.csv("C:/Users/hbelton/Downloads/Owner Rollup_data (5).csv"))
setnames(dt.owners_growth_buckets_22,old=colnames(dt.owners_growth_buckets_22),new=c("Owner","Booking_2022"))
dt.owners_growth_buckets = merge(dt.owners_growth_buckets_23,dt.owners_growth_buckets_22,by = c("Owner"),all = T)
dt.owners_growth_buckets[, Booking_Growth := (Booking_2023 / Booking_2022) - 1]
intervals <- c(-1,-.5,-.2,-.1,0,.1,.2,.5,1)
dt.owners_growth_buckets[,Growth_Group := cut(Booking_Growth, breaks = intervals, labels = c("-100 to -50", "-50 to -20", "-20 to -10","-10 to 0","0 to 10","10 to 20","20 to 50","50 to 100"))]
dt.owners_growth_buckets_pull = dt.owners_growth_buckets[,list(.N,Booking_2023=sum(Booking_2023)),by=Growth_Group][order(Growth_Group)]
write.table(dt.owners_growth_buckets_pull,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

## Owner x Size ##
dt.owners_size_buckets = dt.owners_growth_buckets[,list(Owner,Booking_2023,Booking_2022)]
dt.owners_size_buckets[Booking_2023 < 0,Booking_2023 := 0]
intervals <- c(0,36000,100000,150000,200000,250000,300000,350000,80000000)
dt.owners_size_buckets[,Size_Group := cut(Booking_2023, breaks = intervals, labels = c("0 to 36K","36K to 100K","100K to 150K","150K to 200K","200K to 250K","250K to 300K","300K to 350K","Greater than 350K"))]
dt.owners_size_buckets[, Booking_Growth := (Booking_2023 / Booking_2022) - 1]
intervals <- c(-1,-.5,-.2,-.1,0,.1,.2,.5,1)
dt.owners_size_buckets[,Growth_Group := cut(Booking_Growth, breaks = intervals, labels = c("-100 to -50", "-50 to -20", "-20 to -10","-10 to 0","0 to 10","10 to 20","20 to 50","50 to 100"))]
dt.owners_size_buckets_Pull = dt.owners_size_buckets[,list(.N,Booking_2023=sum(Booking_2023),Booking_2022=sum(Booking_2022,na.rm = TRUE)),by=c("Size_Group","Growth_Group")][order(Size_Group)]
write.table(dt.owners_size_buckets_Pull,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)



#### Parent Company, Owners, Individuals Count and dollars ####
dt.owner_rollup_overview = data.table(read.csv("C:/Users/hbelton/Downloads/Owner Rollup (2)_data (2).csv"))
setnames(dt.owner_rollup_overview,old=colnames(dt.owner_rollup_overview),new=c("Parent","Owner","Store","Booking_2023"))
length(unique(dt.owner_rollup_overview$Store))
dt.owner_rollup_overview[order(-Booking_2023)]
dt.owner_rollup_owners = dt.owner_rollup_overview[,list(.N,Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("Parent","Owner")]
dt.owner_rollup_parents = dt.owner_rollup_owners[,list(.N,Stores=sum(N),Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("Parent")][order(-Booking_2023)][1:10]
write.table(dt.owner_rollup_parents,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)


dt.owner_rollup_overview_overzero = dt.owner_rollup_overview[Booking_2023 > 0]
length(unique(dt.owner_rollup_overview_overzero$Store))
dt.owner_rollup_overview_S = dt.owner_rollup_overview[,list(Parent,Owner,Booking_2023=sum(Booking_2023)),by=c("Store")][order(-Booking_2023)]
dt.owner_rollup_overview_S = dt.owner_rollup_overview_S[!duplicated(Owner)]
dt.owner_rollup_overview_S = dt.owner_rollup_overview_S[1:10]

write.table(dt.owner_rollup_overview_S,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)


#### Cut by Channel, Owners Per Channel ####
dt.owner_rollup_channel = data.table(read.csv("C:/Users/hbelton/Downloads/Owner Rollup (2)_data (1).csv"))
setnames(dt.owner_rollup_channel,old=colnames(dt.owner_rollup_channel),new=c("Channel","SAP_Group_1","SAP_Group_4","Owner","Store","Booking_2023"))

dt.by_channel = dt.owner_rollup_channel[,list(Booking_2023=sum(Booking_2023,na.rm=TRUE)),by=c("Channel","Store")]
dt.by_channel = dt.by_channel[,list(.N,Booking_2023=sum(Booking_2023,na.rm=TRUE)),by=c("Channel")]
write.table(dt.by_channel,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)
dt.owner_rollup_channel = dt.owner_rollup_channel[order(-Booking_2023)]
dt.other = dt.owner_rollup_channel[Channel == "Other",list(Booking_2023=sum(Booking_2023)),by=c("Store")][1:3]
write.table(dt.other,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

dt.channel_SAP1 = dt.owner_rollup_channel[,list(Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("Channel","SAP_Group_1")]
write.table(dt.channel_SAP1,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)
dt.SAP_names = dt.owner_rollup_channel[,list(Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("SAP_Group_1","Store")]
dt.SAP_names_ID = dt.SAP_names[SAP_Group_1 == "Independent Dealer"][1:3]
dt.SAP_names_CD = dt.SAP_names[SAP_Group_1 == "Core Dealers"][1:3]
dt.SAP_names_SP = dt.SAP_names[SAP_Group_1 == "Strategic Partnershp"][1:3]
dt.SAP_names_BB = dt.SAP_names[SAP_Group_1 == "Budget Blinds"][1:3]
dt.SAP_names_Int = dt.SAP_names[SAP_Group_1 == "Internet"][1:3]
dt.SAP_names_Oth = dt.SAP_names[SAP_Group_1 == "Other"][1:3]

dt.SAP_names_output = rbind(dt.SAP_names_ID,dt.SAP_names_CD,dt.SAP_names_SP,dt.SAP_names_BB,dt.SAP_names_Int,dt.SAP_names_Oth)
write.table(dt.SAP_names_output,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

dt.CBG_HD_Dealer_Margin = data.table(read.csv("C:/Users/hbelton/Downloads/CBG vs HD Margin Check_data (1).csv"))
setnames(dt.CBG_HD_Dealer_Margin,old=colnames(dt.CBG_HD_Dealer_Margin),new=c("Brand","Store_Name","Measure_Name","Measure_Values","Booking_2023","MSRP_23"))

dt.CBG_HD_Dealer_Margin = dcast(dt.CBG_HD_Dealer_Margin, Brand + Store_Name ~ Measure_Name, value.var = "Measure_Values")
setnames(dt.CBG_HD_Dealer_Margin,old=colnames(dt.CBG_HD_Dealer_Margin),new=c("Brand","Store_Name","Booking_2023","Client_Ext_Amt","Std_Cost","MSRP_23"))
dt.CBG_HD_Dealer_Margin[Booking_2023<0, Booking_2023 := 0]
dt.CBG_HD_Dealer_Margin[,Margin := ((Client_Ext_Amt - Booking_2023)/Client_Ext_Amt)]
dt.CBG_Dealer_Margin = dt.CBG_HD_Dealer_Margin[Brand == "CBG",list(Booking_2023=sum(Booking_2023,na.rm = TRUE),Client_Ext_Amt=sum(Client_Ext_Amt,na.rm = TRUE),Std_Cost=sum(Std_Cost,na.rm = TRUE),MSRP_23=sum(MSRP_23,na.rm = TRUE))]
dt.HD_Dealer_Margin = dt.CBG_HD_Dealer_Margin[Brand == "HD Brand",list(Booking_2023=sum(Booking_2023,na.rm = TRUE),Client_Ext_Amt=sum(Client_Ext_Amt,na.rm = TRUE),Std_Cost=sum(Std_Cost,na.rm = TRUE),MSRP_23=sum(MSRP_23,na.rm = TRUE))]

dt.HD_CBG_Total_Margin = rbind(dt.CBG_Dealer_Margin[,Brand:= "CBG"],dt.HD_Dealer_Margin[,Brand := "HD Brand"])
dt.HD_CBG_Total_Margin[,Margin := ((Client_Ext_Amt - Booking_2023)/Client_Ext_Amt)]
write.table(dt.HD_CBG_Total_Margin,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)


#### Cuts by Customer Attributes, Customer Groups, Product, Channel ####
dt.CBG_HD_All = data.table(read.csv("C:/Users/hbelton/Downloads/Company x All Fields x Store_data (1).csv"))
setnames(dt.CBG_HD_All,old=colnames(dt.CBG_HD_All),new=c("Company","Channel","Customer_Group","Product_3","Customer_Attribute","Owner","Store_Name","Booking_2023"))
sum(dt.CBG_HD_All$Booking_2023,na.rm = TRUE)
dt.CBG_HD_All[Company == "HD Brand",sum(Booking_2023,na.rm = TRUE)]
dt.HD_CG_Product = dt.CBG_HD_All[Company == "HD Brand",list(Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("Customer_Group","Product_3")]
write.table(dt.HD_CG_Product,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)


dt.CAG_4 = dt.CBG_HD_All[Company == "CBG",list(Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("Product_3")]
write.table(dt.CAG_4,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

dt.Product = dt.CBG_HD_All[,list(Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("Product_3")]
write.table(dt.Product,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

#### Table Summary of different stores / classifications ####
dt.table_classifications = data.table(read.csv("C:/Users/hbelton/Downloads/Table Full_data (3).csv"))
setnames(dt.table_classifications,old=colnames(dt.table_classifications),new=c("Company","Exclusivity","Channel","Store_Type","Store_Name","Booking_2023"))

dt.duplicate_names = dt.table_classifications[,list(Company,Store_Name)]
dt.duplicate_names_CBG = dt.duplicate_names[Company == "CBG"]
dt.duplicate_names_CBG = dt.duplicate_names_CBG[!duplicated(Store_Name)]
dt.duplicate_names_HD = dt.duplicate_names[Company == "HD Brand"]
dt.duplicate_names_HD = dt.duplicate_names_HD[!duplicated(Store_Name)]
dt.store_names = rbind(dt.duplicate_names_CBG,dt.duplicate_names_HD)
dt.store_names[, Stores := duplicated(Store_Name) | duplicated(Store_Name,fromLast = TRUE)]
dt.store_names[Stores == TRUE, Stores_2 := "Both"] 
dt.store_names[Stores == FALSE, Stores_2 := Company] 
dt.store_names = dt.store_names[,list(Stores = Stores_2,Store_Name,Company)]
dt.table_classifications = merge(dt.table_classifications[,list(Exclusivity,Channel,Store_Type,Store_Name,Booking_2023,Company)],dt.store_names[,list(Company,Store_Name,Stores)],by=c("Store_Name","Company"),all = TRUE)
dt.table_classifications[,N := 1]
dt.table_classifications[Booking_2023 > 0,Over_zero := 1]
dt.table_classifications[Booking_2023 > 36000,Over_thirtysix := 1]
dt.table_classifications[Store_Type != "Independent WC", Store_Type := "Other"]
dt.table_classifications[Company == "CBG",Exclusivity := "Non-Exclusive"]
dt.table_classifications_output = dt.table_classifications[,list(N=sum(N),Booking_2023=sum(Booking_2023,na.rm = TRUE),Over_zero=sum(Over_zero,na.rm = TRUE),Over_thirtysix=sum(Over_thirtysix,na.rm = TRUE)),by=c("Stores","Exclusivity","Channel","Store_Type")][order(-Booking_2023)]
write.table(dt.table_classifications_output,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)


dt.brand_summary = dt.table_classifications[,list(Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("Stores")]
write.table(dt.brand_summary,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

dt.exclusivity_summary = dt.table_classifications[,list(Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("Exclusivity")]
write.table(dt.exclusivity_summary,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

dt.channel_summary = dt.table_classifications[,list(Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("Channel")]
write.table(dt.channel_summary,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

dt.storetype_summary = dt.table_classifications[,list(Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("Store_Type")]
write.table(dt.storetype_summary,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

#### Booking Deciles ####
dt.table_classifications_deciles_HD = dt.table_classifications[Company == "HD Brand"]
dt.table_classifications_deciles_HD = dt.table_classifications_deciles_HD[,list(Booking_2023 = sum(Booking_2023)),by=c("Store_Name")][,N := 1]
dt.table_classifications_deciles_HD = dt.table_classifications_deciles_HD[ , Decile_Booking := cut2(Booking_2023,g = 10)]
dt.table_classifications_deciles_HD = dt.table_classifications_deciles_HD[,list(Booking_2023=sum(Booking_2023),Stores = .N),by = list(Decile_Booking)]
dt.table_classifications_deciles_HD = dt.table_classifications_deciles_HD[,Booking_per_store := Booking_2023 / Stores][order(Decile_Booking)]
write.table(dt.table_classifications_deciles_HD,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

dt.table_classifications_deciles_CBG = dt.table_classifications[Company == "CBG"]
dt.table_classifications_deciles_CBG = dt.table_classifications_deciles_CBG[,list(Booking_2023 = sum(Booking_2023)),by=c("Store_Name")][,N := 1]
dt.table_classifications_deciles_CBG = dt.table_classifications_deciles_CBG[ , Decile_Booking := cut2(Booking_2023,g = 10)]
dt.table_classifications_deciles_CBG = dt.table_classifications_deciles_CBG[,list(Booking_2023=sum(Booking_2023),Stores = .N),by = list(Decile_Booking)]
dt.table_classifications_deciles_CBG = dt.table_classifications_deciles_CBG[,Booking_per_store := Booking_2023 / Stores][order(Decile_Booking)]
write.table(dt.table_classifications_deciles_CBG,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

#### Product Mix Breakout ####
dt.product_mix = data.table(read.csv("C:/Users/hbelton/Downloads/Company x Product Fields x Store_data (1).csv"))
setnames(dt.product_mix,old=colnames(dt.product_mix),new=c("Company","Level_1","Level_2","Store_Name","Booking_2023"))
dt.product_mix = merge(dt.product_mix,dt.store_names)
dt.product_mix_level1 = dt.product_mix[,list(.N,Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("Level_1","Stores")]
write.table(dt.product_mix_level1,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)
dt.product_mix_level2 = dt.product_mix[,list(.N,Booking_2023=sum(Booking_2023,na.rm = TRUE)),by=c("Level_2","Stores")]
write.table(dt.product_mix_level2,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)








#### Dealer x Zip 20-23 Overview ####
## 2023 ##
dt.dealer_zip_23 = data.table(read.csv("C:/Users/hbelton/Downloads/23 Overview by Dealer x Zip_data.csv"))
setnames(dt.dealer_zip_23,old=colnames(dt.dealer_zip_23),new=c("Purchase_Zip","Store_Name","Measure_Name",
                                                                 "Measure_Values","Booking_Amt","Booking_Units","Client_Ext_Amount","MSRP_23,Std_Cost"))
dt.dealer_zip_23 = dt.dealer_zip_23[,list(`Store_Name`,`Purchase_Zip`,`Measure_Name`,`Measure_Values`)]
dt.dealer_zip_23 = dcast(dt.dealer_zip_23, Store_Name + Purchase_Zip ~ Measure_Name, value.var = "Measure_Values")
setnames(dt.dealer_zip_23,old=colnames(dt.dealer_zip_23),new=c("Store_Name","Purchase_Zip","Booking_2023","Client_Ext_23","Std_Cost_23","MSRP_23"))
dt.dealer_zip_23[,Dealer_Margin_23 := ((Client_Ext_23 - Booking_2023))/Client_Ext_23]
dt.dealer_zip_23[,HD_Margin_Dollars_23 := (Booking_2023 - Std_Cost_23)]
dt.dealer_zip_23[,HD_Margin_Perc_23 := (HD_Margin_Dollars_23 / Booking_2023)]

## 2022 ##
dt.dealer_zip_22 = data.table(read.csv("C:/Users/hbelton/Downloads/22 Overview by Dealer x Zip_data.csv"))
setnames(dt.dealer_zip_22,old=colnames(dt.dealer_zip_22),new=c("Purchase_Zip","Store_Name","Measure_Name",
                                                               "Measure_Values","Booking_Amt","Booking_Units","Client_Ext_Amount","MSRP_22,Std_Cost"))
dt.dealer_zip_22 = dt.dealer_zip_22[,list(`Store_Name`,`Purchase_Zip`,`Measure_Name`,`Measure_Values`)]
dt.dealer_zip_22 = dcast(dt.dealer_zip_22, Store_Name + Purchase_Zip ~ Measure_Name, value.var = "Measure_Values")
setnames(dt.dealer_zip_22,old=colnames(dt.dealer_zip_22),new=c("Store_Name","Purchase_Zip","Booking_2022","Client_Ext_22","Std_Cost_22","MSRP_22"))
dt.dealer_zip_22[,Dealer_Margin_22 := ((Client_Ext_22 - Booking_2022))/Client_Ext_22]
dt.dealer_zip_22[,HD_Margin_Dollars_22 := (Booking_2022 - Std_Cost_22)]
dt.dealer_zip_22[,HD_Margin_Perc_22 := (HD_Margin_Dollars_22 / Booking_2022)]

## 2021 ##
dt.dealer_zip_21 = data.table(read.csv("C:/Users/hbelton/Downloads/21 Overview by Dealer x Zip_data.csv"))
setnames(dt.dealer_zip_21,old=colnames(dt.dealer_zip_21),new=c("Purchase_Zip","Store_Name","Measure_Name",
                                                               "Measure_Values","Booking_Amt","Booking_Units","Client_Ext_Amount","MSRP_21,Std_Cost"))
dt.dealer_zip_21 = dt.dealer_zip_21[,list(`Store_Name`,`Purchase_Zip`,`Measure_Name`,`Measure_Values`)]
dt.dealer_zip_21 = dcast(dt.dealer_zip_21, Store_Name + Purchase_Zip ~ Measure_Name, value.var = "Measure_Values")
setnames(dt.dealer_zip_21,old=colnames(dt.dealer_zip_21),new=c("Store_Name","Purchase_Zip","Booking_2021","Client_Ext_21","Std_Cost_21","MSRP_21"))
dt.dealer_zip_21[,Dealer_Margin_21 := ((Client_Ext_21 - Booking_2021))/Client_Ext_21]
dt.dealer_zip_21[,HD_Margin_Dollars_21 := (Booking_2021 - Std_Cost_21)]
dt.dealer_zip_21[,HD_Margin_Perc_21 := (HD_Margin_Dollars_21 / Booking_2021)]


## 2020 ##
dt.dealer_zip_20 = data.table(read.csv("C:/Users/hbelton/Downloads/20 Overview by Dealer x Zip_data.csv"))
setnames(dt.dealer_zip_20,old=colnames(dt.dealer_zip_20),new=c("Purchase_Zip","Store_Name","Measure_Name",
                                                               "Measure_Values","Booking_Amt","Booking_Units","Client_Ext_Amount","MSRP_20,Std_Cost"))
dt.dealer_zip_20 = dt.dealer_zip_20[,list(`Store_Name`,`Purchase_Zip`,`Measure_Name`,`Measure_Values`)]
dt.dealer_zip_20 = dcast(dt.dealer_zip_20, Store_Name + Purchase_Zip ~ Measure_Name, value.var = "Measure_Values")
setnames(dt.dealer_zip_20,old=colnames(dt.dealer_zip_20),new=c("Store_Name","Purchase_Zip","Booking_2020","Client_Ext_20","Std_Cost_20","MSRP_20"))
dt.dealer_zip_20[,Dealer_Margin_20 := ((Client_Ext_20 - Booking_2020))/Client_Ext_20]
dt.dealer_zip_20[,HD_Margin_Dollars_20 := (Booking_2020 - Std_Cost_20)]
dt.dealer_zip_20[,HD_Margin_Perc_20 := (HD_Margin_Dollars_20 / Booking_2020)]

dt.dealer_zip_allyears = merge(dt.dealer_zip_23,dt.dealer_zip_22,by=c("Purchase_Zip","Store_Name"),all = TRUE)
dt.dealer_zip_allyears = merge(dt.dealer_zip_allyears,dt.dealer_zip_21,by=c("Purchase_Zip","Store_Name"),all = TRUE)
dt.dealer_zip_allyears = merge(dt.dealer_zip_allyears,dt.dealer_zip_20,by=c("Purchase_Zip","Store_Name"),all = TRUE)

write.csv(dt.dealer_zip_allyears,"C:/Users/hbelton/OneDrive - Mars & Co Consulting LLC/Documents/Hunter Douglas/Tableau Overall Metrics/Dealer x Zip Overview 20-23.csv")



