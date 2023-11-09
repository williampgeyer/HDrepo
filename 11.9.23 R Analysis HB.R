library(Hmisc)
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)

dt.dealers_attributes = data.table(read.csv("C:/Users/hbelton/Downloads/bookings_by_dealer_w_attributes_v2.csv"))
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


#### Ownership Count ####
## HD
dt.dealers_attributes_HD_ownership = dt.dealers_attributes[year == "2023_TTM" & brand == "HD Brand"]
dt.dealers_attributes_HD_ownership_bydealer = dt.dealers_attributes_HD_ownership[,list(bookingamt = sum(bookingamt,na.rm = TRUE),stdcost=sum(stdcost,na.rm = TRUE),
                                  msrp=sum(msrp,na.rm = TRUE),customer_id1),by=customer_id3][!duplicated(customer_id3)]
dt.dealers_attributes_HD_byowner = dt.dealers_attributes_HD_ownership_bydealer[,list(bookingamt=sum(bookingamt,na.rm = TRUE),stdcost=sum(stdcost,na.rm = TRUE),
                                  msrp=sum(msrp,na.rm = TRUE),.N),by="customer_id1"][order(N)]
intervals <- c(0,1,100,2112)
dt.dealer_avgcost_ownergroups_HD = dt.dealers_attributes_HD_byowner[,Ownership_Groups := cut(N, breaks = intervals, labels = c("One Store", "Two to 100 Stores", "Over 100 Stores"))]
dt.dealer_avgcost_ownergroups_HD = dt.dealers_attributes_HD_byowner[,list(bookingamt=sum(bookingamt,na.rm = TRUE),N=sum(N),Owners=.N),by=Ownership_Groups]

## CBG
dt.dealers_attributes_CBG_ownership = dt.dealers_attributes[year == "2023_TTM" & brand == "CBG"]
dt.dealers_attributes_CBG_ownership_bydealer = dt.dealers_attributes_CBG_ownership[,list(bookingamt = sum(bookingamt,na.rm = TRUE),stdcost=sum(stdcost,na.rm = TRUE),
                                  msrp=sum(msrp,na.rm = TRUE),customer_id1),by=customer_id3][!duplicated(customer_id3)]
dt.dealers_attributes_CBG_byowner = dt.dealers_attributes_CBG_ownership_bydealer[,list(bookingamt=sum(bookingamt,na.rm = TRUE),stdcost=sum(stdcost,na.rm = TRUE),
                                  msrp=sum(msrp,na.rm = TRUE),.N),by="customer_id1"][order(N)]
intervals <- c(0,1,100,2112)
dt.dealer_avgcost_ownergroups_CBG = dt.dealers_attributes_CBG_byowner[,Ownership_Groups := cut(N, breaks = intervals, labels = c("One Store", "Two to 100 Stores", "Over 100 Stores"))]
dt.dealer_avgcost_ownergroups_CBG = dt.dealers_attributes_CBG_byowner[,list(bookingamt=sum(bookingamt,na.rm = TRUE),N=sum(N),Owners=.N),by=Ownership_Groups][order(-bookingamt)]

write.table(dt.dealer_avgcost_ownergroups_CBG,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)


## Over 36K
## HD
dt.dealers_attributes_HD_ownership_bydealer_36 = dt.dealers_attributes_HD_ownership_bydealer[bookingamt > 36000]
dt.dealers_attributes_HD_byowner_36 = dt.dealers_attributes_HD_ownership_bydealer_36[,list(bookingamt=sum(bookingamt,na.rm = TRUE),stdcost=sum(stdcost,na.rm = TRUE),
                                 msrp=sum(msrp,na.rm = TRUE),.N),by="customer_id1"][order(-N)]
intervals <- c(0,1,25,50,100,200000000)
dt.dealer_avgcost_ownergroups_HD_36 = dt.dealers_attributes_HD_byowner_36[,Ownership_Groups := cut(N, breaks = intervals, labels = c("One Store", "Two to 25 Stores", "25 to 50 Stores", "50 to 100 Stores", "Over 100 Stores"))]
dt.dealer_avgcost_ownergroups_HD_36 = dt.dealer_avgcost_ownergroups_HD_36[,list(bookingamt=sum(bookingamt,na.rm = TRUE),N=sum(N),Owners=.N),by=Ownership_Groups]

## CBG
dt.dealers_attributes_CBG_ownership_bydealer_36 = dt.dealers_attributes_CBG_ownership_bydealer[bookingamt > 36000]
dt.dealers_attributes_CBG_byowner_36 = dt.dealers_attributes_CBG_ownership_bydealer_36[,list(bookingamt=sum(bookingamt,na.rm = TRUE),stdcost=sum(stdcost,na.rm = TRUE),
                                                                                           msrp=sum(msrp,na.rm = TRUE),.N),by="customer_id1"][order(N)]
intervals <- c(0,1,25,50,100,200000000)
dt.dealer_avgcost_ownergroups_CBG_36 = dt.dealers_attributes_CBG_byowner_36[,Ownership_Groups := cut(N, breaks = intervals, labels = c("One Store", "Two to 25 Stores", "25 to 50 Stores", "50 to 100 Stores", "Over 100 Stores"))]
dt.dealer_avgcost_ownergroups_CBG_36 = dt.dealer_avgcost_ownergroups_CBG_36[,list(bookingamt=sum(bookingamt,na.rm = TRUE),N=sum(N),Owners=.N),by=Ownership_Groups]

write.table(dt.dealer_avgcost_ownergroups_CBG_36,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)


#### Cost Factor at Product Level 5 ####
dt.cf_product_level5 = data.table(read.csv("C:/Users/hbelton/Downloads/Cost Factor x Product Lvl 5 (2)_data.csv"))
setnames(dt.cf_product_level5,old=colnames(dt.cf_product_level5),new=c("Brand","Store_ID","Product_Level_5","Booking_2023","MSRP_2023"))
## HD
dt.cf_product_CF_HD = dt.cf_product_level5[Brand == "HD Brand",list(Booking_2023=sum(Booking_2023,na.rm = TRUE),MSRP_2023=sum(MSRP_2023,na.rm = TRUE)),by=Product_Level_5]
dt.cf_product_CF_HD = dt.cf_product_CF_HD[, CF_Product := Booking_2023 / MSRP_2023][order(-Booking_2023)][1:10]
dt.cf_product_level5_HD = dt.cf_product_level5[Brand == "HD Brand",list(Booking_2023,MSRP_2023),by=c("Store_ID","Product_Level_5")][,CF := (Booking_2023 / MSRP_2023)]
dt.cf_product_index_HD = merge(dt.cf_product_level5_HD,dt.cf_product_CF_HD[,list(Product_Level_5,CF_Product)],by="Product_Level_5",all = TRUE)
dt.cf_product_index_HD = dt.cf_product_index_HD[Booking_2023 > 0 & MSRP_2023 > 0]
dt.cf_product_index_HD[,CF_Index := CF / CF_Product]
dt.cf_product_index_HD = dt.cf_product_index_HD[complete.cases(CF_Product)]
dt.cf_product_index_HD = dt.cf_product_index_HD[complete.cases(CF_Index)]
dt.cf_product_index_HD = dt.cf_product_index_HD[CF_Index < 2]

dt.cf_product_index_HD = dt.cf_product_index_HD[!(Product_Level_5 == "Other/Not a Finished Product")]
dt.cf_product_index_HD = dt.cf_product_index_HD[Booking_2023 > 5000]
ggplot(dt.cf_product_index_HD, aes(x = Product_Level_5, y = CF)) + geom_boxplot() + theme_bw(base_size = 16) + coord_cartesian(ylim = c(0.15,0.55))
write.table(dt.cf_product_CF_HD,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

## CBG
dt.cf_product_CF_CBG = dt.cf_product_level5[Brand == "CBG",list(Booking_2023=sum(Booking_2023,na.rm = TRUE),MSRP_2023=sum(MSRP_2023,na.rm = TRUE)),by=Product_Level_5]
dt.cf_product_CF_CBG = dt.cf_product_CF_CBG[, CF_Product := Booking_2023 / MSRP_2023][order(-Booking_2023)][1:10]
dt.cf_product_level5_CBG = dt.cf_product_level5[Brand == "CBG",list(Booking_2023,MSRP_2023),by=c("Store_ID","Product_Level_5")][,CF := (Booking_2023 / MSRP_2023)]
dt.cf_product_index_CBG = merge(dt.cf_product_level5_CBG,dt.cf_product_CF_CBG[,list(Product_Level_5,CF_Product)],by="Product_Level_5",all = TRUE)
dt.cf_product_index_CBG = dt.cf_product_index_CBG[Booking_2023 > 0 & MSRP_2023 > 0]
dt.cf_product_index_CBG[,CF_Index := CF / CF_Product]
dt.cf_product_index_CBG = dt.cf_product_index_CBG[complete.cases(CF_Product)]
dt.cf_product_index_CBG = dt.cf_product_index_CBG[complete.cases(CF_Index)]
dt.cf_product_index_CBG = dt.cf_product_index_CBG[CF_Index < 2]

dt.cf_product_index_CBG = dt.cf_product_index_CBG[!(Product_Level_5 == "Trade Brands Vinyl Shutters")]
dt.cf_product_index_CBG = dt.cf_product_index_CBG[Booking_2023 > 5000]
ggplot(dt.cf_product_index_CBG, aes(x = Product_Level_5, y = CF)) + geom_boxplot() + theme_bw(base_size = 16) + coord_cartesian(ylim = c(0.15,0.6))
write.table(dt.cf_product_CF_CBG,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

#### Deciles > $36K ####
dt.dealers_attributes_deciles = dt.dealers_attributes[year == "2023_TTM",list(storename,bookingamt=sum(bookingamt),purchasezip),by=c("customer_id3","brand")]
dt.dealers_attributes_deciles_HD = dt.dealers_attributes_deciles[brand == "HD Brand"]
dt.dealers_attributes_deciles_HD = dt.dealers_attributes_deciles_HD[bookingamt > 36000]
dt.dealers_attributes_deciles_HD = dt.dealers_attributes_deciles_HD[!duplicated(customer_id3)][,N := 1]
dt.dealer_deciles_HD = dt.dealers_attributes_deciles_HD[ , Decile_Booking := cut2(bookingamt,g = 10)]
dt.dealer_deciles_HD_grouping = dt.dealer_deciles_HD[,list(bookingamt=sum(bookingamt),Stores = .N),by = list(Decile_Booking)]
dt.dealer_deciles_HD_grouping = dt.dealer_deciles_HD_grouping[,Booking_per_store := bookingamt / Stores][order(Decile_Booking)]
dt.dealer_deciles_HD_top5 = dt.dealer_deciles_HD[order(-bookingamt)][1:5]
dt.dealer_deciles_HD_top5 = merge(dt.dealer_deciles_HD_top5[,list(customer_id3,storename,bookingamt)],dt.dealers_attributes[brand == "HD Brand",list(customer_id3,purchasezip,exclusivity,alliance,channel,customer_attribute)],by="customer_id3")
write.table(dt.dealer_deciles_HD_top5,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)


dt.dealers_attributes_deciles = dt.dealers_attributes[year == "2023_TTM",list(storename,bookingamt=sum(bookingamt),purchasezip),by=c("customer_id3","brand")][!duplicated(customer_id3)]
dt.dealers_attributes_deciles_CBG = dt.dealers_attributes_deciles[brand == "CBG"]
dt.dealers_attributes_deciles_CBG = dt.dealers_attributes_deciles_CBG[bookingamt > 36000]
dt.dealers_attributes_deciles_CBG = dt.dealers_attributes_deciles_CBG[!duplicated(customer_id3)][,N := 1]
dt.dealer_deciles_CBG = dt.dealers_attributes_deciles_CBG[ , Decile_Booking := cut2(bookingamt,g = 10)]
dt.dealer_deciles_CBG_grouping = dt.dealer_deciles_CBG[,list(bookingamt=sum(bookingamt),Stores = .N),by = list(Decile_Booking)]
dt.dealer_deciles_CBG_grouping = dt.dealer_deciles_CBG_grouping[,Booking_per_store := bookingamt / Stores][order(Decile_Booking)]
write.table(dt.dealer_deciles_CBG_grouping,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)
dt.dealer_deciles_CBG_top5 = dt.dealer_deciles_CBG[order(-bookingamt)][1:5]
dt.dealer_deciles_CBG_top5 = merge(dt.dealer_deciles_CBG_top5[,list(customer_id3,storename,bookingamt)],dt.dealers_attributes[brand == "CBG",list(customer_id3,purchasezip,exclusivity,alliance,channel,customer_attribute)],by="customer_id3")
dt.dealer_deciles_CBG_top5 = dt.dealer_deciles_CBG_top5[order(-bookingamt)]
write.table(dt.dealer_deciles_CBG_top5,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

## Under 36K
dt.dealers_attributes_deciles_under36_HD = dt.dealers_attributes_deciles[brand == "HD Brand"][bookingamt < 36000][!duplicated(customer_id3)][,N := 1][order(customer_id3)][,list(bookingamt=sum(bookingamt),N=sum(N))][,brand := "HD Brand"]
dt.dealers_attributes_deciles_under36_CBG = dt.dealers_attributes_deciles[brand == "CBG"][bookingamt < 36000][!duplicated(customer_id3)][,N := 1][order(customer_id3)][,list(bookingamt=sum(bookingamt),N=sum(N))][,brand := "CBG"]
dt.dealers_attributes_deciles_under36 = rbind(dt.dealers_attributes_deciles_under36_HD,dt.dealers_attributes_deciles_under36_CBG)
write.table(dt.dealers_attributes_deciles_under36,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)


#### Dealer Attributes ####
dt.attributes_breakdown = dt.dealers_attributes[year == "2023_TTM"]
dt.duplicate_names = dt.attributes_breakdown[,list(brand,storename)]
dt.duplicate_names_CBG = dt.duplicate_names[brand == "CBG"]
dt.duplicate_names_CBG = dt.duplicate_names_CBG[!duplicated(storename)]
dt.duplicate_names_HD = dt.duplicate_names[brand == "HD Brand"]
dt.duplicate_names_HD = dt.duplicate_names_HD[!duplicated(storename)]
dt.store_names = rbind(dt.duplicate_names_CBG,dt.duplicate_names_HD)
dt.store_names[, Stores := duplicated(storename) | duplicated(storename,fromLast = TRUE)]
dt.store_names[Stores == TRUE, Stores_2 := "Both"] 
dt.store_names[Stores == FALSE, Stores_2 := brand] 
dt.store_names = dt.store_names[,list(Stores = Stores_2,storename,brand)]
dt.table_classifications = merge(dt.attributes_breakdown[,list(exclusivity,channel,customer_attribute,alliance,storename,bookingamt,brand)],dt.store_names[,list(brand,storename,Stores)],by=c("storename","brand"),all = TRUE)
dt.table_classifications[,N := 1]
dt.table_classifications[bookingamt > 0,Over_zero := 1]
dt.table_classifications[bookingamt > 36000,Over_thirtysix := 1]
dt.brand = dt.table_classifications[,list(bookingamt=sum(bookingamt,na.rm = TRUE)),by=c("Stores")][order(-bookingamt)]
dt.exclusivity = dt.table_classifications[,list(bookingamt=sum(bookingamt,na.rm = TRUE)),by=c("exclusivity")][order(-bookingamt)]
dt.channel = dt.table_classifications[,list(bookingamt=sum(bookingamt,na.rm = TRUE)),by=c("channel")][order(-bookingamt)]
dt.attribute = dt.table_classifications[,list(bookingamt=sum(bookingamt,na.rm = TRUE)),by=c("customer_attribute")][order(-bookingamt)]
dt.alliance = dt.table_classifications[,list(bookingamt=sum(bookingamt,na.rm = TRUE)),by=c("alliance","brand")][order(-bookingamt)]

write.table(dt.alliance,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

## Old numbers
dt.dealers_attributes_og = data.table(read.csv("C:/Users/hbelton/Downloads/bookings_by_dealer_w_attributes.csv"))
dt.attributes_breakdown = dt.dealers_attributes_og[year == "2023_TTM"]
dt.duplicate_names = dt.attributes_breakdown[,list(brand,storename)]
dt.duplicate_names_CBG = dt.duplicate_names[brand == "CBG"]
dt.duplicate_names_CBG = dt.duplicate_names_CBG[!duplicated(storename)]
dt.duplicate_names_HD = dt.duplicate_names[brand == "HD Brand"]
dt.duplicate_names_HD = dt.duplicate_names_HD[!duplicated(storename)]
dt.store_names = rbind(dt.duplicate_names_CBG,dt.duplicate_names_HD)
dt.store_names[, Stores := duplicated(storename) | duplicated(storename,fromLast = TRUE)]
dt.store_names[Stores == TRUE, Stores_2 := "Both"] 
dt.store_names[Stores == FALSE, Stores_2 := brand] 
dt.store_names = dt.store_names[,list(Stores = Stores_2,storename,brand)]
dt.table_classifications = merge(dt.attributes_breakdown[,list(exclusivity,channel,customer_attribute,alliance,storename,bookingamt,brand)],dt.store_names[,list(brand,storename,Stores)],by=c("storename","brand"),all = TRUE)
dt.table_classifications[,N := 1]
dt.table_classifications[bookingamt > 0,Over_zero := 1]
dt.table_classifications[bookingamt > 36000,Over_thirtysix := 1]
dt.brand = dt.table_classifications[,list(bookingamt=sum(bookingamt,na.rm = TRUE)),by=c("Stores")][order(-bookingamt)]
dt.exclusivity = dt.table_classifications[,list(bookingamt=sum(bookingamt,na.rm = TRUE)),by=c("exclusivity")][order(-bookingamt)]
dt.channel = dt.table_classifications[,list(bookingamt=sum(bookingamt,na.rm = TRUE)),by=c("channel")][order(-bookingamt)]
dt.attribute = dt.table_classifications[,list(bookingamt=sum(bookingamt,na.rm = TRUE)),by=c("customer_attribute")][order(-bookingamt)]
dt.alliance = dt.table_classifications[,list(bookingamt=sum(bookingamt,na.rm = TRUE)),by=c("alliance","brand")][order(-bookingamt)]
write.table(dt.alliance,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)



#### Growth Buckets ####
dt.dealers_attributes_growth_all = dt.dealers_attributes[,list(year,storename,purchasezip,exclusivity,alliance,customer_attribute,channel,brand,bookingamt),by="customer_id3"]
dt.dealers_attributes_growth_withchannel = dt.dealers_attributes_growth_all[year %in% c("2023_TTM","2022_PTM"),list(year,storename,channel,bookingamt),by=c("customer_id3","brand")]
dt.dealers_attributes_growth_withchannel = dcast(dt.dealers_attributes_growth_withchannel, customer_id3 + brand + storename + channel ~ year, fun.aggregate = sum, value.var = "bookingamt")
setnames(dt.dealers_attributes_growth_withchannel,old=colnames(dt.dealers_attributes_growth_withchannel),new=c("customer_id3","brand","storename","channel","booking_PTM","booking_TTM"))
dt.dealers_attributes_growth_withchannel[is.na(booking_PTM), booking_PTM := 0]
dt.dealers_attributes_growth_withchannel[is.na(booking_TTM), booking_TTM := 0]

## without channel
dt.dealers_attributes_growth = dt.dealers_attributes_growth_all[year %in% c("2023_TTM","2022_PTM"),list(year,bookingamt,purchasezip),by=c("customer_id3","brand")]
dt.dealers_attributes_growth = dcast(dt.dealers_attributes_growth, customer_id3 + brand + purchasezip ~ year, fun.aggregate = sum, value.var = "bookingamt")
setnames(dt.dealers_attributes_growth,old=colnames(dt.dealers_attributes_growth),new=c("customer_id3","brand","purchasezip","booking_PTM","booking_TTM"))
dt.dealers_attributes_growth = dt.dealers_attributes_growth[is.na(booking_PTM), booking_PTM := 0]
dt.dealers_attributes_growth = dt.dealers_attributes_growth[is.na(booking_TTM), booking_TTM := 0]

## Remove dealers who have under 36K in PTM booking $s
dt.dealers_attributes_growth_over36 = dt.dealers_attributes_growth[,list(booking_TTM=sum(booking_TTM),booking_PTM=sum(booking_PTM)),by=c("customer_id3","purchasezip")][booking_PTM > 36000][booking_TTM < 0,booking_TTM := 0][booking_PTM < 0,booking_PTM := 0]
dt.dealers_growth_bychannel_over36 = dt.dealers_attributes_growth_over36[,Growth := (booking_TTM / booking_PTM) - 1][order(-Growth)]

## Characteristics 
dt.dealers_characteristics = data.table(read.csv("C:/Users/hbelton/Downloads/poe_v3.csv"))
dt.dealers_characteristics = dt.dealers_characteristics[,HD_dealers_density := HD_dealers_all / Households * 100000][,HD_program_dealers_perc := HD_program_dealers / Households]
dt.dealers_growth_full = merge(dt.dealers_growth_bychannel_over36,dt.dealers_characteristics,by="purchasezip",all = TRUE)
dt.dealers_growth_full = dt.dealers_growth_full[!is.na(customer_id3)]
dt.dealers_growth_full = dt.dealers_growth_full[!is.na(purchasezip)]

## Overall - Both Brands w characteristics
dt.dealer_characteristics_growth_output = dt.dealers_growth_full[,list(N_dealers = .N,booking_TTM = sum(booking_TTM,na.rm = TRUE),Households=sum(Households,na.rm = TRUE),
                           avg_dealer=mean(booking_TTM,na.rm = TRUE),dealer_density=mean.weighted(HD_dealers_density,booking_TTM,na.rm = TRUE),
                           comp_dealers=mean.weighted(comp_density,booking_TTM,na.rm = TRUE),
                           program_dealers=mean(HD_program_dealers/Households*100000,na.rm = TRUE),HH_greater_100k=sum(HH_greater_100k,na.rm = TRUE),
                           permits=sum(Building_Permits,na.rm = TRUE),HH_Inc=median(Median_census_home_value,na.rm = TRUE),
                           Program_Dealers_perc = sum(HD_program_dealers,na.rm = TRUE) / sum(HD_dealers_all,na.rm = TRUE)),
                           by=list(Growth_Group=cut2(Growth,cuts=c(-.50,-.2,-.1,0,.1,.2,.5)))][order(Growth_Group)]
dt.urbanization_perc = dt.dealers_growth_full[,list(N_dealers = .N,booking_TTM = sum(booking_TTM,na.rm = TRUE)),by=list(Growth_Group=cut2(Growth,cuts=c(-.50,-.2,-.1,0,.1,.2,.5)),Grouping)][order(Growth_Group)]
write.table(dt.dealer_characteristics_growth_output,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

## Brand Count
dt.dealers_brand_growth_over36 = dt.dealers_attributes_growth[,list(booking_TTM=sum(booking_TTM),booking_PTM=sum(booking_PTM)),by=c("customer_id3","brand")][booking_PTM > 36000][booking_TTM < 0,booking_TTM := 0][booking_PTM < 0,booking_PTM := 0][,Growth := (booking_TTM / booking_PTM) - 1][order(-Growth)]
dt.dealers_brand_growth_over36_output = dt.dealers_brand_growth_over36[,list(N_dealers = .N,booking_TTM = sum(booking_TTM,na.rm = TRUE)),by=list(Growth_Group=cut2(Growth,cuts=c(-.50,-.2,-.1,0,.1,.2,.5)),brand)][order(Growth_Group)]

write.table(dt.dealers_brand_growth_over36_output,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)


#### Product Mix ####
dt.product_mix = data.table(read.csv("C:/Users/hbelton/Downloads/productmix_channel_data (2).csv"))
setnames(dt.product_mix,old=colnames(dt.product_mix),new=c("Brand","Product","Channel","Booking_2023","Stdcost_23"))
dt.product_mix_HD = dt.product_mix[Brand == "HD Brand",list(Booking_2023=sum(Booking_2023),Stdcost_23=sum(Stdcost_23)),by=c("Product","Channel")]
dt.product_mix_CBG = dt.product_mix[Brand == "CBG",list(Booking_2023=sum(Booking_2023),Stdcost_23=sum(Stdcost_23)),by=c("Product","Channel")]
write.table(dt.product_mix_CBG,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)
