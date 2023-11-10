
# Chapter 1: External Data Unification ------------------------------------
# Libraries and Functions -------------------------------------------------
{
  library(data.table)
  library(ggplot2)
  library(dplyr)
  library(stringr)
  library(scales)
  library(zoo)
  library(ggpmisc)
  library(sf)
  library(tigris)
  library(censusapi)
  library(zctaCrosswalk)
  
  copyfromexcel <- function(header=TRUE,...) {
    read.table("clipboard",sep="\t",header=header,...)
  }
  
  copytoexcel <- function(x,row.names=FALSE,col.names=TRUE,...) {
    write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
  }
  
  fn.moconv = function(x) {
    return(as.Date(paste0(substr(x,1,4),"-",substr(x,5,6),"-01")))
  }
  
  fn.mo_to_fy = function(x) {
    xyr = as.numeric(substr(x,1,4))
    xmo = as.numeric(substr(x,5,6))
    xfy = xyr + ifelse(xmo>=9,1,0)
    return(xfy)
  }
  
  fn.ecdf = function(dist,value) {
    sapply(value,dist=dist,FUN = function(value,dist) {
      mean(value>=dist)
    })
  }
  
  fn.group_simple = function(group) {
    group_simple = gsub("group_","",group)
    group_simple = gsub("_global_acct_id","",group_simple)
    group_simple = recode(group_simple,"silver"="Silver","gold"="Gold","plat"="Platinum","fnd_plat"="Fnd Platinum","emd"="Emerald","fnd_emd"="Fnd Emerald","dia"="Diamond")
    group_simple = factor(group_simple,levels=c("Silver","Gold","Platinum","Fnd Platinum","Emerald","Fnd Emerald","Diamond"))
  }
  
  # Cut2
  
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
      if(any(c("...","digits") %in%  names(formals(args(formatfun)))))
        c(digits = digits, list(...))
    else list(...)
    
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
  
}





# Data Retrieval ----------------------------------------------------------

# set up working directory
chr.prev_wd = getwd()
setwd("C:/Users/dmcgrorey/OneDrive - Mars & Co Consulting LLC/Documents/1244 -- HD")

dt.new.home.starts.21 = data.table(read.csv("./External Data -- New Starts.csv"))
dt.new.home.starts.20 = data.table(read.csv("./2020 building permits.csv"))
dt.fips.cost.of.living = data.table(read.csv("./Cost of Living Map.csv"))
dt.zips.to.zcta = data.table(read.csv("./Zip to ZCTA.csv"))

# 2021 Census Pull ---------------------------------------------------------------

my_api_key = "96ebedb816b8efd4eab1d84a33cf12943c14251a" #Set your census API Key

variables = c("B01002_001E", "B25010_001E", "B19013_001E", "B25002_001E", "B25002_002E", "B25002_003E", "B25003_001E", "B25003_002E", "B25003_003E", "B25001_001E", 
              "B25018_001E", "B25035_001E", "B25024_002E", "B25024_001E", "B25077_001E", "B11001_001E", 
              "B07001_001E", "B07001_017E", "B07001_033E", "B07001_049E", "B07001_065E", "B07001_081E", "B25088_001E", "B01003_001E", "B11001_001E", 
              "B19001_014E", "B19001_015E", "B19001_016E", "B19001_017E", "B19001_001E") #Set variables of interest

#Variable Description
#B01002_001E -- Estimate!!Median age --!!Total: -- MEDIAN AGE BY SEX -- Includes all genders
#B25010_001E -- Estimate!!Average household size --!!Total: -- AVERAGE HOUSEHOLD SIZE OF OCCUPIED HOUSING UNITS BY TENURE -- Includes both renters and owners
#B19013_001E -- Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars) -- MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2021 INFLATION-ADJUSTED DOLLARS)
#B25002_001E -- Estimate!!Total: -- OCCUPANCY STATUS
#B25002_002E -- Estimate!!Total:!!Occupied -- OCCUPANCY STATUS
#B25002_003E -- Estimate!!Total:!!Vacant -- OCCUPANCY STATUS
#B25003_001E -- Estimate!!Total: -- TENURE
#B25003_002E -- Estimate!!Total:!!Owner occupied -- TENURE
#B25003_003E -- Estimate!!Total:!!Renter occupied -- TENURE
#B25001_001E -- Estimate!!Total -- HOUSING UNITS
#B25018_001E -- Estimate!!Median number of rooms -- MEDIAN NUMBER OF ROOMS
#B25035_001E -- Estimate!!Median year structure built -- MEDIAN YEAR STRUCTURE BUILT
#B25024_002E -- Estimate!!Total:!!1, detached -- UNITS IN STRUCTURE
#B25024_001E -- Estimate!!Total: -- UNITS IN STRUCTURE
#B25077_001E -- Estimate!!Median value (dollars) -- MEDIAN VALUE (DOLLARS)
#B07001_001E -- Estimate!!Total: -- GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY AGE FOR CURRENT RESIDENCE IN THE UNITED STATES
#B07001_017E -- Estimate!!Total:!!Same house 1 year ago: -- GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY AGE FOR CURRENT RESIDENCE IN THE UNITED STATES
#B07001_033E -- Estimate!!Total:!!Moved within same county: -- GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY AGE FOR CURRENT RESIDENCE IN THE UNITED STATES
#B07001_049E -- Estimate!!Total:!!Moved from different county within same state: -- GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY AGE FOR CURRENT RESIDENCE IN THE UNITED STATES
#B07001_065E -- Estimate!!Total:!!Moved from different state: -- GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY AGE FOR CURRENT RESIDENCE IN THE UNITED STATES
#B07001_081E -- Estimate!!Total:!!Moved from abroad:
#B25088_001E -- Estimate!!Median selected monthly owner costs (dollars) --!!Total: -- MEDIAN SELECTED MONTHLY OWNER COSTS (DOLLARS) BY MORTGAGE STATUS
#B19001_014E -- Estimate!!Total:!!$100,000 to $124,999
#B19001_015E -- Estimate!!Total:!!$125,000 to $149,999
#B19001_016E -- Estimate!!Total:!!$150,000 to $199,999
#B19001_017E -- Estimate!!Total:!!$200,000 or more
#B19001_001E -- BASE



geography = "zip code tabulation area" #Specify geography -- i.e., Zip code level

census_data_21 = getCensus(name = "acs/acs5", vintage = 2021, 
                           key = my_api_key, vars = variables, region = geography) #Output

census_data_21 = data.table(census_data_21) #Convert to data.table

census_data_21 = census_data_21[, list(ZCTA = zip_code_tabulation_area, 
                                       Median_Age_Total = B01002_001E, 
                                       Avg_Household_Size = B25010_001E, 
                                       Median_Household_Income = B19013_001E, 
                                       Occ_vs_Vac_Total = B25002_001E,
                                       Occ_vs_Vac_Occ = B25002_002E,
                                       Occ_vs_Vac_Vac = B25002_003E,
                                       Own_vs_rent_Total = B25003_001E,
                                       Own_vs_rent_own = B25003_002E,
                                       Own_vs_rent_rent = B25003_003E,
                                       Total_Housing_Units = B25001_001E, 
                                       Median_Rooms = B25018_001E, 
                                       Median_year_structure_built = B25035_001E, 
                                       Single_family_Units = B25024_002E, 
                                       Total_Units = B25024_001E, 
                                       Median_census_home_value = B25077_001E, 
                                       households = B11001_001E, 
                                       Migration_Total = B07001_001E,
                                       Migration_No_Change = B07001_017E,
                                       Migration_within_county = B07001_033E,
                                       Migration_diff_county = B07001_049E,
                                       Migration_diff_state = B07001_065E,
                                       Migration_abroad = B07001_081E, 
                                       Home_Owner_Costs_Monthly = B25088_001E*1, 
                                       Population = B01003_001E, 
                                       Households = B11001_001E, 
                                       HH_100_to_125k = B19001_014E, 
                                       HH_125_to_150k = B19001_015E, 
                                       HH_150_to_200k = B19001_016E, 
                                       HH_200_plus_k = B19001_017E, 
                                       HH_income_bracket_base = B19001_001E)] #Clean labels

census_data_21[, Single_family_units_Perc := Single_family_Units / Total_Units]

census_data_21[, Own_vs_rent_own_Perc := Own_vs_rent_own / Own_vs_rent_Total]

census_data_21[, Year := 2021]

census_data_21[, HH_greater_100k := HH_100_to_125k + HH_125_to_150k + HH_150_to_200k + HH_200_plus_k]

census_data_21[, sum(HH_greater_100k)]

# 2020 Census Pull ----------------------------------------------------------------------
census_data_20 = getCensus(name = "acs/acs5", vintage = 2020, 
                           key = my_api_key, vars = variables, region = geography) #Output

census_data_20 = data.table(census_data_20) #Convert to data.table

census_data_20 = census_data_20[, list(ZCTA = zip_code_tabulation_area, 
                                       Median_Age_Total = B01002_001E, 
                                       Avg_Household_Size = B25010_001E, 
                                       Median_Household_Income = B19013_001E, 
                                       Occ_vs_Vac_Total = B25002_001E,
                                       Occ_vs_Vac_Occ = B25002_002E,
                                       Occ_vs_Vac_Vac = B25002_003E,
                                       Own_vs_rent_Total = B25003_001E,
                                       Own_vs_rent_own = B25003_002E,
                                       Own_vs_rent_rent = B25003_003E,
                                       Total_Housing_Units = B25001_001E, 
                                       Median_Rooms = B25018_001E, 
                                       Median_year_structure_built = B25035_001E, 
                                       Single_family_Units = B25024_002E, 
                                       Total_Units = B25024_001E, 
                                       Median_census_home_value = B25077_001E, 
                                       households = B11001_001E, 
                                       Migration_Total = B07001_001E,
                                       Migration_No_Change = B07001_017E,
                                       Migration_within_county = B07001_033E,
                                       Migration_diff_county = B07001_049E,
                                       Migration_diff_state = B07001_065E,
                                       Migration_abroad = B07001_081E, 
                                       Home_Owner_Costs_Monthly = B25088_001E*1, 
                                       Population = B01003_001E, 
                                       Households = B11001_001E, 
                                       HH_100_to_125k = B19001_014E, 
                                       HH_125_to_150k = B19001_015E, 
                                       HH_150_to_200k = B19001_016E, 
                                       HH_200_plus_k = B19001_017E, 
                                       HH_income_bracket_base = B19001_001E)] #Clean labels

census_data_20[, Single_family_units_Perc := Single_family_Units / Total_Units]

census_data_20[, Own_vs_rent_own_Perc := Own_vs_rent_own / Own_vs_rent_Total]

census_data_20[, Year := 2020]

census_data_20[, HH_greater_100k := HH_100_to_125k + HH_125_to_150k + HH_150_to_200k + HH_200_plus_k]

census_data_20[, sum(HH_greater_100k)]

# 2021 and 2022 Cost of Living ----------------------------------------------------------

#Start with a map of FIPS by ZIP -- note not 1:1
dt.zips.by.fips = data.table(zcta_crosswalk)
dt.zips.by.fips = dt.zips.by.fips[, list(county_fips, zcta)][order(county_fips)]

#now pull in ZIP level home ownership cost and populations -- we are allocating off 2021 metrics
dt.zips.by.fips = merge(x = dt.zips.by.fips, y = census_data_21[, list(ZCTA, Home_Owner_Costs_Monthly, Population)], by.x = c("zcta"), by.y = c("ZCTA"))

#now pull in FIP level cost of living
dt.zips.by.fips = merge(x = dt.zips.by.fips, y = dt.fips.cost.of.living, by.x = c("county_fips"), by.y = c("FIPS"))

#Filter out irrelevant data

dt.zips.by.fips = dt.zips.by.fips[Home_Owner_Costs_Monthly > 0 & Population > 0 & Total > 0]

#Add WA housing costs

dt.zips.by.fips[, WA_housing_census := sum(Home_Owner_Costs_Monthly*Population) / sum(Population), by = list(county_fips)]

#Calculate scaling factor

dt.zips.by.fips[, Housing_scaling_factor := Home_Owner_Costs_Monthly / WA_housing_census]

#Calculate new housing costs

dt.zips.by.fips[, Housing_v2 := Housing_scaling_factor * Housing]

#Calculate new total costs

dt.zips.by.fips[, Cost_of_living := Housing_v2 + Food + Transportation + Healthcare + Other.Necessities + Childcare + Taxes]

#Now aggregate to pure zip code

dt.zips.by.fips = dt.zips.by.fips[, list(zcta, Cost_of_living)][order(zcta)]
dt.zips.by.fips = dt.zips.by.fips[, list(Cost_of_living = median(Cost_of_living)), by = list(zcta)]

# New Home Starts 2021 ------------------------------------------------------

#Start with a map of FIPS by ZIP -- note not 1:1
dt.zips.by.fips.nhs.21 = data.table(zcta_crosswalk)
dt.zips.by.fips.nhs.21 = dt.zips.by.fips.nhs.21[, list(county_fips, zcta)][order(county_fips)]

#pull in ZIP level population data
dt.zips.by.fips.nhs.21 = merge(x = dt.zips.by.fips.nhs.21, y = census_data_21[, list(ZCTA, Population)], by.x = c("zcta"), by.y = c("ZCTA"))

#now pull in FIP level permits

dt.new.home.starts.21 = dt.new.home.starts.21[, State := sprintf("%02d", State)]
dt.new.home.starts.21 = dt.new.home.starts.21[, County := sprintf("%03d", County)]
dt.new.home.starts.21 = dt.new.home.starts.21[, FIPS := paste0(State, County)]
dt.zips.by.fips.nhs.21 = merge(x = dt.zips.by.fips.nhs.21, y = dt.new.home.starts.21[, list(FIPS, units)], by.x = c("county_fips"), by.y = c("FIPS"))

dt.zips.by.fips.nhs.21 = dt.zips.by.fips.nhs.21[order(county_fips)]

#now we need to allocate by population mix

dt.zips.by.fips.nhs.21[, Pop_mix := Population / sum(Population), by = list(county_fips)]
dt.zips.by.fips.nhs.21[, Units_allocated := units * Pop_mix]

#now sum by zip to get rid of duplicates

dt.zips.by.fips.nhs.21 = dt.zips.by.fips.nhs.21[, list(Building_Permits = sum(Units_allocated, na.rm = T)), by = list(zcta)]

dt.zips.by.fips.nhs.21[, sum(Building_Permits)]

dt.zips.by.fips.nhs.21[, Year := 2021]





# New Home Starts 2020 ------------------------------------------------------

#Start with a map of FIPS by ZIP -- note not 1:1
dt.zips.by.fips.nhs.20 = data.table(zcta_crosswalk)
dt.zips.by.fips.nhs.20 = dt.zips.by.fips.nhs.20[, list(county_fips, zcta)][order(county_fips)]

#pull in ZIP level population data
dt.zips.by.fips.nhs.20 = merge(x = dt.zips.by.fips.nhs.20, y = census_data_20[, list(ZCTA, Population)], by.x = c("zcta"), by.y = c("ZCTA"))

#now pull in FIP level permits

dt.new.home.starts.20 = dt.new.home.starts.20[, State := sprintf("%02d", State)]
dt.new.home.starts.20 = dt.new.home.starts.20[, County := sprintf("%03d", County)]
dt.new.home.starts.20 = dt.new.home.starts.20[, FIPS := paste0(State, County)]
dt.zips.by.fips.nhs.20 = merge(x = dt.zips.by.fips.nhs.20, y = dt.new.home.starts.20[, list(FIPS, Units)], by.x = c("county_fips"), by.y = c("FIPS"))

dt.zips.by.fips.nhs.20 = dt.zips.by.fips.nhs.20[order(county_fips)]

#now we need to allocate by population mix

dt.zips.by.fips.nhs.20[, Pop_mix := Population / sum(Population), by = list(county_fips)]
dt.zips.by.fips.nhs.20[, Units_allocated := Units * Pop_mix]

#now sum by zip to get rid of duplicates

dt.zips.by.fips.nhs.20 = dt.zips.by.fips.nhs.20[, list(Building_Permits = sum(Units_allocated, na.rm = T)), by = list(zcta)]

dt.zips.by.fips.nhs.20[, sum(Building_Permits)]

dt.zips.by.fips.nhs.20[, Year := 2020]


# 2021 Urban/Rural Classification ---------------------------------------------------------------

census_shape_file = tigris::zctas() #Shapefile -- zips

#Population Density

dt.density = data.table(census_shape_file)[, list(ZCTA5CE20, ALAND20)]

dt.density[, area_sqmi := ALAND20*3.8610215855e-7] #convert square meters to square miles

dt.density = merge(x = dt.density, 
                   y = census_data_21[, list(ZCTA5CE20 = ZCTA, Population)], 
                   by = c("ZCTA5CE20"))

dt.density[, Pop_per_sq_mi := Population / area_sqmi]

#Population Pareto

dt.density.pareto =  dt.density[order(-Pop_per_sq_mi)] #order by density

dt.density.pareto[, Cummulative_Pop_21 := cumsum(Population)] #add cumulative sum

dt.density.pareto[, Cummulative_Pop_21_Perc := Cummulative_Pop_21 / sum(Population)] #Add agg percent

dt.density.pareto[, Count := 1:.N] #add Count

dt.density.pareto[Cummulative_Pop_21_Perc < 0.27, Grouping := "Urban"]
dt.density.pareto[Cummulative_Pop_21_Perc >= 0.27 & Cummulative_Pop_21_Perc < 0.79, Grouping := "Sub_Urban"]
dt.density.pareto[Cummulative_Pop_21_Perc >= 0.79, Grouping := "Rural"]
dt.density.pareto[Grouping == "Urban", Color := "#32537E"]
dt.density.pareto[Grouping == "Rural", Color := "#C0D1E3"]
dt.density.pareto[Grouping == "Sub_Urban", Color := "#D55066"]

dt.class = dt.density.pareto[, list(ZCTA = ZCTA5CE20, Grouping)]

# Merge data sets -- Zip level External Data -------------------------------

dt.unified.11.2 = rbind(census_data_21, census_data_20) %>% 
  merge(y = dt.zips.by.fips, by.x = c("ZCTA"), by.y = c("zcta"), all.x = T) %>% #cost of living only at zcta, not year
  merge(y = rbind(dt.zips.by.fips.nhs.21, dt.zips.by.fips.nhs.20), by.x = c("ZCTA", "Year"), by.y = c("zcta", "Year"), all.x = T) %>% #at year/zcta level
  merge(y = dt.class,  by = c("ZCTA"), all.x = T) %>% 
  merge(y = dt.density.pareto[, list(ZCTA = ZCTA5CE20, area_sqmi)], by = c("ZCTA"), all.x = T)

dt.unified.11.2 = dt.unified.11.2[order(ZCTA, Year)]

#replace negatives with NAs

dt.unified.11.2[, (3:37) := lapply(.SD, function(x) ifelse(x < 0, NA, x)), .SDcols = 3:37]

#Add Discretionary Income

dt.unified.11.2[Cost_of_living > 0 & Median_Household_Income > 0, Discretionary_Income := Median_Household_Income - Cost_of_living]

#this is at zcta level, lets expand to zips

dt.zips.to.zcta[, Zip.Code := sprintf("%05d", Zip.Code)]
dt.zips.to.zcta[, ZCTA := sprintf("%05d", ZCTA)]

dt.unified.11.2 = merge(x = dt.zips.to.zcta[, list(Zip.Code, ZCTA)] %>% unique(), y = dt.unified.11.2, by = c("ZCTA"))


# Chapter 2: Defining Dealer Markets --------------------------------------
# Libraries and Functions -------------------------------------------------
library(readxl)

# Function to calculate distance between two latitude-longitude pairs (in miles)

calculate_distance <- function(lat1, lon1, lat2, lon2) {
  rad <- pi/180
  miles_per_km <- 0.621371
  
  # Convert latitude and longitude from degrees to radians
  lat1_rad <- lat1 * rad
  lon1_rad <- lon1 * rad
  lat2_rad <- lat2 * rad
  lon2_rad <- lon2 * rad
  
  # Calculate differences between latitudes and longitudes
  d_lat <- lat2_rad - lat1_rad
  d_lon <- lon2_rad - lon1_rad
  
  # Calculate distance using haversine formula
  a <- sin(d_lat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(d_lon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance_km <- 6371 * c
  
  # Convert distance from kilometers to miles
  distance_miles <- distance_km * miles_per_km
  
  return(distance_miles)
}


# Data Retrieval ----------------------------------------------------------

# Get competitor dealers
setzipaschar3 = "character"
names(setzipaschar3) = "Store.Zip"
dt.comp = data.table(read.csv("./competitor dealers_v2.csv",colClasses = setzipaschar3))
for (i in 1:5) {dt.comp[nchar(Store.Zip)<5,Store.Zip:=paste0("0",Store.Zip)]}

# Get long/lat mapping
dt.longlat <- data.table(read.csv("./Full list of zips and coordinates v1.csv")) %>% .[, Zip.as.Text := sprintf("%05d", Zip.as.Text)]

# Get home improvement retailers
dt.retail = data.table(read.csv("./homeimp_retail_locations.csv"))

# Get dealer data
setzipaschar2 = "character"
names(setzipaschar2) = "purchasezip"
dt.hd2 = data.table(read.csv("./bookings_by_dealer_w_attributes_v2.csv",colClasses = setzipaschar2))
dt.hd2[,alliance_num := as.numeric(gsub("\\D", "", alliance))]
# HD summed booking amt by purchase zip - defines unique purchase-zips / markets
dt.bookings_byzip = dt.hd2[year=="2023_TTM" & bookingamt>0,list(bookingamt=sum(bookingamt,na.rm=T)),by=purchasezip]

#merge on Grouping

dt.bookings_byzip = merge(x = dt.bookings_byzip, y = dt.unified.11.2[, list(Zip.Code, Grouping)] %>% unique(), by.x = c("purchasezip"), by.y = c("Zip.Code"))

# Loop to define markets --------------------------------------------------

# loop through each HD dealer zip to define market zips;
# sum up retail locations, competitor locations, HD locations, and HD booking $s
dt.dealer_mkt_eval = rbindlist(lapply(dt.bookings_byzip[bookingamt>0,purchasezip], function(zipcur) {
  dt.longlat_cur = dt.longlat[Zip.as.Text == zipcur]
  if (nrow(dt.longlat_cur)!=1) {
    warning(paste0("zip=",zipcur," - no long/lat found"))
    return(data.table(purchasezip=zipcur, homeimp_retailers=NA, comp_dealers=NA, HD_bookingamt=NA, HD_dealers_all=NA, HD_dealers_over36k=NA,HD_program_dealers=NA))
  }
  
  dt.dist = dt.longlat_cur[,list(purchasezip=Zip.as.Text,curLat=Latitude,curLong=Longitude,x=1)] %>% 
    merge(dt.longlat[,list(mktzip=Zip.as.Text,mktLat=Latitude,mktLong=Longitude,x=1)],by='x',allow.cartesian = T)
  dt.dist[,dist := calculate_distance(curLat,curLong,mktLat,mktLong)]
  n.dist_limit = switch(dt.bookings_byzip[purchasezip==zipcur,Grouping[1]],"Urban"=15,"Sub_Urban"=20,"Rural"=25)
  dt.dist_keep = dt.dist[dist < n.dist_limit]
  
  data.table(purchasezip = zipcur,
             homeimp_retailers=dt.retail[zip_codes %in% dt.dist_keep[,mktzip],sum(retailstores)],
             comp_dealers=dt.comp[Store.Zip %in% dt.dist_keep[,mktzip],.N],
             dt.hd2[year=="2023_TTM" & purchasezip %in% dt.dist_keep[,mktzip],list(bookingamt=sum(bookingamt)),by=list(storename)][bookingamt>0,list(HD_bookingamt=sum(bookingamt),HD_dealers_all=.N,HD_dealers_over36k=.SD[bookingamt>36000,.N])],
             dt.hd2[year=="2023_TTM" & purchasezip %in% dt.dist_keep[,mktzip]][(brand=="CBG" & alliance_num<8)|(brand=="HD Brand" & alliance_num<5)][bookingamt>0,list(HD_program_dealers=length(unique(customer_id3)))])
}))

#we also need a list of purchase zip by market zips
dt.dealer_mkt_mapping = rbindlist(lapply(dt.bookings_byzip[bookingamt>0,purchasezip], function(zipcur) {
  dt.longlat_cur = dt.longlat[Zip.as.Text == zipcur]
  if (nrow(dt.longlat_cur)!=1) {
    warning(paste0("zip=",zipcur," - no long/lat found"))
    return(data.table(dt.bookings_byzip[purchasezip==zipcur], mktzip = NA,dist = NA, curLat = NA, curLong = NA, mktLat = NA , mktLong = NA))
  }
  
  dt.dist = dt.longlat_cur[,list(purchasezip=Zip.as.Text,curLat=Latitude,curLong=Longitude,x=1)] %>% 
    merge(dt.longlat[,list(mktzip=Zip.as.Text,mktLat=Latitude,mktLong=Longitude,x=1)],by='x',allow.cartesian = T)
  dt.dist[,dist := calculate_distance(curLat,curLong,mktLat,mktLong)]
  n.dist_limit = switch(dt.bookings_byzip[purchasezip==zipcur,Grouping[1]],"Urban"=15,"Sub_Urban"=20,"Rural"=25)
  dt.dist_keep = dt.dist[dist < n.dist_limit]
  
  data.table(dt.bookings_byzip[purchasezip==zipcur], dt.dist_keep[,list(mktzip, dist, curLat, curLong, mktLat, mktLong )])
}))

dt.booking_zip_market = dt.dealer_mkt_mapping[order(purchasezip, dist)]

dt.markets = dt.booking_zip_market[, list(purchasezip, mktzip, dist)] %>% unique()



# Chapter 3: Aggregate External Data to Dealer Market Level ---------------

dt.dealer.markets.use = copy(dt.markets)

#Filter out NAs

dt.dealer.markets.use = dt.dealer.markets.use[ dist >= 0 & !is.na(dist)]

#now that we have purchase zip by mkt zip, lets merge by mkt zip to bring in data

dt.dealer.markets.use = merge(x = dt.dealer.markets.use, y = dt.unified.11.2, by.x = c("mktzip"), by.y = c("Zip.Code"), allow.cartesian=TRUE)

#to avoid double counting, take it down to zcta level

dt.dealer.markets.use = dt.dealer.markets.use[, list(Median_Age_Total, 
                                                     Avg_Household_Size, 
                                                     Median_Household_Income, 
                                                     Occ_vs_Vac_Total,
                                                     Occ_vs_Vac_Occ,
                                                     Occ_vs_Vac_Vac,
                                                     Own_vs_rent_Total,
                                                     Own_vs_rent_own,
                                                     Own_vs_rent_rent,
                                                     Total_Housing_Units, 
                                                     Median_Rooms, 
                                                     Median_year_structure_built, 
                                                     Single_family_Units, 
                                                     Total_Units, 
                                                     Median_census_home_value, 
                                                     Migration_Total,
                                                     Migration_No_Change,
                                                     Migration_within_county,
                                                     Migration_diff_county,
                                                     Migration_diff_state,
                                                     Migration_abroad, 
                                                     Home_Owner_Costs_Monthly, 
                                                     Population, 
                                                     Households, 
                                                     Single_family_units_Perc, 
                                                     Own_vs_rent_own_Perc, 
                                                     Cost_of_living, 
                                                     Building_Permits, 
                                                     Discretionary_Income, 
                                                     Zips_included = .N, 
                                                     HH_greater_100k, 
                                                     area_sqmi), 
                                              by = list(purchasezip, ZCTA, Year)] %>% unique()

dt.dealer.markets.use = dt.dealer.markets.use[order(purchasezip)]


#now lets roll up to purchase zip level

dt.dealer.markets.use = dt.dealer.markets.use[, list(Median_Age_Total = sum(Median_Age_Total*Households, na.rm = T) / sum(Households, na.rm = T), #weight by households
                                                     Avg_Household_Size = sum(Avg_Household_Size*Households, na.rm = T) / sum(Households, na.rm = T), #weight by households 
                                                     Median_Household_Income = sum(Median_Household_Income*Households, na.rm = T) / sum(Households, na.rm = T), #weight by households
                                                     Occ_vs_Vac_Total = sum(Occ_vs_Vac_Total, na.rm = T), #just sum
                                                     Occ_vs_Vac_Occ = sum(Occ_vs_Vac_Occ, na.rm = T), #just sum
                                                     Occ_vs_Vac_Vac = sum(Occ_vs_Vac_Vac, na.rm = T), #just sum
                                                     Own_vs_rent_Total = sum(Own_vs_rent_Total, na.rm = T), #just sum
                                                     Own_vs_rent_own = sum(Own_vs_rent_own, na.rm = T), #just sum
                                                     Own_vs_rent_rent = sum(Own_vs_rent_rent, na.rm = T), #just sum
                                                     Total_Housing_Units = sum(Total_Housing_Units, na.rm = T), #just sum
                                                     Median_Rooms = sum(Median_Rooms*Households, na.rm = T) / sum(Households, na.rm = T), #weight by households  
                                                     Median_year_structure_built = sum(Median_year_structure_built*Households, na.rm = T) / sum(Households, na.rm = T), #weight by households 
                                                     Single_family_Units = sum(Single_family_Units, na.rm = T), #just sum
                                                     Total_Units = sum(Total_Units, na.rm = T), #just sum
                                                     Median_census_home_value = sum(Median_census_home_value*Households, na.rm = T) / sum(Households, na.rm = T), #weight by households
                                                     Households = sum(Households, na.rm = T), #just sum, 
                                                     Migration_Total = sum(Migration_Total, na.rm = T), #just sum
                                                     Migration_No_Change = sum(Migration_No_Change, na.rm = T), #just sum
                                                     Migration_within_county = sum(Migration_within_county, na.rm = T), #just sum
                                                     Migration_diff_county = sum(Migration_diff_county, na.rm = T), #just sum
                                                     Migration_diff_state = sum(Migration_diff_state, na.rm = T), #just sum
                                                     Migration_abroad = sum(Migration_abroad, na.rm = T), #just sum 
                                                     Home_Owner_Costs_Monthly = sum(Home_Owner_Costs_Monthly*Households, na.rm = T) / sum(Households, na.rm = T), #weight by households 
                                                     Population = sum(Population, na.rm = T), #just sum
                                                     Cost_of_living = sum(Cost_of_living*Households, na.rm = T) / sum(Households, na.rm = T), #weight by households
                                                     Building_Permits = sum(Building_Permits, na.rm = T), 
                                                     Zips_included = sum(Zips_included, na.rm = T), 
                                                     HH_greater_100k = sum(HH_greater_100k, na.rm = T), 
                                                     area_sqmi = sum(area_sqmi, na.rm = T)), #just sum
                                              by = list(purchasezip, Year)] %>% unique()

dt.dealer.markets.use = unique(dt.dealer.markets.use)

dt.dealer.markets.use[Cost_of_living > 0 , Discretionary_income := Median_Household_Income - Cost_of_living]

#now merge on classification

dt.dealer.markets.use = merge(x = dt.dealer.markets.use, y = dt.unified.11.2[, list(Zip.Code, Grouping)] %>% unique(), by.x = c("purchasezip"), by.y = c("Zip.Code"))


# Chapter 4: Merge Internal and External Data -----------------------------

dt.ext = copy(dt.dealer.markets.use)

dt.ext_level = dt.ext[Year==2021,list(purchasezip,Median_Household_Income,
                                      Own_vs_rent_own,Own_perc=Own_vs_rent_own/Own_vs_rent_Total,
                                      Total_rooms=Median_Rooms*Households,Single_family_Units,
                                      Median_census_home_value,Households,HH_greater_100k,Migration_Outflow=Migration_Total-Migration_No_Change,Building_Permits,
                                      Discretionary_income,Grouping, area_sqmi)] %>% 
  merge(dt.ext[,list(purchasezip,Year,Population)][order(purchasezip,Year)][,pop_growth := Population - lag(Population),by=list(purchasezip)][Year==2021][,Year:=NULL],by=c("purchasezip"))

dt.ext_level[,Migration_Inflow := pop_growth + Migration_Outflow]

dt.level2 = na.omit(merge(dt.dealer_mkt_eval, dt.ext_level,by="purchasezip"))


# Chapter 5: POEs ---------------------------------------------------------

# DEFINE POES

dt.poe = dt.level2[,list(purchasezip,Grouping,HH_greater_100k,Building_Permits,Median_census_home_value,comp_dealers,HD_dealers_all,HD_program_dealers,Households)]
dt.poe[,HH_perc := ecdf(HH_greater_100k)(HH_greater_100k),by=Grouping]
dt.poe[,permits_perc := ecdf(Building_Permits)(Building_Permits),by=Grouping]
dt.poe[,homevalue_perc := ecdf(Median_census_home_value)(Median_census_home_value),by=Grouping]
dt.poe[,growth_potential := (HH_perc+permits_perc+homevalue_perc)/3]

dt.poe[,comp_density := comp_dealers/Households]
dt.poe[,median_comp_density := median(comp_density),by=Grouping]
dt.poe[,median(comp_density)*100000,by=Grouping]

dt.poe[,POE := 1]
dt.poe[,POE := as.numeric(NA)]
dt.poe[Grouping=="Rural" & growth_potential>=0.5, POE := 1]
dt.poe[Grouping=="Rural" & growth_potential<0.5, POE := 2]
dt.poe[Grouping=="Sub_Urban" & growth_potential>=0.5 & comp_density>=median_comp_density, POE := 3]
dt.poe[Grouping=="Sub_Urban" & growth_potential>=0.5 & comp_density<median_comp_density, POE := 4]
dt.poe[Grouping=="Sub_Urban" & growth_potential<0.5, POE := 5]
dt.poe[Grouping=="Urban" & growth_potential>=0.5 & comp_density>=median_comp_density, POE := 6]
dt.poe[Grouping=="Urban" & growth_potential>=0.5 & comp_density<median_comp_density, POE := 7]
dt.poe[Grouping=="Urban" & growth_potential<0.5, POE := 8]

dt.poe[,.N,by=POE][order(POE)] %>% copytoexcel()


# Chapter 6: Regressions --------------------------------------------------

# Libraries and Functions -------------------------------------------------

library(corrplot)


# Analysis ----------------------------------------------------------------

# Identify numeric columns
numeric_cols <- sapply(dt.level2, is.numeric)

# Extract column names
v.vars <- colnames(dt.level2[, .SD, .SDcols = numeric_cols])

# correlations
cor.level2 = cor(dt.level2[HD_bookingamt>1000][,.SD,.SDcols = v.vars])
corrplot(cor.level2, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45,tl.cex = 0.5)


# kitchen sink multivariate regression
summary(lm(data=dt.level2[HD_bookingamt>1000][,!1],HD_bookingamt~.))


# univariate regressions

v.vars = v.vars[v.vars != "HD_bookingamt"]

dt.univar = rbindlist(lapply(v.vars,function(varcur) {
  lm.cur = summary(lm(data=dt.level2[HD_bookingamt>1000][,!1],as.formula(paste0("HD_bookingamt~",varcur))))
  if (nrow(lm.cur$coefficients)!=2) {
    warning(paste0("could not run regression on: ",varcur))
    return(data.table())
  }
  data.table(variable=varcur,
             r_sq=lm.cur$r.squared,
             intercept=lm.cur$coefficients[1,1],
             slope=lm.cur$coefficients[2,1],
             std_error=lm.cur$coefficients[2,2],
             t_value=lm.cur$coefficients[2,3],
             p_value=lm.cur$coefficients[2,4],
             var_sd=sd(unlist(dt.level2[,varcur,with=F])))
  
}))

dt.univar[,response := abs(slope)*var_sd][order(-response)]
dt.univar[order(-r_sq)] %>% copytoexcel()

# univariate regression WITHIN each urbanization

dt.univar_bygroup = rbindlist(lapply(dt.level2[,unique(Grouping)],function(groupcur){
  rbindlist(lapply(v.vars,function(varcur) {
    lm.cur = summary(lm(data=dt.level2[HD_bookingamt>1000][Grouping==groupcur][,!1],as.formula(paste0("HD_bookingamt~",varcur))))
    if (nrow(lm.cur$coefficients)!=2) {
      warning(paste0("could not run regression on: ",varcur))
      return(data.table())
    }
    data.table(Grouping=groupcur,
               variable=varcur,
               r_sq=lm.cur$r.squared,
               intercept=lm.cur$coefficients[1,1],
               slope=lm.cur$coefficients[2,1],
               std_error=lm.cur$coefficients[2,2],
               t_value=lm.cur$coefficients[2,3],
               p_value=lm.cur$coefficients[2,4],
               var_sd=sd(unlist(dt.level2[,varcur,with=F])))
    
  }))
}))


dt.univar_bygroup[,response := abs(slope)*var_sd][order(-response)]
dt.univar_bygroup[order(Grouping,-r_sq)] %>% copytoexcel()


# pruned multivariate regression
dt.lm_input = dt.level2[HD_bookingamt>1000][,!1][,HD_dealer_density:=HD_dealers_over36k/Households][,comp_dealer_density:=comp_dealers/Households][,dealer_density:=(HD_dealers_over36k+comp_dealers)/Households]

lm.simple = summary(lm(data=dt.lm_input,HD_bookingamt~HH_greater_100k+Building_Permits+Median_census_home_value+Grouping))
dt.lm_output = data.table(variable=dimnames(lm.simple$coefficients)[[1]],lm.simple$coefficients)

v.var_sds = dt.level2[HD_bookingamt>1000][,dt.lm_output$variable[-1][!dt.lm_output$variable[-1] %like% "Grouping"],with=F][,sapply(.SD,sd)] %>% unlist %>% t %>% t
dt.lm_output_response = cbind(dt.lm_output,var_sd=c(0,v.var_sds,rep(0,nrow(dt.lm_output)-length(v.var_sds)-1)))
dt.lm_output_response[,response := ifelse(var_sd==0,Estimate/1000,var_sd*Estimate/1000)]
dt.lm_output_response

# model fit - as we add variables, how does it improve model fit?
lm.simple1 = summary(lm(data=dt.lm_input,HD_bookingamt~HH_greater_100k+Building_Permits+Median_census_home_value+Grouping))
dt.lm_output = data.table(variable=dimnames(lm.simple$coefficients)[[1]],lm.simple$coefficients)

lm.1 = lm(data=dt.lm_input,HD_bookingamt~HH_greater_100k)
lm.2 = lm(data=dt.lm_input,HD_bookingamt~HH_greater_100k+Building_Permits)
lm.3 = lm(data=dt.lm_input,HD_bookingamt~HH_greater_100k+Building_Permits+Median_census_home_value)
lm.4 = lm(data=dt.lm_input,HD_bookingamt~HH_greater_100k+Building_Permits+Median_census_home_value+Grouping)

dt.fit_scatter = rbind(
  dt.lm_input[order(HD_bookingamt),list(rank=1:.N,actual=HD_bookingamt,fit=HD_bookingamt,type="actual")],
  data.table(lm.1$model,fit=lm.1$fitted.values)[order(HD_bookingamt),list(rank=1:.N,actual=HD_bookingamt,fit,type="lm.1")],
  data.table(lm.2$model,fit=lm.2$fitted.values)[order(HD_bookingamt),list(rank=1:.N,actual=HD_bookingamt,fit,type="lm.2")],
  data.table(lm.3$model,fit=lm.3$fitted.values)[order(HD_bookingamt),list(rank=1:.N,actual=HD_bookingamt,fit,type="lm.3")],
  data.table(lm.4$model,fit=lm.4$fitted.values)[order(HD_bookingamt),list(rank=1:.N,actual=HD_bookingamt,fit,type="lm.4")]
)

ggplot(mapping=aes(rank,fit/1E6)) + geom_point(data=dt.fit_scatter[type!="actual"],mapping=aes(color=type),alpha=0.4) + 
  geom_point(data=dt.fit_scatter[type=="actual"]) + 
  theme_bw(base_size = 16) + scale_color_manual(values=c("#A46869","#618BBA","#B0B0D1","#A9CAB9"))

dt.fit_scatter[,list(MAPE=mean(abs(fit-actual)/actual)),by=type]


# Chapter 7: Bolt on -- TTM Permits March 2023 ----------------------------

dt.TTM.Permits = rbind(fread("https://www2.census.gov/econ/bps/County/co2303y.txt"), 
             fread("https://www2.census.gov/econ/bps/County/co2212c.txt"), 
             fread("https://www2.census.gov/econ/bps/County/co2211c.txt"), 
             fread("https://www2.census.gov/econ/bps/County/co2210c.txt"), 
             fread("https://www2.census.gov/econ/bps/County/co2209c.txt"), 
             fread("https://www2.census.gov/econ/bps/County/co2208c.txt"), 
             fread("https://www2.census.gov/econ/bps/County/co2207c.txt"), 
             fread("https://www2.census.gov/econ/bps/County/co2206c.txt"), 
             fread("https://www2.census.gov/econ/bps/County/co2205c.txt"), 
             fread("https://www2.census.gov/econ/bps/County/co2204c.txt"))

dt.TTM.Permits = dt.TTM.Permits[, list(SFH_Units = V8), 
                                by = list(State = sprintf("%02d", V2), 
                                          County = sprintf("%03d", V3), 
                                          Date = V1)][, FIPS := paste0(State, County)]

#check work
dt.TTM.Permits[, list(Units = sum(SFH_Units)), by = Date] 

#allocate to zips

dt.TTM.Permits = dt.TTM.Permits[, list(Units = sum(SFH_Units, na.rm = T)), by = FIPS]

#Start with a map of FIPS by ZIP -- note not 1:1
dt.zips.by.fips.nhs.march.TTM = data.table(zcta_crosswalk)
dt.zips.by.fips.nhs.march.TTM = dt.zips.by.fips.nhs.march.TTM[, list(county_fips, zcta)][order(county_fips)]

#pull in ZIP level population data
dt.zips.by.fips.nhs.march.TTM = merge(x = dt.zips.by.fips.nhs.march.TTM, y = census_data_21[, list(ZCTA, Population)], by.x = c("zcta"), by.y = c("ZCTA"))

#now pull in FIP level permits

dt.zips.by.fips.nhs.march.TTM = merge(x = dt.zips.by.fips.nhs.march.TTM, y = dt.TTM.Permits, by.x = c("county_fips"), by.y = c("FIPS"))

dt.zips.by.fips.nhs.march.TTM = dt.zips.by.fips.nhs.march.TTM[order(county_fips)]

#now we need to allocate by population mix

dt.zips.by.fips.nhs.march.TTM[, Pop_mix := Population / sum(Population), by = list(county_fips)]
dt.zips.by.fips.nhs.march.TTM[, Units_allocated := Units * Pop_mix]

#now sum by zip to get rid of duplicates

dt.zips.by.fips.nhs.march.TTM = dt.zips.by.fips.nhs.march.TTM[, list(Building_Permits_march_TTM = sum(Units_allocated, na.rm = T)), by = list(zcta)]

dt.zips.by.fips.nhs.march.TTM[, sum(Building_Permits_march_TTM)]

#now we need to aggregate to market level

dt.permit.market = copy(dt.dealer_mkt_mapping)

#Convert market zips to zcta

dt.permit.market = merge(x = dt.permit.market, y = dt.zips.to.zcta, by.x = c("mktzip"), by.y = c("Zip.Code"))

#now take down to zcta level to avoid double counting

dt.permit.market = dt.permit.market[, list(purchasezip), by = list(mkt_zcta = ZCTA)] %>% unique() %>% .[order(purchasezip)]

#merge on permits

dt.permit.market = merge(x = dt.permit.market, y = dt.zips.by.fips.nhs.march.TTM, by.x = c("mkt_zcta"), by.y = c("zcta")) %>% .[order(purchasezip, mkt_zcta)]

dt.permit.market = dt.permit.market[, list(mkt_Building_Permits_march_TTM = sum(Building_Permits_march_TTM, na.rm = T)), by = list(purchasezip)]


# Chapter 8: Dealer Performance Summary ----------------------------------------------

dt.hd_perf = dcast.data.table(dt.hd2[year %in% c("2022_PTM","2023_TTM"),list(storename,purchasezip,brand,bookingamt,year)],formula = storename+purchasezip+brand~year,value.var = "bookingamt",fun.aggregate = sum)[is.na(`2022_PTM`),`2022_PTM` := 0][is.na(`2023_TTM`),`2023_TTM` := 0][`2022_PTM` > 0][nchar(purchasezip)==5] %>% 
  merge(dt.hd2[year == "2022_PTM",list(bookingamt=sum(bookingamt)),by=list(storename,brand,alliance,alliance_num)][order(storename,-bookingamt)][,.SD[1],by=list(storename,brand)],by=c("storename","brand"))

#merge on market summary

old_col_names <- colnames(dt.level2)
new_col_names <- paste0("mkt_", old_col_names)

setnames(dt.level2, old_col_names, new_col_names)

dt.hd_perf = merge(x = dt.hd_perf, y = dt.level2, by.x = c("purchasezip"), by.y = c("mkt_purchasezip")) %>% 
  merge(y = dt.permit.market, by = c("purchasezip")) #march TTM purchase permits



dt.hd_perf = dt.hd_perf[order(purchasezip)]

# Exporting Script --------------------------------------------------------

write.csv(dt.hd_perf,"./Dealer Rollup 11.10.23v1.csv")


