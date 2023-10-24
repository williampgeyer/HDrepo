# web scraping
library(rvest)
library(stringr)
library(data.table)

# get Home Depot locations
dt.homedepot_zips = rbindlist(lapply(state.abb, function(state.cur) {
  chr.link = paste0("https://www.homedepot.com/l/",state.cur)
  
  page <- read_html(chr.link)
  
  v.zips = html_element(page,xpath="//*[contains(@class, 'store-directory')]") %>% html_text() %>% 
    str_match_all(paste0(", ",state.cur," ","\\s*(.*?)\\s*","\\(")) %>% .[[1]] %>% .[,2]
  data.table(state=state.cur,zip_codes=v.zips)
}))

write.csv(dt.homedepot_zips,file = "C:/Users/wgeyer/OneDrive - Mars & Co Consulting LLC/Desktop/HomeDepotZips.csv")

# Lowe's locations
read_html("https://www.lowes.com/Lowes-Stores/Alabama/AL")
# website being difficult... might have an anti-scraping tool

# use AllStays - https://www.allstays.com/c/lowes-new-jersey-locations.htm

dt.lowes_zips = rbindlist(lapply(state.abb, function(state.cur) {
  statename.cur = state.name[state.abb==state.cur] %>% tolower() %>% gsub(pattern=" ",replacement="-")
  chr.link = paste0("https://www.allstays.com/c/lowes-",statename.cur,"-locations.htm")
  flag.error = FALSE
  tryCatch({
    page <- read_html(chr.link)},
    error = function(e) {flag.error <<- TRUE})
  if (flag.error) return(data.table())
  
  v.zips = html_element(page,xpath="//*[contains(@class, 'col-md-5')]") %>% html_text() %>% 
    str_match_all(paste0(" ",state.cur," ","\\s*(.*?)\\s*"," ph:")) %>% .[[1]] %>% .[,2]
  if (length(v.zips)==0) return(data.table()) else return(data.table(state=state.cur,zip_codes=v.zips))
  
}))

write.csv(dt.lowes_zips,file = "C:/Users/wgeyer/OneDrive - Mars & Co Consulting LLC/Desktop/LowesZips.csv")


# Menards via AllStays

dt.menards_zips = rbindlist(lapply(state.abb, function(state.cur) {
  statename.cur = state.name[state.abb==state.cur] %>% tolower() %>% gsub(pattern=" ",replacement="-")
  chr.link = paste0("https://www.allstays.com/c/menards-",statename.cur,"-locations.htm")
  flag.error = FALSE
  tryCatch({
    page <- read_html(chr.link)},
    error = function(e) {flag.error <<- TRUE})
  if (flag.error) return(data.table())
  
  v.zips = html_element(page,xpath="//*[contains(@class, 'col-md-5')]") %>% html_text() %>% 
    str_match_all(paste0(" ",state.cur," ","\\s*(.*?)\\s*"," ph:")) %>% .[[1]] %>% .[,2]
  if (length(v.zips)==0) return(data.table()) else return(data.table(state=state.cur,zip_codes=v.zips))
  
}))

write.csv(dt.menards_zips,file = "C:/Users/wgeyer/OneDrive - Mars & Co Consulting LLC/Desktop/MernardsZips.csv")


# Costco via Allstays

dt.costco_zips = rbindlist(lapply(state.abb, function(state.cur) {
  statename.cur = state.name[state.abb==state.cur] %>% tolower() %>% gsub(pattern=" ",replacement="-")
  chr.link = paste0("https://www.allstays.com/c/costco-",statename.cur,"-locations.htm")
  flag.error = FALSE
  tryCatch({
    page <- read_html(chr.link)},
    error = function(e) {flag.error <<- TRUE})
  if (flag.error) return(data.table())
  
  v.zips = html_element(page,xpath="//*[contains(@class, 'col-md-5')]") %>% html_text() %>% 
    str_match_all(paste0(" ",state.cur," ","\\s*(.*?)\\s*"," ph:")) %>% .[[1]] %>% .[,2]
  if (length(v.zips)==0) return(data.table()) else return(data.table(state=state.cur,zip_codes=v.zips))
  
}))

write.csv(dt.costco_zips,file = "C:/Users/wgeyer/OneDrive - Mars & Co Consulting LLC/Desktop/CostcoZips.csv")



# The Shade Store

chr.link = "https://www.theshadestore.com/showrooms"
page_showrooms <- read_html(chr.link)
v.showrooms = page_showrooms %>% html_nodes(".name") %>% html_attr('href') %>% as.character %>% na.omit()

dt.shadestore_zips = rbindlist(lapply(v.showrooms, function(cur.showroom) {
  chr.link = paste0("https://www.theshadestore.com",cur.showroom)
  page <- read_html(chr.link)
  chr.zip = page %>% html_nodes(".hero-dark-copy") %>% html_text() %>% .[2] %>% str_sub(-6,-2)
  state.cur = state.abb[sapply(tolower(state.name),function(state.long){grepl(gsub(x=state.long," ","-"),cur.showroom,fixed=T)})]
  data.table(state=state.cur,zip_codes=chr.zip)
}))

write.csv(dt.shadestore_zips,file = "C:/Users/wgeyer/OneDrive - Mars & Co Consulting LLC/Desktop/ShadeStoreZips.csv")

# Gruber from download
page <- read_html("C:/Users/wgeyer/OneDrive - Mars & Co Consulting LLC/Desktop/test.html")
page <- read_html("C:/Users/wgeyer/Downloads/30328 & 80550.txt")
page <- read_html("C:/Users/wgeyer/Downloads/01701 - 53005.txt")
list.loc = page %>% html_nodes(".dealer-card--content") %>% html_text() %>% str_split(pattern="\r\n")
rbindlist(lapply(1:length(list.loc),function(i) {
  v.cur = list.loc[[i]] %>% gsub(pattern=" ",replacement="")
  v.cur = v.cur[nchar(v.cur)>0]
  n.dashes = length(str_match_all(v.cur[5],"-")[[1]])
  if (n.dashes==2) {
    return(data.table(dealername=v.cur[1],dealertype=v.cur[2],dealerzip=str_sub(str_split(v.cur[5],pattern="-")[[1]][1],-8,-4)))
  } else if (n.dashes==3|n.dashes==1) {
    return(data.table(dealername=v.cur[1],dealertype=v.cur[2],dealerzip=str_sub(str_split(v.cur[5],pattern="-")[[1]][1],-5,-1)))
  } else {
    warning(paste0("i=",i," - no zip found"))
    return(data.table())
  }
  
}))

