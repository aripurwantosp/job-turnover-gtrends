# ************************************************************************
# Apakah Turnover ke Sektor Informal Terjadi Saat Pandemi COVID-19?
#   Signal dari Data Google Trends
# 
# 1. Scrapping/Pengambilan Data Google Trends
# 
# Ari Purwanto Sarwo Prasojo
#
# -Pusat Riset Kependudukan, Badan Riset dan Inovasi Nasional
# -Program Studi Magister Ekonomi Kependudukan dan Ketenagakerjaan,
#  Fakultas Ekonomi dan Bisnis, Universitas Indonesia
# 
# 2022
# ************************************************************************


library(gtrendsR)
library(tidyverse)
library(tidyr)
library(xlsx)


# ************************************************************************
# Penyiapan----
#-dokumentasi
#-https://support.google.com/trends/answer/4365533?hl=en

keywords <- read.xlsx("data/keywords.xlsx",sheetIndex = 1) %>% 
  filter(selected==1)
glimpse(keywords)
period1 <- "2019-01-01 2019-10-31"
period2 <- "2020-01-01 2020-10-31"
allperiod <- "2019-01-01 2020-10-31"
loc <- "ID"

# disimpan dengan nama
singnm <- "grsr-1920-id"
singnmn <- "scrap-grsr-1920"
relnm <- "grsr-rel-id"
multnm <- "grsr-mult-id"


# ************************************************************************
# Pengambilan/scrap secara parsial----
#-parsial = tanpa komparasi antar keyword

## fungsi untuk scrap----
scrap_trend <- function(keyword, location, time, category="0"){
  interest <- data.frame()

  for(i in keyword){
    print(i)
    trend <- gtrends(keyword=i, time = time, geo = location)
    
    if(!is.null(trend$interest_over_time)){
      interest <- interest %>% bind_rows(
        trend$interest_over_time %>% 
          #https://www.r-bloggers.com/2019/10/vignette-google-trends-with-the-gtrendsr-package/
          mutate(hits=sapply(hits,FUN=function(x){
            if(is.character(x)){
              x <- ifelse(x == "<1","0.5",x)
            }else{
              x
            }
            return(as.numeric(x))
          }))
      )
    }
    
  }
  return(interest %>% rename(grsr = hits))
}

## scrap-----
trends1 <- scrap_trend(keywords$keyword, loc, period1)
trends2 <- scrap_trend(keywords$keyword, loc, period2)
alltrends <- scrap_trend(keywords$keyword, loc, allperiod)


# ************************************************************************
# Penskalaan----

## pembobotan----
m19 <- trends1 %>% group_by(keyword) %>% summarise(m19 = mean(grsr))
m20 <- trends2 %>% group_by(keyword) %>% summarise(m20 = mean(grsr))
m1920 <- alltrends %>% group_by(keyword) %>% summarise(m1920 = mean(grsr))
w <- m19 %>% left_join(m20,by="keyword") %>%
  left_join(m1920,by="keyword") %>% 
  mutate(w19=m1920/m19,
         w20=m1920/m20,
         w1920=1) %>% 
  dplyr::select(contains("w")) %>% 
  pivot_longer(-keyword,names_to="period",values_to="weight") %>% 
  mutate(period = case_when(period == "w19" ~ "2019",
                            period == "w20" ~ "2020",
                            period == "w1920" ~ "2019-2020"))

## penggabungan, normalisasi, simpan----
dtatrends <- bind_rows(
  trends1 %>% add_column(period = "2019"),
  trends2 %>% add_column(period = "2020"),
  alltrends %>% add_column(period = "2019-2020")
  ) %>% 
  left_join(w,by=c("period","keyword")) %>% 
  mutate(grsr_n = grsr*weight,
         sel = ifelse(period %in% c("2019","2020"),1,0)
         ) %>%
  group_by(keyword,sel) %>% 
  mutate(mx = max(grsr_n),
         grsr_n = grsr_n/mx*100) %>% 
  ungroup() %>% 
  dplyr::select(-sel)
  
write_csv(dtatrends,paste0("data/",singnm,".csv"))


# ************************************************************************
# Data untuk analisis----
#-bentuk indeks minggu berdasarkan diterapkannya kebijakan

dtatrends <- read_csv(paste0("data/",singnm,".csv"))

#wfh/social distancing
# wfhsd19 <- as.Date("2019/03/15", format = "%Y/%m/%d")
# wfhsd20 <- as.Date("2020/03/15", format = "%Y/%m/%d")
wfhsd19 <- as.Date("2019/03/17", format = "%Y/%m/%d")
wfhsd20 <- as.Date("2020/03/15", format = "%Y/%m/%d")

#new normal
# newn19 <- as.Date("2019/06/05", format = "%Y/%m/%d")
# newn20 <- as.Date("2020/06/05", format = "%Y/%m/%d")
newn19 <- as.Date("2019/06/02", format = "%Y/%m/%d")
newn20 <- as.Date("2020/06/07", format = "%Y/%m/%d")

dtatrends %>% filter(period!="2019-2020") %>% 
  inner_join(keywords %>% dplyr::select(-description), by="keyword") %>% 
  mutate(pts1a = ifelse(period=="2019",wfhsd19,wfhsd20),
         pts2 = ifelse(period=="2019",newn19,newn20),
         date = as.Date(date),
         days1a = as.numeric(date-pts1a),
         days2 = as.numeric(date-pts2),
         grp = ifelse(period=="2019",0,1),
         post1a = ifelse(days1a<=0,0,1),
         post2 = ifelse(days2<=0,0,1)) %>% 
  mutate(low = ifelse(days1a<0,1,0)) %>% 
  group_by(categories,period,low) %>% 
  mutate(days1a = ifelse(low==1,-(n():1),(1:n())-1)) %>%
  mutate(low = ifelse(days2<0,1,0)) %>%
  mutate(days2 = ifelse(low==1,-(n():1),(1:n())-1)) %>%
  ungroup() %>% select(-low) %>% 
  write_csv(.,paste0("data/",singnmn,".csv"))


# ************************************************************************
# Kueri atau kata kunci pencarian yang berhubungan----

related <- list()
for(i in keywords$keyword){
  print(i)
  tmp <- gtrends(keyword=i, time = period2, geo = loc)
  related[[i]] <- tmp$related_queries
}

related %>% 
  map(~filter(.,related_queries=="top")) %>% 
  map_df(.,~.x,.id="keyword") %>% 
  left_join(keywords,by="keyword") %>% 
  write_csv(.,paste0("data/",relnm,".csv"))


# ************************************************************************
# Scrap secara berganda untuk dibandingkan----

#scrap
dtatrends2 <- gtrends(keyword = keywords %>% pull(keyword) %>% unique,
                time=period2,
                geo=loc)

if(!is.null(dtatrends2$interest_over_time)){
  dtatrends2 <- dtatrends2$interest_over_time %>% 
      #https://www.r-bloggers.com/2019/10/vignette-google-trends-with-the-gtrendsr-package/
      mutate(hits=sapply(hits,FUN=function(x){
        if(is.character(x)){
          x <- ifelse(x == "<1","0.5",x)
        }else{
          x
        }
        return(as.numeric(x))
      })) %>% 
    rename(grsr = hits)
}

#simpan
dtatrends2 <- dtatrends2 %>%
  inner_join(keywords %>% filter(categories!="PHK") %>% 
               select(-description),
             by="keyword") %>% 
  mutate(date = as.Date(date),
         days1a = as.numeric(date-wfhsd20),
         days2 = as.numeric(date-newn20)) %>% 
  mutate(low = ifelse(days1a<0,1,0)) %>% 
  group_by(categories,low) %>% 
  mutate(days1a = ifelse(low==1,-(n():1),(1:n())-1)) %>%
  mutate(low = ifelse(days2<0,1,0)) %>%
  mutate(days2 = ifelse(low==1,-(n():1),(1:n())-1)) %>%
  ungroup() %>% select(-low)

write_csv(dtatrends2,paste0("data/",multnm,".csv"))