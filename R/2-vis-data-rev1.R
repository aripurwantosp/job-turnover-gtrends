# ************************************************************************
# Apakah Turnover ke Sektor Informal Terjadi Saat Pandemi COVID-19?
#   Signal dari Data Google Trends
# 
# 2. Visualisasi Data
# 
# Ari Purwanto Sarwo Prasojo
#
# -Pusat Riset Kependudukan, Badan Riset dan Inovasi Nasional
# -Program Studi Magister Ekonomi Kependudukan dan Ketenagakerjaan,
#  Fakultas Ekonomi dan Bisnis, Universitas Indonesia
# 
# 2022
# ************************************************************************


library(tidyverse)
library(ggwordcloud)


# singnmn <- "scrap-grsr-1920-q"
# relnm <- "grsr-rel-id-q"
# multnm <- "grsr-mult-id-q"


# ************************************************************************
# Kata kunci pencarian (parsial)----

#membaca data
dtatrends <- read_csv(paste0("data/",singnmn,".csv"))

## phk----
dtatrends %>% filter(categories=="PHK") %>% 
  ggplot(aes(days1a,grsr_n,group=factor(period),color=factor(period))) +
  # geom_line() +
  geom_point(size=.6) +
  geom_smooth(method=loess, method.args=list(family="symmetric",degree=2),
              span=0.5,level=.95,size=.5) +
  geom_vline(aes(xintercept = 0),color="#cd375d",
             linetype = "longdash",size=.5) +
  geom_vline(aes(xintercept = 12),color="#578a85",
             linetype = "longdash",size=.5) +
  scale_color_manual(breaks = c("2019","2020"),
                     values = c("black","red")) +
  labs(color="", x="Minggu menurut pelaksanaan WFH/pembatasan fisik",
       y="GRSR") +
  facet_wrap(~categories) +
  scale_x_continuous(breaks = seq(-10,32,2),
                     minor_breaks = NULL) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8))
ggsave("graphics/phk-q.png")

## pencarian kerja----
dtatrends %>% filter(categories!="PHK") %>% 
  ggplot(aes(days1a,grsr_n,group=factor(period),color=factor(period))) +
  # geom_line() +
  geom_point(size=.6) +
  geom_smooth(method=loess, method.args=list(family="symmetric",degree=2),
              span=0.5,level=.95,size=.5) +
  geom_vline(aes(xintercept = 0),color="#cd375d",
             linetype = "longdash",size=.5) +
  geom_vline(aes(xintercept = 12),color="#578a85",
             linetype = "longdash",size=.5) +
  scale_color_manual(breaks = c("2019","2020"),
                     values = c("black","red")) +
  labs(color="", x="Minggu menurut pelaksanaan WFH/pembatasan fisik",
       y="GRSR") +
  facet_wrap(~categories) +
  scale_x_continuous(breaks = seq(-10,32,2),
                     minor_breaks = NULL) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8))
ggsave("graphics/carikerja-q.png")


# ************************************************************************
# Kueri atau kata kunci pencarian yang berhubungan----

#membaca data
relquer <- read_csv(paste0("data/",relnm,".csv"))

#wordcloud---
relquer %>% filter(categories!="PHK",subject>=15) %>% 
ggplot(aes(label=value, size=subject,color=subject)) +
  # geom_text_wordcloud(show.legend=TRUE,rm_outside=TRUE) +
  geom_text_wordcloud(show.legend=TRUE,rm_outside=FALSE,
                      grid_size = 1, max_steps = 1,
                      eccentricity = .9)+
  scale_size_area(max_size = 4) +
  labs(title=NULL, color=NULL) +
  guides(size=FALSE) +
  facet_wrap(~categories) +
  scale_colour_gradient(low="gray40",high="red") +
  theme(legend.position = "none")
ggsave("graphics/kueri-terkait-q.png")

## Berjualan
relquer %>% filter(categories=="Berjualan") %>% 
  ggplot(aes(label=value, size=subject,color=subject)) +
  geom_text_wordcloud(show.legend=TRUE,rm_outside=TRUE) +
  labs(title=NULL, color=NULL) +
  guides(size=FALSE) +
  scale_colour_gradient(low="gray40",high="red") +
  theme(legend.position = "none")

## Driver online
relquer %>% filter(categories=="Driver online") %>% 
  ggplot(aes(label=value, size=subject,color=subject)) +
  geom_text_wordcloud(show.legend=TRUE,rm_outside=TRUE) +
  labs(title=NULL, color=NULL) +
  guides(size=FALSE) +
  scale_colour_gradient(low="gray40",high="red") +
  theme(legend.position = "none")

## Kurir
relquer %>% filter(categories=="Kurir") %>% 
  ggplot(aes(label=value, size=subject,color=subject)) +
  geom_text_wordcloud(show.legend=TRUE,rm_outside=TRUE) +
  labs(title=NULL, color=NULL) +
  guides(size=FALSE) +
  scale_colour_gradient(low="gray40",high="red") +
  theme(legend.position = "none")

## Situs loker
relquer %>% filter(categories=="Situs loker") %>% 
  ggplot(aes(label=value, size=subject,color=subject)) +
  geom_text_wordcloud(show.legend=TRUE,rm_outside=TRUE) +
  labs(title=NULL, color=NULL) +
  guides(size=FALSE) +
  scale_colour_gradient(low="gray40",high="red") +
  theme(legend.position = "none")


# ************************************************************************
# Kata kunci pencarian (berganda) untuk dibandingkan----

#membaca data
dtatrends2 <- read_csv(paste0("data/",multn,".csv"))

## pencarian kerja----
dtatrends2 %>% 
  ggplot(aes(days1a,grsr,group=categories,color=categories))+
  geom_point(size=.6) +
  geom_smooth(method=loess, method.args=list(family="symmetric",degree=2),
              span=0.5,level=.95,size=.5) +
  geom_vline(aes(xintercept = 0),color="#cd375d",
             linetype = "longdash",size=.5) +
  geom_vline(aes(xintercept = 12),color="#578a85",
             linetype = "longdash",size=.5) +
  labs(color="", x="Minggu menurut pelaksanaan WFH/pembatasan fisik",
       y="GRSR") +
  scale_x_continuous(breaks = seq(-10,32,2),
                     minor_breaks = NULL) +
  scale_color_manual(breaks = c("Berjualan","Driver online",
                                "Kurir", "Situs loker"),
                     values = c("blue","red","seagreen4","black")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8))
ggsave("graphics/carikerja-bandingkan-q.png")
