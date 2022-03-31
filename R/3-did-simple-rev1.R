# ************************************************************************
# Apakah Turnover ke Sektor Informal Terjadi Saat Pandemi COVID-19?
#   Signal dari Data Google Trends
# 
# 3. Simple Difference in Difference
# 
# Ari Purwanto Sarwo Prasojo
#
# -Pusat Riset Kependudukan, Badan Riset dan Inovasi Nasional
# -Program Studi Magister Ekonomi Kependudukan dan Ketenagakerjaan,
#  Fakultas Ekonomi dan Bisnis, Universitas Indonesia
# 
# 2022
# ************************************************************************


library(sandwich)
library(lmtest)

# singnmn <- "scrap-grsr-1920-q"
#membaca data
dtatrends <- read_csv(paste0("data/",singnmn,".csv"))


# ************************************************************************
# WFH/pembatasan fisik----

dtatrends %>% 
  group_by(categories,grp) %>% 
  filter(days2<0) %>% 
  summarise(mean = mean(grsr_n))

dtatrends %>% 
  group_by(categories,post1a) %>% 
  filter(days2<0) %>% 
  summarise(mean = mean(grsr_n))

## did mean----
dtatrends %>% 
  group_by(categories,grp,post1a) %>% 
  filter(days2<0) %>% 
  summarise(mean = mean(grsr_n)) %>% 
  pivot_wider(id_cols=c("categories","grp"),names_from = post1a,
              values_from = "mean") %>%
  group_by(categories) %>% 
  mutate(d = `1`-`0`,
         did = diff(d))

## did regress----
wfhdid <- dtatrends %>% 
  filter(days2 < 0) %>% 
  split(.$categories) %>% 
  map(.,~lm(grsr_n ~ grp + post1a + grp:post1a,data = .)) %>% 
  map(.,~coeftest(.,vcov=vcovHAC) %>% as.data.frame.matrix() %>% 
        rownames_to_column()) %>% 
  map_df(.,~.x,.id="categories") %>% 
  rename(pars = rowname) %>% 
  add_column(treat="WFH/pembatasan fisik",.before = "categories")
  

# ************************************************************************
# Normal baru----

dtatrends %>% 
  group_by(categories,grp) %>% 
  filter(days1a >= 0) %>% 
  summarise(mean = mean(grsr_n))
  
dtatrends %>% 
  group_by(categories,post2) %>% 
  filter(days1a >= 0) %>% 
  summarise(mean = mean(grsr_n))

## did mean----
dtatrends %>% 
  group_by(categories,grp,post2) %>% 
  filter(days1a >= 0) %>% 
  summarise(mean = mean(grsr_n)) %>% 
  pivot_wider(id_cols=c("categories","grp"),names_from = post2,
              values_from = "mean") %>%
  group_by(categories) %>% 
  mutate(d = `1`-`0`,
         did = diff(d))

## did regress----
newndid <- dtatrends %>% 
  filter(days1a >= 0) %>% 
  split(.$categories) %>% 
  map(.,~lm(grsr_n ~ grp + post2 + grp:post2,data = .)) %>% 
  map(.,~coeftest(.,vcov=vcovHAC) %>% as.data.frame.matrix() %>% 
        rownames_to_column()) %>% 
  map_df(.,~.x,.id="categories") %>% 
  rename(pars = rowname) %>% 
  add_column(treat="Normal baru",.before = "categories")


# ************************************************************************
# Plot----

estdid <- bind_rows(wfhdid,newndid) %>% 
  mutate(categories = factor(categories,
                             levels = c("PHK","Berjualan","Driver online",
                                        "Kurir","Situs loker")),
         treat = factor(treat,levels=c("WFH/pembatasan fisik",
                                       "Normal baru")))

estdid %>% filter(str_detect(pars,"grp:")) %>% 
  mutate(ll = Estimate - 1.96*`Std. Error`,
         ul = Estimate + 1.96*`Std. Error`) %>% 
  ggplot(aes(categories,Estimate,group=treat)) +
  geom_abline(intercept = 0, slope = 0) +
  geom_bar(aes(fill=treat),stat = "identity",
           width=0.6,position = "dodge") +
  geom_errorbar(aes(ymin=ll,ymax=ul,color=treat), width=0.15,
                position = position_dodge(.6)) +
  labs(x="",y=expression("Estimasi DD " (beta[3])),fill="") +
  guides(color=FALSE, size=FALSE) +
  scale_fill_manual(breaks=c("WFH/pembatasan fisik","Normal baru"),
                    values=c("#cd375d","#578a85")) +
  scale_color_manual(breaks=c("WFH/pembatasan fisik","Normal baru"),
                    values=c("#cd375d","#578a85")) +
  theme(legend.position = "bottom")
ggsave("graphics/simple-did-q.png")
