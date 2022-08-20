library(haven)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
colorcode <- c("#3C3B6E", "#6E3A9E", "#B22234")


## VCF FILE 1958 to 1998 
ANES <- read_dta("Documents/R/2022 Projects/anes_timeseries_cdf_stata_20211118.dta")
ANESVCF <- ANES %>% as_survey_design(id = VCF0006, 
                                     weights = VCF0009z)
ANESmarry <- ANESVCF %>% 
  filter(VCF0102 %in% c(2,3,4)) %>%
  mutate(year = as.numeric(VCF0004)) %>%
mutate(kids = case_when(
  VCF0138==0 ~ "No kids",
  VCF0138 %in% c(1,2,3) ~ "Kids")) %>% 
  mutate(marry = case_when(
           VCF0147 == 1 ~ "Married",
           VCF0147 %in% c(2,3,4,5) ~ "Not married")) %>%
  mutate(partyID = case_when(
    VCF0303 ==1 ~ "Democrat",
    VCF0303 ==3 ~ "Republican",
    VCF0303 ==2 ~ "Independent")) %>%
  select(year,kids,marry,partyID) %>% 
  filter(kids=="Kids") %>%  filter(!is.na(marry)) %>% 
  group_by(year,marry,partyID) %>% 
  summarise(wtn = survey_total()) %>%
  summarise(partyID, perc = wtn / sum(wtn)) 


## ANES 2000
anes2000TS <- read_dta("Documents/R/2022 Projects/anes2000TS.dta")
anes2000TS$year <- "2000"
anes2000 <- anes2000TS %>% as_survey_design(id = V000001, 
                                            weights = V000002)
anes00 <- anes2000 %>%
  mutate(year = as.numeric(year)) %>%
  filter(V000908>24) %>% filter(V000908<56) %>%
mutate(kids = case_when(
  V001024==0 ~ "No kids",
  V001024 %in% c(1,2,3,4,5,6,7,8,9,10,11) ~ "Kids")) %>% 
  mutate(marry = case_when(
    V000909 == 1 ~ "Married",
    V000909 %in% c(2,3,4,5,6) ~ "Not married")) %>%
  mutate(partyID = case_when(
    V000523 %in% c(0,1,2) ~ "Democrat",
    V000523 %in% c(4,5,6) ~ "Republican",
    V000523 %in% c(3) ~ "Independent")) %>%
  select(year,kids,marry,partyID) %>% 
  filter(kids=="Kids") %>%  filter(!is.na(marry)) %>% 
  group_by(year,marry,partyID) %>% 
  summarise(wtn = survey_total()) %>%
  summarise(partyID, perc = wtn / sum(wtn)) 

## ANES 2002
anes2002TS <- read_dta("Documents/R/2022 Projects/anes2002TS.dta")
anes2002TS$year <- "2002"
anes2002 <- anes2002TS %>% as_survey_design(id = V020001, 
                                            weights = V020101)
anes02 <- anes2002 %>%
  mutate(year = as.numeric(year)) %>%
  filter(V023126x>24) %>% filter(V023126x<56) %>%
  mutate(kids = case_when(
    V021107==0 ~ "No kids",
    V021107 %in% c(1,2,3,4,5,6) ~ "Kids")) %>% 
  mutate(marry = case_when(
    V023127 == 1 ~ "Married",
    V023127 %in% c(2,3,4,5,6) ~ "Not married")) %>%
  mutate(partyID = case_when(
    V023038x %in% c(0,1,2) ~ "Democrat",
    V023038x %in% c(4,5,6) ~ "Republican",
    V023038x %in% c(3) ~ "Independent")) %>%
  select(year,kids,marry,partyID) %>% 
  filter(kids=="Kids") %>%  filter(!is.na(marry)) %>% 
  group_by(year,marry,partyID) %>% 
  summarise(wtn = survey_total()) %>%
  summarise(partyID, perc = wtn / sum(wtn)) 

##ANES 2004
anes2004TS <- read_dta("Documents/R/2022 Projects/anes2004TS.dta")
anes2004TS$year <- "2004"
anes2004 <- anes2004TS %>% as_survey_design(id = V040001, 
                                            weights = V040101)
anes04 <- anes2004 %>%
  mutate(year = as.numeric(year)) %>%
  filter(V043250>24) %>% filter(V043250<56) %>%
  mutate(kids = case_when(
    V041103==0 ~ "No kids",
    V041103 %in% c(1,2,3,4,5,6,7) ~ "Kids")) %>% 
  mutate(marry = case_when(
    V043251 == 1 ~ "Married",
    V043251 %in% c(2,3,4,5,6) ~ "Not married")) %>%
  mutate(partyID = case_when(
    V043116 %in% c(0,1,2) ~ "Democrat",
    V043116 %in% c(4,5,6) ~ "Republican",
    V043116 %in% c(3) ~ "Independent")) %>%
  select(year,kids,marry,partyID) %>% 
  filter(kids=="Kids") %>%  filter(!is.na(marry)) %>% 
  group_by(year,marry,partyID) %>% 
  summarise(wtn = survey_total()) %>%
  summarise(partyID, perc = wtn / sum(wtn)) 


## ANES 2008
anes2008TS <- read_dta("Documents/R/2022 Projects/anes_timeseries_2008.dta")
anes2008TS$year <- "2008"
anes2008 <- anes2008TS %>% as_survey_design(id = V080001, 
                                            weights = V080101)
anes08 <- anes2008 %>%
  mutate(year = as.numeric(year)) %>%
  filter(V083215x>24) %>% filter(V083215x<56) %>%
  mutate(kids = case_when(
    V081109==0 ~ "No kids",
    V081109 %in% c(1,2,3,4,5,6,7) ~ "Kids")) %>% 
  mutate(marry = case_when(
    V083216x == 1 ~ "Married",
    V083216x %in% c(2,3,4,5,6) ~ "Not married")) %>%
  mutate(partyID = case_when(
    V083098x %in% c(0,1,2) ~ "Democrat",
    V083098x %in% c(4,5,6) ~ "Republican",
    V083098x %in% c(3) ~ "Independent")) %>%
  select(year,kids,marry,partyID) %>% 
  filter(kids=="Kids") %>%  filter(!is.na(marry)) %>% 
  group_by(year,marry,partyID) %>% 
  summarise(wtn = survey_total()) %>%
  summarise(partyID, perc = wtn / sum(wtn)) 


#ANES 2012
anes2012TS <- read_dta("Documents/R/2022 Projects/anes_timeseries_2012_Stata12.dta")
anes2012TS$year <- "2012"
anes2012 <- anes2012TS %>% as_survey_design(id = caseid, 
                                             weights = weight_full, 
                                             strata = strata_full,
                                             psu = psu_full)
anes12 <- anes2012 %>%
  mutate(year = as.numeric(year)) %>%
  filter(dem_age_r_x>24) %>% filter(dem_age_r_x<56) %>%
  mutate(kids = case_when(
    dem2_numchild==0 ~ "No kids",
    dem2_numchild %in% c(1,2,3) ~ "Kids")) %>% 
  mutate(marry = case_when(
    dem_marital %in% c(1,2) ~ "Married",
    dem_marital %in% c(3,4,5,6) ~ "Not married")) %>%
  mutate(partyID = case_when(
    pid_x %in% c(1,2,3) ~ "Democrat",
    pid_x %in% c(5,6,7) ~ "Republican",
    pid_x %in% c(4) ~ "Independent")) %>%
  select(year,kids,marry,partyID) %>% 
  filter(kids=="Kids") %>%  filter(!is.na(marry)) %>% 
  group_by(year,marry,partyID) %>% 
  summarise(wtn = survey_total()) %>%
  summarise(partyID, perc = wtn / sum(wtn)) 


## ANES 2016
anes2016TS <- read_dta("Documents/R/2022 Projects/anes_timeseries_2016.dta")
anes2016TS$year <- "2016"
anes2016_ <- anes2016TS %>% as_survey_design(id = V160001, 
                                            weights = V160101, 
                                            strata = V160201,
                                            psu = V160202)
anes16 <- anes2016_ %>%
  mutate(year = as.numeric(year)) %>%
  filter(V161267>24) %>% filter(V161267<56) %>%
  mutate(kids = case_when(
    V161324==0 ~ "No kids",
    V161324 %in% c(1,2,3,4,5,6,7,8,9) ~ "Kids")) %>% 
  mutate(marry = case_when(
    V161268 %in% c(1,2) ~ "Married",
    V161268 %in% c(3,4,5,6) ~ "Not married")) %>%
  mutate(partyID = case_when(
    V161158x %in% c(1,2,3) ~ "Democrat",
    V161158x %in% c(5,6,7) ~ "Republican",
    V161158x %in% c(4) ~ "Independent")) %>%
  select(year,kids,marry,partyID) %>% 
  filter(kids=="Kids") %>%  filter(!is.na(marry)) %>% 
  group_by(year,marry,partyID) %>% 
  summarise(wtn = survey_total()) %>%
  summarise(partyID, perc = wtn / sum(wtn)) 

    
## ANES 2020
anes2020TS <- read_dta("Documents/R/2022 Projects/anes_timeseries_2020_stata_20220210.dta")
anes2020TS$year <- "2020"
anes2020 <- anes2020TS %>% as_survey_design(id = V200001, 
                                            weights = V200010a, 
                                            strata = V200010d,
                                            psu = V200010c)
anes20 <- anes2020 %>%
  mutate(year = as.numeric(year)) %>%
  filter(V201507x>24) %>% filter(V201507x<56) %>%
  mutate(kids = case_when(
    V201567==0 ~ "No kids",
    V201567 %in% c(1,2,3,4) ~ "Kids")) %>% 
  mutate(marry = case_when(
    V201508 %in% c(1,2) ~ "Married",
    V201508 %in% c(3,4,5,6) ~ "Not married")) %>%
  mutate(partyID = case_when(
    V201231x %in% c(1,2,3) ~ "Democrat",
    V201231x %in% c(5,6,7) ~ "Republican",
    V201231x %in% c(4) ~ "Independent")) %>%
  select(year,kids,marry,partyID) %>% 
  filter(kids=="Kids") %>%  filter(!is.na(marry)) %>% 
  group_by(year, marry, partyID) %>% 
  summarise(wtn = survey_total()) %>%
  summarise(partyID, perc = wtn / sum(wtn)) 


#MERGE
anesunite <- rbind(ANESmarry, anes00, anes02, anes04, anes08, anes12, anes16, anes20)


#GRAPH

anesunite %>% filter(!is.na(partyID)) %>% 
  ggplot(aes(x=year,y=as.numeric(perc),color=partyID)) + 
  geom_point(alpha=.5) + facet_wrap(~marry) + geom_smooth(size=1.25, method="loess", se=F) + 
  labs(title="Self-described partisan ID among parents (1958-2020)", y="Share", x=" ",
       subtitle="Among voters age 25-54 with children at home, by marital status", 
       color="Party self-ID\n(Including leaners)",
       caption="Source: American National Election Survey. Not married includes never married, divorced, separated, widowed.") +
  theme_bw() +
  scale_y_continuous(labels=scales::percent_format()) + 
  scale_color_manual(values=colorcode) +
  theme(text=element_text(family="Bahnschrift"), legend.position="bottom")  




#CSV FILES
write.table(ANESmarry, file = "~/Documents/R/2022 Projects/anes_timeseries.txt", sep = ",", quote = FALSE, row.names = F)
write.table(anes00, file = "~/Documents/R/2022 Projects/anes_2000.txt", sep = ",", quote = FALSE, row.names = F)
write.table(anes02, file = "~/Documents/R/2022 Projects/anes_2002.txt", sep = ",", quote = FALSE, row.names = F)
write.table(anes04, file = "~/Documents/R/2022 Projects/anes_2004.txt", sep = ",", quote = FALSE, row.names = F)
write.table(anes08, file = "~/Documents/R/2022 Projects/anes_2008.txt", sep = ",", quote = FALSE, row.names = F)
write.table(anes12, file = "~/Documents/R/2022 Projects/anes_2012.txt", sep = ",", quote = FALSE, row.names = F)
write.table(anes16, file = "~/Documents/R/2022 Projects/anes_2016.txt", sep = ",", quote = FALSE, row.names = F)
write.table(anes20, file = "~/Documents/R/2022 Projects/anes_2020.txt", sep = ",", quote = FALSE, row.names = F)




