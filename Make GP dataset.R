
library(tidyverse)

# DERA scores and components
# lower = less at risk; higher = more at risk
# deciles: 1 = most at risk, 10 = least at risk
dera<-read_csv("DERA_07062026.csv") %>%
  rename(d1=DOM1_STD_SCORE, # digital access and capability
         d2=DOM2_STD_SCORE, # affordability and economic pressure
         d3=DOM3_STD_SCORE, # ability and skills
         d4=DOM4_STD_SCORE) # agency, motivation, trust (includes age structure)

# list size of GPs - all ages/sexes
list<-read_csv("listsize_2025.csv") %>%
  dplyr::select(yearmonth, GP_CODE, NUMBER_OF_PATIENTS) 

# workforce
work<-read_csv("linked data/GPworkforce/work25.csv")%>%
  rename(GP_CODE=PRAC_CODE) %>%
  select(-filename)

# attended primary care appts in 2025 by modality - compiled from monthly datasets
appts<-read_csv("appts 2024-2025/NHSD-appts-2025-Attended_prac_level.csv") %>%
  group_by(GP_CODE, APPT_MODE, yearmonth) %>%
  summarise(appts=sum(COUNT_OF_APPOINTMENTS)) %>%
  pivot_wider(names_from=APPT_MODE, values_from=appts) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(appts=`Face-to-Face`+`Home Visit`+Telephone+Unknown+`Video Conference/Online`,
         remote=Telephone+`Video Conference/Online`)

# online consultations data: compiled from monthly NHSD datasets
# used to obtain ICB name and code (pre ICB groupings in late 2025)
ocs1<-read_csv("OCs_Jan24-Dec25.csv") 
look<-ocs1 %>%
  distinct(ICB_CODE, ICB_NAME)
icb<-ocs1 %>% 
  distinct(GP_CODE, ICB_CODE) %>%left_join(look)
ocs1<-ocs1%>%
  filter(MONTH>="2025-01-01", 
         SUPPLIER!="no data") %>% # should be excluded based NHSD on documentation
  rename(yearmonth=MONTH) %>%
  select(yearmonth, GP_CODE, OC_TOTAL_SUBMISSIONS, ICB_CODE, ICB_NAME, REGION_CODE, REGION_NAME)
tab<-ocs1%>%count(GP_CODE, yearmonth) %>%count(GP_CODE)
tab %>%filter(n<12) %>% # 12 months of data
  nrow() # 1209
tab<-tab %>%filter(n==12) # select only those with a complete year of data

# GP prac info and postcode lookup
info<-read_csv("prac info/epraccur.csv") %>%
  dplyr::select(ODS_CODE, POSTCODE, ICB_CODE) %>%
  rename(Postcode=POSTCODE, GP_CODE=ODS_CODE)
pcd1<-read_csv("pcd to lsoa21/NSPL_MAY_2025_UK.csv") 
pcd<-pcd1%>%
  dplyr::select(pcds, lsoa21) %>%
  rename(Postcode=pcds, LSOA21CD=lsoa21)

# lsoa list size data
ll <- read_csv("pats_LSOA_2025.csv") %>%
  rename(yearmonth=EXTRACT_DATE, GP_CODE=PRACTICE_CODE )

# female patients, based on list size data
female<-read_csv("listsize_female_2025.csv")

#### icb and pcn info ####
# requires cleaning
pcn<-read_csv("epcncorepartnerdetails.csv") 
pcn$PracticeToPCNStartDate<-as.Date(as.character(pcn$PracticeToPCNStartDate), format=c("%Y%m%d"))
pcn$PracticeToPCNEndDate<-as.Date(as.character(pcn$PracticeToPCNEndDate), format=c("%Y%m%d"))

pcn$current<-NA
pcn$current[year(pcn$PracticeToPCNStartDate)<=2025]<-1
pcn$current[year(pcn$PracticeToPCNEndDate)<2025]<-0
pcn$current[year(pcn$PracticeToPCNStartDate)>2025]<-0
pcn$duration <-as.numeric(as.Date("2025-12-31")-pcn$PracticeToPCNStartDate)

pcn<-pcn%>%filter(current==1) %>%
  distinct(GP_CODE, ICB_CODE, ICB_NAME, PCN_CODE, PCN_NAME, duration) %>%
  group_by(GP_CODE, ICB_CODE, ICB_NAME) %>%
  slice_max(order_by = duration, n = 1, with_ties = FALSE) %>%
  ungroup()
nrow(pcn) ==length(unique(pcn$GP_CODE))

pcn<-list[2] %>%
  left_join(pcn) %>%
  left_join(icb, by="GP_CODE")
pcn$ICB_NAME<-pcn$ICB_NAME.y
pcn$ICB_NAME[is.na(pcn$ICB_NAME)]<-pcn$ICB_NAME.x[is.na(pcn$ICB_NAME)]
pcn<-pcn%>%
  distinct(GP_CODE, PCN_CODE, PCN_NAME, ICB_NAME)
pcn$ICB_NAME<- sub(" -.*$", "", pcn$ICB_NAME) # .*  = everything after ' -' 
# until the end of the string $
pcn$ICB_NAME<-str_replace(pcn$ICB_NAME, "ICB", "INTEGRATED CARE BOARD")
pcn<-pcn %>%
  left_join(look) %>%
  distinct(GP_CODE, PCN_CODE, PCN_NAME, ICB_CODE, ICB_NAME)%>%
  group_by(GP_CODE, PCN_CODE, PCN_NAME) %>%
  slice(1)
pcn$ICB_CODE[pcn$ICB_NAME=="NHS SUSSEX INTEGRATED CARE BOARD"]<-"QNX"
pcn$ICB_CODE[pcn$ICB_NAME=="NHS SURREY HEARTLANDS INTEGRATED CARE BOARD"]<-"QXU"
length(unique(pcn$ICB_NAME))==43 # 42 + NA
a<-pcn%>%filter(is.na(ICB_CODE)) # OK to exclude (LOW LIST SIZE / INACTIVE)
B<-list%>%filter(GP_CODE %in% a$GP_CODE) %>%group_by(GP_CODE)%>%summarise(pats=mean(NUMBER_OF_PATIENTS))

pcn$ICB_NAME[pcn$GP_CODE=="Y05788"] <-"NHS LANCASHIRE AND SOUTH CUMBRIA INTEGRATED CARE BOARD"
pcn$ICB_CODE[pcn$GP_CODE=="Y05788"] <-"QE1"
pcn$ICB_NAME[pcn$GP_CODE=="Y07377"] <-"NHS HUMBER AND NORTH YORKSHIRE INTEGRATED CARE BOARD"
pcn$ICB_CODE[pcn$GP_CODE=="Y07377"] <-"QOQ"
pcn$ICB_NAME[pcn$GP_CODE=="Y05788"] <-"NHS LANCASHIRE AND SOUTH CUMBRIA INTEGRATED CARE BOARD"
pcn$ICB_CODE[pcn$GP_CODE=="Y05788"] <-"QE1"
pcn$ICB_NAME[pcn$GP_CODE=="Y00049"] <-"NHS CORNWALL AND THE ISLES OF SCILLY INTEGRATED CARE BOARD"
pcn$ICB_CODE[pcn$GP_CODE=="Y00049"] <-"QT6"

rm(look, icb, a, B)

#### make base dataset ####
prac<-list %>%
  left_join(info[1:2]) %>%
  left_join(pcd) %>%
  left_join(ocs1[1:3], by=join_by("yearmonth", "GP_CODE")) %>%
  left_join(appts, by=join_by("yearmonth", "GP_CODE")) %>%
  left_join(work, by=c("GP_CODE","yearmonth")) %>%
  left_join(female, by=c("GP_CODE","yearmonth")) %>%
  mutate(across(6:13, ~replace_na(.x, 0))) %>%
  left_join(pcn)

prac%>%filter(is.na(PCN_CODE)) %>%count(GP_CODE) %>%nrow() # 77 practices with no PCN 

# select for England
eng<-ll %>%
  filter(grepl("^E", LSOA_CODE), LSOA_CODE!="EMPTY")  %>%
  distinct(LSOA_CODE)

nrow(eng) == nrow(dera) # no missingness

#### practice-level DERA score ####
# Get GP-level DERA using LSOA list size numbers 
grouped<-ll %>%
  filter(LSOA_CODE%in%eng$LSOA_CODE) %>%
  rename(LSOA21CD=LSOA_CODE) %>%
  left_join(dera) %>%
  left_join(d4) %>%
  group_by(GP_CODE, LSOA21CD) %>%
  summarise(patients=mean(NUMBER_OF_PATIENTS, na.rm=T), 
            DERA_SCORE=mean(DERA_SCORE, na.rm=T),
            d1=mean(d1),
            d2=mean(d2),
            d3=mean(d3),
            d4=mean(d4))

gc()

total<-grouped %>%
  group_by(GP_CODE) %>%
  summarise(totpat=sum(patients))

grouped<-grouped %>%
  left_join(total) %>%
  mutate(wt=patients/totpat)

dera.gp<-grouped %>%
  mutate(score=wt*DERA_SCORE,
         d1=wt*d1, d2=wt*d2, d3=wt*d3, d4=wt*d4, 
         d4_v1=wt*d4_v1, d4_v2=wt*d4_v2, d4_v3=wt*d4_v3) %>%
  group_by(GP_CODE) %>%
  summarise(DERA_SCORE=sum(score, na.rm=T),
            d1=sum(d1, na.rm=T),
            d2=sum(d2, na.rm=T),
            d3=sum(d3, na.rm=T),
            d4=sum(d4, na.rm=T))

rm(total, grouped)

#### practice-level DERA quintile ####
# Get GP-level DERA using LSOA list size numbers 
grouped<-ll %>%
  filter(LSOA_CODE%in%eng$LSOA_CODE) %>%
  rename(LSOA21CD=LSOA_CODE) %>%
  group_by(GP_CODE, LSOA21CD) %>%
  summarise(patients=mean(NUMBER_OF_PATIENTS))

quantile(dera$DERA_SCORE)
dera%>% # 1 = most at risk
  group_by(DERA_NTL_DECILE) %>%
  summarise(min=min(DERA_SCORE))
q1<-dera%>%filter(DERA_NTL_DECILE == 1 | DERA_NTL_DECILE   ==2) 
q2<-dera%>%filter(DERA_NTL_DECILE == 3 | DERA_NTL_DECILE   ==4)
q3<-dera%>%filter(DERA_NTL_DECILE == 5 | DERA_NTL_DECILE   ==6)
q4<-dera%>%filter(DERA_NTL_DECILE == 7 | DERA_NTL_DECILE   ==8)
q5<-dera%>%filter(DERA_NTL_DECILE == 9 | DERA_NTL_DECILE   ==10)

grouped$dera_quintile<-NA
grouped$dera_quintile[grouped$LSOA21CD%in% q1$LSOA21CD]<-1
grouped$dera_quintile[grouped$LSOA21CD%in% q2$LSOA21CD]<-2
grouped$dera_quintile[grouped$LSOA21CD%in% q3$LSOA21CD]<-3
grouped$dera_quintile[grouped$LSOA21CD%in% q4$LSOA21CD]<-4
grouped$dera_quintile[grouped$LSOA21CD%in% q5$LSOA21CD]<-5
table(grouped$dera_quintile, exclude=F)

gc()

total<-grouped %>%
  group_by(GP_CODE) %>%
  summarise(totpat=sum(patients))

grouped<-grouped %>%
  left_join(total) %>%
  mutate(wt=patients/totpat)

deraq.gp<-grouped %>%
  mutate(one=1) %>%
  pivot_wider(names_from=dera_quintile, values_from=one) %>%
  mutate(`1`=wt*`1`,
         `2`=wt*`2`,
         `3`=wt*`3`,
         `4`=wt*`4`,
         `5`=wt*`5`) %>%
  group_by(GP_CODE) %>%
  summarise(dera.q1=sum(`1`, na.rm=T),
            dera.q2=sum(`2`, na.rm=T),
            dera.q3=sum(`3`, na.rm=T),
            dera.q4=sum(`4`, na.rm=T),
            dera.q5=sum(`5`, na.rm=T)) # there's no NAs in these

rm(q1, q2, q3, q4, q5, total, grouped)

#### practice-level urban/rural ####
# Get GP-level urban% using LSOA list size numbers & urban/rural status
ur<-read_csv("Rural_Urban_Classification_(2021)_of_LSOAs_in_EW.csv")
grouped<-ll %>%
  filter(LSOA_CODE%in%eng$LSOA_CODE) %>%
  rename(LSOA21CD=LSOA_CODE) %>%
  left_join(ur) 

grouped$urban_score<-ifelse(grouped$Urban_rural_flag =="Urban", 1, 0) # no missingness in this
grouped<-grouped%>%
  group_by(GP_CODE, LSOA21CD) %>%
  summarise(patients=mean(NUMBER_OF_PATIENTS, na.rm=T), 
            urban_score=mean(urban_score, na.rm=T))

gc()

total<-grouped %>%
  group_by(GP_CODE) %>%
  summarise(totpat=sum(patients))

grouped<-grouped %>%
  left_join(total) %>%
  mutate(wt=patients/totpat)

urban.gp<-grouped %>%
  mutate(score=wt*urban_score) %>%
  group_by(GP_CODE) %>%
  summarise(urban_score=sum(score, na.rm=T))

rm(total, grouped)

#### make practice-level dataset ####

# check list size data is consistent over the months
prac %>%
  filter(!is.na(appts), NUMBER_OF_PATIENTS>=100) %>%
  ggplot(aes(yearmonth, GP/NUMBER_OF_PATIENTS)) +
  geom_point(alpha=0.2)

# final dataset
blk<-prac%>%
  group_by(GP_CODE, ICB_CODE, ICB_NAME, PCN_CODE, PCN_NAME) %>%
  summarise(oc_total=sum(OC_TOTAL_SUBMISSIONS, na.rm=T), 
            patients=mean(NUMBER_OF_PATIENTS, na.rm=T ),
            female_patients=mean(female_patients, na.rm=T ),
            appts=sum(appts, na.rm=T), remote=sum(remote, na.rm=T), 
            `Video Conference/Online`=sum(`Video Conference/Online`, na.rm=T),
            `Admin/Non-Clinical`=mean(`Admin/Non-Clinical`, na.rm=T),
            `Direct Patient Care`=mean(`Direct Patient Care`, na.rm=T),
            GP=mean(GP, na.rm=T),
            Nurses=mean(Nurses, na.rm=T)
            ) %>%
  mutate(oc_rate=oc_total/patients*1000,
         appt_rate=appts/patients*1000,
         vc_perc=remote/appts,
         female=female_patients/patients) 

# check exclusions
n<-nrow(blk)
blk%>%filter(appts==0) %>% nrow() # 56
blk%>%filter(patients<100) %>% nrow() # 46

# apply exclusions
blk <- blk %>%
  filter(appts>0, patients>=100) %>%
  left_join(dera.gp) %>%
  left_join(deraq.gp) %>%
  left_join(urban.gp)
n-nrow(blk) # 59

length(unique(blk$GP_CODE))==nrow(blk) # 1 row per

# flag to identify if in OC dataset 
blk$in_oc<-ifelse(blk$GP_CODE %in% tab$GP_CODE, 1, 0)

##### save out  #####
write_csv(blk, "gp_level.csv")
