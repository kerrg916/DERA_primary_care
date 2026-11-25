# outcomes: GPPS items
library(tidyverse)
library(glmmTMB)
set.seed(15072026)

blk<-read_csv("gp_level.csv")

# GPPS data setup
gpps <- read_csv("linked data/GPPS/GPPS_2026_Practice_data_(weighted)_(csv)_PUBLIC.csv") %>%
  select(ad_practicecode, ad_pcncode, ad_pcnname, popsize,
         localgpservicesphone.pcteval, localgpserviceswebsite.pcteval,
         localgpservicesapp.pcteval,
         localgpservicesused_1.pct, localgpservicesused_2.pct, localgpservicesused_8.pct,
         starts_with("gpcontacthow"), 
         starts_with("localgpservicesused"), 
         -ends_with(".count"),gpcontactoverall.pcteval,
         lastgpappttype.pcteval,
         overallexp.pcteval,
         dv_ethnicityband_1.pct,
         received,
         localgpservicesprefhpsee.pcteval, 
         -ends_with("w")) %>%
  rename(GP_CODE=ad_practicecode,
         PCN_CODE=ad_pcncode, PCN_NAME=ad_pcnname,
         ease_phone = localgpservicesphone.pcteval, # fairly/very easy to contact GP using the phone
         ease_website=localgpserviceswebsite.pcteval, # fairly/very easy to contact GP using a website
         ease_app=localgpservicesapp.pcteval, # fairly/very easy to contact GP using NHS app
         useonline_book=localgpservicesused_1.pct, # used online services in last 12 months for appt booking
         useonline_form=localgpservicesused_2.pct, # used online services in last 12 months for filling in an online form
         useonline_pres=localgpservicesused_3.pct, # used online for ordering repeat prescriptions
         useonline_rec=localgpservicesused_4.pct, # used online for accessing medical records
         useonline_reg=localgpservicesused_5.pct, # used online for registering with a GP
         useonline_test=localgpservicesused_6.pct, # used online for test results
         useonline_admin=localgpservicesused_7.pct, # used online for admin request
         useonline_none=localgpservicesused_8.pct, # have not used online services in the last 12 months
         contact_phone=gpcontacthow_1.pct, # last contacted GP on phone
         contact_inperson=gpcontacthow_2.pct, # last contact gp in person visit
         contact_app=gpcontacthow_4.pct, # last contacted gp using the nhs app
         contact_exp_good=gpcontactoverall.pcteval, # last GP contact overall fairly/very good
         appt_remote=lastgpappttype.pcteval, # last gp appointment was remote (phone, video, text, online msg)
         experience=overallexp.pcteval, # overall gp experience good or very good
         preferred_hcp=localgpservicesprefhpsee.pcteval, # % (almost) always seeg their preferred HCP
         white=dv_ethnicityband_1.pct) %>% # % white
  mutate(contact_website=gpcontacthow_3.pct+gpcontacthow_5.pct) %>%
  dplyr::select(-c(gpcontacthow_3.pct,gpcontacthow_5.pct, gpcontacthow_6.pct))
gpps$white[gpps$white< -01] <-NA

blk<-blk %>%left_join(gpps) %>%
  filter(PCN_NAME !="GP AT HAND PCN")

# dataset for models 
dat<-blk%>%filter(!is.na(white), !is.na(GP))
dat$ICB_NAME<-str_replace(dat$ICB_NAME, "INTEGRATED CARE BOARD", "")
dat$ICB_NAME<-str_replace(dat$ICB_NAME, "NHS ", "")

dat$DERA_c <- scale(dat$DERA_SCORE, scale = FALSE) # centred
dat$vc_perc_c <- scale(dat$vc_perc, scale = FALSE) # centred
dat$urban_z <- scale(dat$urban_score)
dat$GP_z <- scale(dat$GP)
dat$female_z <- scale(dat$female)
dat$white_z <- scale(dat$white)

#### Run models: loop over outcomes ####
n=1
empty<-list()
preds<-list()
me<-list()

cols<-colnames(gpps[5:ncol(gpps)])[-c(18:19)]

for(a in cols) {
  
  dat[[a]][dat[[a]]< -1]<-NA
  dat$y<-dat[[a]]
  
  dat$y <- (dat$y * (nrow(dat) - 1) + 0.5) / nrow(dat)
  mod<-glmmTMB(y ~ vc_perc_c+DERA_c+urban_z+female_z+white_z+GP_z+ log(patients) +
                 (DERA_c|ICB_NAME),
               family = beta_family(link = "logit"),
               weights=received,
               data = dat)
  
  
  out<-broom.mixed::tidy(mod) %>%
    filter(effect=="fixed", term!="(Intercept)") %>%
    select(term, estimate, std.error, p.value)
  out$var<-a
  
  bb<- marginaleffects::avg_slopes(mod) %>%
    mutate(var=a)
  
  empty[[n]] <-out
  me[[n]] <- bb
  
  n=n+1
  rm(out, mod, newd, bb)
  
}
tog2<-do.call("rbind", empty) %>%
  mutate(lwr=exp(estimate-1.96*std.error),upr=exp(estimate+1.96*std.error),
         estimate=exp(estimate))
tog2$sig<-ifelse(tog2$p.value<=0.05, 1,0)

marg<-do.call("rbind", me) %>%
  mutate(sig=ifelse(p.value<=0.05, 1, 0))

write_csv(tog2, "results/gpps/both-reg-icbRS-deraRS.csv")
write_csv(marg, "results/gpps/both-reg-marginal-icbRS-deraRS.csv")

