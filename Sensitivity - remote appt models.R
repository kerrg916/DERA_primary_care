# sensitivity analysis: remote appts to include only video or online
library(tidyverse)
set.seed(15072026)


blk<-read_csv("gp_level.csv")

# get white % from GPPS dataset
gpps <- read_csv("GPPS_2026_Practice_data_(weighted)_(csv)_PUBLIC.csv") %>%
  select(ad_practicecode, ad_pcncode, ad_pcnname, popsize,
         dv_ethnicityband_1.pct,
         -ends_with("w")) %>%
  rename(GP_CODE=ad_practicecode,
         PCN_CODE=ad_pcncode, PCN_NAME=ad_pcnname,
         white=dv_ethnicityband_1.pct) # % white
gpps$white[gpps$white< -01] <-NA

blk<-blk %>%left_join(gpps) %>%
  filter(PCN_NAME !="GP AT HAND PCN")%>%
  mutate(GP_per1000=GP/patients*1000,
         remote2=`Video Conference/Online`)

#### random slope model ####
library(marginaleffects)
library(glmmTMB)

dat<-blk %>%
  filter(!is.na(GP), !is.na(white)) # n = 5965
dat$ICB_NAME<-str_replace(dat$ICB_NAME, "INTEGRATED CARE BOARD", "")
dat$ICB_NAME<-str_replace(dat$ICB_NAME, "NHS ", "")

dat$urban_z <- scale(dat$urban_score)
dat$GP_z <- scale(dat$GP)
dat$female_z <- scale(dat$female)
dat$white_z <- scale(dat$white)

n=0
av<-list()
empty<-list()
varc<-list()
inc<-list()
predsa<-list()
predsi<-list()
rf<-list()

cols<-c("DERA_SCORE", "d1", "d2", "d3", "d4")
for(a in cols) {
  
  print(a)
  n=n+1
  print(n)
  
  dat$x<-dat[[a]]
  dat$DERA_c <- scale(dat$x, scale = FALSE) # centred
  
  mod<-glmmTMB(cbind(remote2, appts - remote2) ~ DERA_c+urban_z+female_z+white_z+GP_z+
               (1|ICB_NAME),
             family = binomial,
             data = dat)
  mod2<-glmmTMB(cbind(remote2, appts - remote2) ~ DERA_c+urban_z+female_z+white_z+GP_z+
                (DERA_c|ICB_NAME),
              family = binomial,
              data = dat)
  test<-data.frame(anova(mod2, mod, test="Chisq")) 
  test$dera<-a

  vc<-(VarCorr(mod2)$cond$ICB_NAME) 
  var <- data.frame(
    effect = c("Intercept", "DERA_c"),
    SD = attr(vc, "stddev"),
    variance = attr(vc, "stddev")^2)
  var$corr_with_intercept <- c(1,attr(vc, "correlation")[2,1])
  # Wald CI estimates initially on log scale; look at first two thetas
  theta <-  as.data.frame(summary(TMB::sdreport(mod2$obj)))
  theta<-theta[grepl("^theta", rownames(theta)), ]
  theta$lwr<-exp(theta$Estimate-1.96*theta$`Std. Error`)
  theta$upr<-exp(theta$Estimate+1.96*theta$`Std. Error`)
  var$lwr<-theta$lwr[1:2]
  var$upr<-theta$upr[1:2]
  var$dera<-a
  
  inc10<-data.frame(inc10=exp(10 * fixef(mod2)$cond["DERA_c"])) 
  
  out<-broom.mixed::tidy(mod2) %>%
    filter(effect=="fixed", term!="(Intercept)") %>%select(-c(effect, group, statistic)) %>%
    mutate(dera=a)
  
  av[[n]] <- test
  empty[[n]]<- out
  varc[[n]]<-var
  inc[[n]]<-inc10
  
  rm(test, var, inc10, out, mod)
  
  # marginal effects : pop-level fixed effect of DERA
  
  newdat <- datagrid(
    model = mod2,
    score = seq(
      min(10, na.rm = TRUE),
      max(70, na.rm = TRUE),
      length.out = 100
    ),
    ICB_NAME = unique(dat$ICB_NAME),
    urban_score = mean(dat$urban_score, na.rm = TRUE),
    female = mean(dat$female, na.rm = TRUE),
    white = mean(dat$white, na.rm = TRUE),
    patients=mean(dat$patients),
    GP=mean(dat$GP, na.rm=T)
  ) %>%
    mutate( DERA_c= scale(score, scale = FALSE),
            female_z= scale(female, scale = FALSE),
            white_z= scale(white, scale = FALSE),
            urban_z= scale(urban_score, scale = FALSE),
            GP_z= scale(GP, scale = FALSE))
  # dera_mean <- mean(dat$y, na.rm = TRUE)
  pred <- data.frame(predictions(
    mod2,
    newdata = newdat,
    type = "response",
    re.form = NA # remove ICB-specific deviations (get pop fixed effect relationship)
  ) )%>%
    mutate(dera=a)
  
  p1<-ggplot(pred,
             aes(x = score,
                 y = estimate)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low,
                    ymax = conf.high),
                alpha = 0.2) +
    theme_bw() +
    scale_y_continuous(limits=c(0,1))
  
  
  
  # marginal effects: average over estimated ICB-specific slopes
  
  pred_icb <- data.frame(predictions(
    model = mod2,
    newdata = newdat,
    type = "response",
    re.form = NULL
  ) )%>%
    mutate(dera=a)
  p2<-ggplot(pred_icb, aes(x = score, y = estimate, colour=ICB_NAME, fill=ICB_NAME)) +
    geom_line(size=1) +
    # geom_ribbon(aes(ymin = conf.low, ymax = conf.high),alpha = 0.1  ) +
    theme_bw() +
    theme(legend.position="none")+
    scale_y_continuous(limits=c(0,1))
  
  
  
  predsa[[n]] <- pred
  predsi[[n]] <- pred_icb
  
  print(p1)
  print(p2)
  
  # random effects
  re <- as.data.frame(ranef(mod2)$cond$ICB_NAME)
  re$ICB_NAME <- rownames(re)
  fixed_slope <- fixef(mod2)$cond["DERA_c"]
  re$DERA_total_slope <- fixed_slope + re$DERA_c
  re$OR_10point_DERA <- exp(10 * re$DERA_total_slope)
  re$var <- a
  rf[[n]] <- re
  
  rm(mod2, pred_icb, pred, p1, p2, re)
  
}

# save out
tests<-do.call("rbind", av)
write_csv(tests, "results/vid_online/anovas.csv")

coefs<-do.call("rbind", empty) %>%
  mutate(lwr=estimate-1.96*std.error,
         upr=estimate+1.96*std.error,
         OR=exp(estimate), OR.lwr=exp(lwr), OR.upr=exp(upr),
         OR10=exp(10*estimate), OR10.lwr=exp(10*lwr), OR10.upr=exp(10*upr))
write_csv(coefs, "results/vid_online/coefs.csv")

vars<-do.call("rbind", varc)
write_csv(vars, "results/vid_online/varcov.csv")

inc10<-do.call("rbind", inc)
write_csv(inc10, "results/vid_online/inc10.csv")

average<-do.call("rbind", predsa)
write_csv(average, "results/vid_online/preds-average.csv")

icb<-do.call("rbind", predsi)
write_csv(icb, "results/vid_online/preds-icb.csv")

ranef<-do.call("rbind", rf)
write_csv(ranef, "results/vid_online/ranef.csv")

average %>%
  ggplot(aes(score, estimate, colour=dera)) +
  geom_line()
icb %>%
  ggplot(aes(score, estimate, colour=dera)) +
  geom_line()+
  facet_wrap(~ICB_NAME )
rm(av, empty, inc, predsa, predsi)
