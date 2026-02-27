
library(tidyverse)
library(estimatr)
library(sf) 
library(MASS)
library(texreg)
select <- dplyr::select


dv.recode <- function(x){
  out <- case_when(x=='Not at all' ~ 0,
                   x=='Only a little' ~ 1,
                   x=='A moderate amount' ~ 2,
                   x=='A great deal' ~ 3)
  return(out)
}

dv.naming <- function(x){
  out <- case_when(x=='dv_personal' ~ "1. Personally",
                   x=='dv_community' ~ "2. Local\nCommunity",
                   x=='dv_us' ~ "3. United\nStates",
                   x=='dv_developing' ~ "4. Developing\nCountries",
                   x=='dv_future' ~ "5. Future\nGenerations")
  return(out)
}

policy.recode <- function(x){
  out <- case_when(x=='Strongly disagree' ~ 0,
                   x=='Disagree' ~ 1,
                   x=='Somewhat disagree' ~ 2,
                   x=='Neither agree nor disagree' ~ 3,
                   x=='Somewhat agree' ~ 4,
                   x=='Agree' ~ 5,
                   x=='Strongly agree' ~ 6)
  return(out)
}

policy.naming <- function(x){
  out <- case_when(x=='dv_policy1' ~ "1. Limit Development\nin Flood Zones",
                   x=='dv_policy2' ~ "2. Increase\nFlood Insurance",
                   x=='dv_policy3' ~ "3. Remove Structures\nfrom Flood Zones",
                   x=='dv_policy4' ~ "4. Build\nFlood Controls")
  return(out)
}
# Cleaning ----------------------------------------------------------------

data1 <- read_csv('./Data/SeaLevelRiseSurvey_California_January+16,+2021_13.54.csv') %>%
  mutate(geo = "CA") %>%
  .[-c(1,2),]
data2 <- read_csv('./Data/SeaLevelRiseSurvey_Florida_January+16,+2021_13.51.csv') %>%
  mutate(geo = "FL") %>%
  .[-c(1,2),]
data3 <- read_csv('./Data/SeaLevelRiseSurvey_NewJersey_January+16,+2021_13.50.csv') %>%
  mutate(geo = "NJ") %>%
  .[-c(1,2),]
data4 <- read_csv('./Data/SeaLevelRiseSurvey_Virginia_January+16,+2021_13.50.csv') %>%
  mutate(geo = "VA") %>%
  .[-c(1,2),]

dist1 <- read_csv('./Data/ca_dist_mins.csv')
dist2 <- read_csv('./Data/fl_dist_mins.csv')
dist3 <- read_csv('./Data/nj_dist_mins.csv')
dist4 <- read_csv('./Data/va_dist_mins.csv')

cw1 <- read_csv('./Data/ca_combined_final_withtraffic_sub.csv')
cw2 <- read_csv('./Data/fl_combined_final.csv')
cw3 <- read_csv('./Data/nj_combined_final.csv')
cw4 <- read_csv('./Data/va_combined_final.csv')

fld_zone <- read.csv('./Data/fld_zone.csv', row.names = F)

dist <- dist1 %>%
  bind_rows(dist2) %>%
  bind_rows(dist3) %>%
  bind_rows(dist4) %>%
  filter(!duplicated(.)) %>% 
  group_by(adds) %>%
  filter(dist_min == min(dist_min)) %>% 
  inner_join(cw1 %>%
               bind_rows(cw2) %>%
               bind_rows(cw3) %>%
               bind_rows(cw4) %>%
               select(adds, email, FID) %>%
               filter(!duplicated(.))) %>%
  rename(RecipientEmail=email) %>%
  inner_join(fld_zone %>% 
               select(adds, COUNTYFP, TRACTCE, FLD_ZONE) %>%
               filter(!duplicated(.))) 

data <- data1 %>%
  bind_rows(data2) %>%
  bind_rows(data3) %>%
  bind_rows(data4) %>%
  left_join(dist) %>%
  mutate(treat_map = !is.na(exp_emphasis),
         slr = str_detect(img, "SLR"),
         traf = as.numeric(traf),
         traf = ifelse(traf>250, 250, traf),
         treat_traf = !is.na(`Q88_Page Submit`),
         treat_traf = ifelse(geo=="CA", treat_traf, NA),
         dv_personal = dv.recode(slharmpersonal),
         dv_community = dv.recode(slharmbayarea),
         dv_us = dv.recode(slharmUS),
         dv_developing = dv.recode(slharmdeveloping),
         dv_future = dv.recode(slharmfuture),
         dv_personal_dk = ifelse(slharmpersonal=="Don't know", 1, 0),
         dv_community_dk = ifelse(slharmbayarea=="Don't know", 1, 0),
         dv_us_dk = ifelse(slharmUS=="Don't know", 1, 0),
         dv_developing_dk = ifelse(slharmdeveloping=="Don't know", 1, 0),
         dv_future_dk = ifelse(slharmfuture=="Don't know", 1, 0),
         race_asian = str_detect(race, 'Asian'),
         race_black = str_detect(race, 'African American'),
         race_hispanic = str_detect(race, 'Hispanic'),
         race_white = str_detect(race, 'Caucasian'),
         pid = case_when(party=="Democrat" ~ "D",
                         party=="Republican" ~ "R",
                         party %in% c("Independent", "Other Party") & partylean=='Closer to the Republican Party' ~ "R",
                         party %in% c("Independent", "Other Party") & partylean=='Closer to the Democratic Party' ~ "D",
                         party %in% c("Independent", "Other Party") ~ "I"),
         pid_num = case_when(party=="Democrat" ~ 1,
                             party=="Republican" ~ 5,
                             party %in% c("Independent", "Other Party") & partylean=='Closer to the Republican Party' ~ 4,
                             party %in% c("Independent", "Other Party") & partylean=='Closer to the Democratic Party' ~ 2,
                             party %in% c("Independent", "Other Party") ~ 3),
         WTP_money = as.numeric(WTP_money),
         wtp_dv = case_when(wtpstrength=='Strongly oppose' ~ 0,
                            wtpstrength=='Somewhat oppose' ~ 1,
                            wtpstrength=='Somewhat support' ~ 2,
                            wtpstrength=='Strongly support' ~ 3),
         dv_policy1 = policy.recode(Q740_1),
         dv_policy2 = policy.recode(Q740_2),
         dv_policy3 = policy.recode(Q740_3),
         dv_policy4 = policy.recode(Q740_4),
         belief_gq = case_when(linkGQ=='Definitely not' ~ 0,
                               linkGQ=='Probably not' ~ 1,
                               linkGQ=='Might or might not be' ~ 2,
                               linkGQ=='Probably yes' ~ 3,
                               linkGQ=='Definitely yes' ~ 4),
         social_talk = case_when(slneighbor=='Never' ~ 0,
                                 slneighbor=='Rarely' ~ 1,
                                 slneighbor=='Sometimes' ~ 2,
                                 slneighbor=='Often' ~ 4),
         flood_impact = case_when(SysRiskElse=="Not at all" ~ 0,
                                  SysRiskElse=="Only a little" ~ 1,
                                  SysRiskElse=="A moderate amount" ~ 2,
                                  SysRiskElse=="A great deal" ~ 3),
         climate_belief = case_when(gwhuman=='Caused mostly by human activities' ~ "Human Activity",
                                    T ~ "No GW/Natural Changes"),
         dist_min = ifelse(slr==T, dist_min*-1, dist_min),
         ideo_num = case_when(ideo=='Extremely conservative' ~ 7,
                              ideo=='Conservative' ~ 6,
                              ideo=='Slightly conservative' ~ 5,
                              ideo=='Moderate; middle of the road' ~ 4,
                              ideo=='Slightly liberal' ~ 3,
                              ideo=='Liberal' ~ 2,
                              ideo=='Extremely liberal' ~ 1))

# Map Experiment ----------------------------------------------------------

data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name, slr, value) %>%
  filter(!is.na(slr) & !is.na(value) & treat_map == 0) %>%
  summarise(count = n()) %>%
  group_by(name, slr) %>%
  mutate(pct = count/sum(count)) %>%
  mutate(slr = case_when(slr==FALSE  ~ "Outside",
                          slr==TRUE ~ "Inside"),
         name = dv.naming(name),
         value = case_when(value==0 ~  '1. Not at all',
                           value==1 ~ '2. Only a little',
                           value==2 ~ '3. A moderate amount',
                           value==3 ~ '4. A great deal')) %>%
  filter(!is.na(name)) %>%
  ggplot() + 
  geom_col(aes(x = slr, y = pct, fill = value)) + 
  facet_wrap(~name, nrow=1) + 
  labs(x = 'of Sea Level Rise Zone', y = 'Proportion of Respodents\nwith Concern', fill = 'Level of\nConcern') + 
  scale_fill_brewer(palette="GnBu")+
  theme_classic() + 
  theme(legend.position='bottom', text=element_text(size=20))
ggsave(file=paste0(out, 'map_control_means.pdf'), width=11, height=8)

data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  filter(!is.na(slr) & !is.na(value) & treat_map == 0) %>%
  group_by(name) %>%
  do(tidy(t.test(value ~ slr, data = .))) %>%
  mutate(name = dv.naming(name)) %>%
  filter(!is.na(name)) %>%
  arrange(name) %>%
  select(` ` = name, `Mean Inside SLR` = estimate2, `Mean Outside SLR` = estimate1, Difference = estimate, `p value` = p.value) -> diff_table
print(xtable(diff_table, 
             caption = 'Two-sample t-test of difference in mean levels of concern between respondents inside and outside of Sea Level Rise zones. Concern variable is scaled to range between 0 and 1.',
             label = 'tab:ttest_main'), 
      include.rownames=F,
      file = paste0(out, 'ttest_main.tex'))

ks.test(x = data %>%
          filter(treat_map == 0 & slr==T) %>%
          select(dv_personal) %>%
          unlist() %>%
          as.numeric(),
        y = data %>%
          filter(treat_map == 0 & slr==F) %>%
          select(dv_personal) %>%
          unlist() %>%
          as.numeric())

data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  filter(!is.na(slr) & !is.na(value) & treat_map == 0) %>%
  group_by(name) %>%
  do(tidy(ks.test(value ~ slr, data = .)))

data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name, slr) %>%
  filter(!is.na(slr)) %>%
  do(tidy(lm_robust(value ~ treat_map, 
                    fixed_effects=~geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          slr==FALSE & term=="treat_mapTRUE" ~ "Live Outside\nof SLR Zone",
                          slr==TRUE & term=="treat_mapTRUE" ~ "Live Within\nSLR Zone",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = dv.naming(name)) %>%
  filter(!is.na(name) & !is.na(term)) -> reg_results
ggplot(reg_results) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error))) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  labs(x='', y = '') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_main_results_nocovar.pdf'), width=11, height=4)

# ordered logit
j <- 1
ologit <- NULL
for(i in names(data)[124:128]){
  temp <- tidy(polr(as.formula(paste0('as.factor(', i, ') ~ treat_map + geo')), 
                    data = data %>% filter(slr==T)))
  ologit <- ologit %>%
    bind_rows(temp %>% mutate(slr=T, name = i))
  j <- j+1
  
  temp <- tidy(polr(as.formula(paste0('as.factor(', i, ') ~ treat_map + geo')), 
                    data = data %>% filter(slr==F)))
  ologit <- ologit %>%
    bind_rows(temp %>% mutate(slr=F, name = i))
  
  j <- j+1
  
}

ologit %>%
  filter(term == 'treat_mapTRUE') %>% 
  mutate(name = dv.naming(name),
         slr = ifelse(slr==TRUE, 'In Flood Zone', 'Out of Flood Zone'),
         row = ifelse(name=="1. Personally", 'a.', 'b.')) %>% 
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=name, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=slr), position=position_dodge(width=.3)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  #coord_flip() +  
  #facet_grid(rows=vars(row), scales='free_x', space='free') + 
  facet_wrap(~row, nrow=2, scales='free_x') + 
  scale_color_manual(values = c('black', 'red')) + 
  labs(x='', y = '') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_main_results_ologit.pdf'), width=8, height=8)

# just CA
data %>%
  filter(geo=="CA") %>% 
  select(starts_with('treat_'), starts_with('dv_'), geo, slr) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name, slr) %>%
  filter(!is.na(slr)) %>%
  do(tidy(lm_robust(value ~ treat_map, 
                    fixed_effects=~geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          slr==FALSE & term=="treat_mapTRUE" ~ "Live Outside\nof SLR Zone",
                          slr==TRUE & term=="treat_mapTRUE" ~ "Live Within\nSLR Zone",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = dv.naming(name)) %>%
  filter(!is.na(name) & !is.na(term)) -> reg_results
ggplot(reg_results) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error))) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  labs(x='', y = '') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20),
        axis.text.x = element_text(size=5)) 
ggsave(file=paste0(out, 'map_main_results_caonly.pdf'), width=10, height=10)

ggplot(reg_results %>%
         mutate(row = ifelse(name=="1. Personally", 'a.', 'b.'))) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=name, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=term), position=position_dodge(width=.3)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  #coord_flip() +  
  #facet_grid(rows=vars(row), scales='free_x', space='free') + 
  facet_wrap(~row, nrow=2, scales='free_x') + 
  scale_color_manual(values = c('black', 'red')) + 
  labs(x='', y = '') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_main_results_new.pdf'), width=8, height=8)


library(xtable)
print(xtable(reg_results %>%
               ungroup() %>% 
               arrange(name) %>%
               select(Variable = name, SLR = term, estimate, std.error, p.value), 
      caption = 'Tabular results of map experiment'), 
      file = paste0(out, 'main_map.tex'),
      include.rownames = getOption("xtable.include.rownames", FALSE))

# covariate adjusted
data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr,
         race_asian,race_black,race_hispanic,race_white,edu,gender,ideo, pid, rent) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name, slr) %>%
  filter(!is.na(slr)) %>%
  do(tidy(lm_robust(value ~ treat_map  + 
                      race_asian + race_black + race_hispanic + race_white + gender + ideo + pid + rent, 
                    fixed_effects=~geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          slr==FALSE & term=="treat_mapTRUE" ~ "Out of SLR Zone",
                          slr==TRUE & term=="treat_mapTRUE" ~ "In SLR Zone",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = dv.naming(name),
         type = "Covariate Adjusted") %>%
  bind_rows(reg_results %>%
              mutate(type = 'No Covariates')) %>%
  filter(!is.na(term) & !is.na(name)) -> reg_results2

ggplot(reg_results2) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=type),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  scale_color_manual(values = c('black', 'grey')) + 
  labs(x='', y = '') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 

ggplot(reg_results2) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=type),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  scale_color_manual(values = c('black', 'grey')) + 
  labs(x='', y = '') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 

ggsave(file=paste0(out, 'map_main_results.pdf'), width=11, height=8)

# with means
data %>%
  group_by(treat_map, slr) %>%
  summarise_at(vars(starts_with('dv_')), mean, na.rm=T) %>%
  filter(!is.na(slr)) %>%
  select(treat_map:dv_future) %>%
  pivot_longer(cols = c(dv_personal:dv_future)) %>%
  mutate(colname = ifelse(treat_map==TRUE, 'x_t', 'x_c')) %>%
  select(-treat_map) %>%
  pivot_wider(id_cols=c(slr, name), names_from=colname, values_from = value) %>%
  mutate(name=dv.naming(name),
         slr = ifelse(slr==TRUE, 'In Flood Zone', 'Out of Flood Zone')) %>%
ggplot() + 
  geom_point(aes(y = name, x=x_t, col=slr),
             pch=18, size=5,
             position = position_dodge(width=.2)) + 
  geom_linerange(aes(y = name, xmin=x_c, xmax=x_t,  col=slr),
                 position = position_dodge(width=.2)) + 
  labs(x='Average Perceived Harm', y = '', caption = 'Arrows travel from control mean (no map shown) to treatment mean (map shown).') +
  theme_classic() +
  scale_color_manual(values=c('red', 'blue')) + 
  scale_x_continuous(breaks = c(0:3), limits = c(0,3),
                     labels = c('Not\nat all','Only a\nlittle','A moderate\namount','A great\ndeal')) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_main_means.pdf'), width=11, height=8)

# means with error bars
data %>%
  group_by(treat_map, slr) %>%
  summarise_at(vars(starts_with('dv_') & !ends_with('_dk') & !contains('policy')), 
               list(mean = mean, sd = sd), na.rm=T) %>%
  filter(!is.na(slr)) %>%
  pivot_longer(cols = c(dv_personal_mean:dv_future_mean, dv_personal_sd:dv_future_sd)) %>%
  mutate(type = ifelse(str_detect(name, '_mean'), 'mean', 'sd'),
         name = str_remove_all(name, '_mean|_sd')) %>%
  pivot_wider(id_cols = c(treat_map, slr, name), names_from = type) %>%
  left_join(data %>%
              group_by(treat_map, slr) %>%
              summarise(n=n())) %>%
  mutate(sd = sd/sqrt(n),
         name=dv.naming(name),
         slr = ifelse(slr==TRUE, 'In Flood Zone', 'Out of Flood Zone'),
         treat_map = ifelse(treat_map==T, 'Map Treatment', 'Control')) %>%
  ggplot() + 
  geom_col(aes(y = name, x=mean, fill = treat_map),
           position='dodge') + 
  geom_linerange(aes(y = name, xmin=mean - 1.96*sd, xmax=mean + 1.96*sd, group = treat_map),
                 position = position_dodge(width=1)) + 
  facet_wrap(~slr, ncol=1) + 
  labs(x='Average Perceived Harm', y = '') +
  theme_classic() +
  coord_flip() + 
  scale_fill_manual(values=c('grey', 'light grey')) + 
  scale_x_continuous(breaks = c(0:3), limits = c(0,3),
                     labels = c('Not\nat all','Only a\nlittle','A moderate\namount','A great\ndeal')) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_main_bar1.pdf'), width=11, height=8)

# means with error bars
data %>%
  group_by(treat_map, slr) %>%
  summarise_at(vars(starts_with('dv_') & !ends_with('_dk') & !contains('policy')), 
               list(mean = mean, sd = sd), na.rm=T) %>%
  filter(!is.na(slr)) %>%
  pivot_longer(cols = c(dv_personal_mean:dv_future_mean, dv_personal_sd:dv_future_sd)) %>%
  mutate(type = ifelse(str_detect(name, '_mean'), 'mean', 'sd'),
         name = str_remove_all(name, '_mean|_sd')) %>%
  pivot_wider(id_cols = c(treat_map, slr, name), names_from = type) %>%
  left_join(data %>%
              group_by(treat_map, slr) %>%
              summarise(n=n())) %>%
  mutate(sd = sd/sqrt(n),
         name=dv.naming(name),
         slr = ifelse(slr==TRUE, 'In\nFlood Zone', 'Out of\nFlood Zone'),
         treat_map = ifelse(treat_map==T, 'Map Treatment', 'Control')) %>%
  ggplot() + 
  geom_point(aes(y = slr, x=mean, pch = treat_map, col = slr), size=3,
           position=position_dodge(width=.2)) + 
  geom_linerange(aes(y = slr, xmin=mean - 1.96*sd, xmax=mean + 1.96*sd, group = treat_map, col=slr),
                 size = 1, position = position_dodge(width=.2)) + 
  facet_wrap(~name) + 
  labs(x='Average Perceived Harm', y = '') +
  theme_classic() +
  coord_flip() + 
  scale_color_grey(guide='none') + 
  scale_x_continuous(breaks = c(0:3), limits = c(0,3),
                     labels = c('Not\nat all','Only a\nlittle','A moderate\namount','A great\ndeal')) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_means_final.pdf'), width=11, height=8)

data %>%
  group_by(treat_map, slr) %>%
  summarise_at(vars(starts_with('dv_') & !ends_with('_dk') & !contains('policy')), 
               list(mean = mean, sd = sd), na.rm=T) %>%
  filter(!is.na(slr)) %>%
  pivot_longer(cols = c(dv_personal_mean:dv_future_mean, dv_personal_sd:dv_future_sd)) %>%
  mutate(type = ifelse(str_detect(name, '_mean'), 'mean', 'sd'),
         name = str_remove_all(name, '_mean|_sd')) %>%
  pivot_wider(id_cols = c(treat_map, slr, name), names_from = type) %>%
  left_join(data %>%
              group_by(treat_map, slr) %>%
              summarise(n=n())) %>%
  mutate(sd = sd/sqrt(n),
         name=dv.naming(name),
         slr = ifelse(slr==TRUE, 'In Flood Zone', 'Out of Flood Zone'),
         treat_map = ifelse(treat_map==T, 'Map Treatment', 'Control')) %>%
  ggplot() + 
  geom_col(aes(y = name, x=mean, fill = treat_map),
           position='dodge') + 
  geom_linerange(aes(y = name, xmin=mean - sd, xmax=mean + sd, group = treat_map),
                 position = position_dodge(width=1)) + 
  facet_wrap(~slr, ncol=1) + 
  labs(x='Average Perceived Harm', y = '') +
  theme_classic() +
  coord_flip() + 
  scale_fill_manual(values=c('grey', 'light grey')) + 
  scale_x_continuous(breaks = c(0:3), limits = c(0,3),
                     labels = c('Not\nat all','Only a\nlittle','A moderate\namount','A great\ndeal')) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_main_bar1.pdf'), width=11, height=8)

# with means -- restricted to 500m around cut point
data %>%
  filter(abs(dist_min)<=250) %>%
  group_by(treat_map, slr) %>%
  summarise_at(vars(starts_with('dv_') & !ends_with('_dk') & !contains('policy')), 
               list(mean = mean, sd = sd), na.rm=T) %>%
  filter(!is.na(slr)) %>%
  pivot_longer(cols = c(dv_personal_mean:dv_future_mean, dv_personal_sd:dv_future_sd)) %>%
  mutate(type = ifelse(str_detect(name, '_mean'), 'mean', 'sd'),
         name = str_remove_all(name, '_mean|_sd')) %>%
  pivot_wider(id_cols = c(treat_map, slr, name), names_from = type) %>%
  left_join(data %>%
              filter(abs(dist_min)<=250) %>%
              group_by(treat_map, slr) %>%
              summarise(n=n())) %>%
  mutate(sd = sd/sqrt(n),
         name=dv.naming(name),
         slr = ifelse(slr==TRUE, 'In\nFlood Zone', 'Out of\nFlood Zone'),
         treat_map = ifelse(treat_map==T, 'Map Treatment', 'Control')) %>%
  ggplot() + 
  geom_point(aes(y = slr, x=mean, pch = treat_map, col = slr), size=3,
             position=position_dodge(width=.2)) + 
  geom_linerange(aes(y = slr, xmin=mean - 1.96*sd, xmax=mean + 1.96*sd, group = treat_map, col=slr),
                 size = 1, position = position_dodge(width=.2)) + 
  facet_wrap(~name) + 
  labs(x='Average Perceived Harm', y = '') +
  theme_classic() +
  coord_flip() + 
  scale_color_grey(guide='none') + 
  scale_x_continuous(breaks = c(0:3), limits = c(0,3),
                     labels = c('Not\nat all','Only a\nlittle','A moderate\namount','A great\ndeal')) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_main_means_250m.pdf'), width=11, height=8)

# Traffic Experiment -------------------------------------------------------

# traffic experiment
reg_results <- data %>%
  select(starts_with('treat_'), starts_with('dv_'),  geo, slr) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name) %>%
  #filter(treat_map==0) %>%
  do(tidy(lm_robust(value ~ treat_traf + treat_map, 
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment"),
         name = dv.naming(name)) %>%
  filter(!is.na(name) & !is.na(term))

ggplot(reg_results) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=name, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error))) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  #facet_wrap(~name, nrow=1) + 
  labs(x='', y = 'Effect of Traffic Treatment') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'traffic_main_results.pdf'), width=11, height=8)

print(xtable(reg_results %>%
               arrange(name) %>%
               select(Variable = name, SLR = term, estimate, std.error, p.value),
             caption = 'Tabular results of traffic experiment'), 
      file = paste0(out, 'main_traffic.tex'),
      include.rownames = getOption("xtable.include.rownames", FALSE))

# ologit
j <- 1
ologit <- NULL
for(i in names(data)[124:128]){
  temp <- tidy(polr(as.formula(paste0('as.factor(', i, ') ~ treat_traf + treat_map')), 
                    data = data %>% filter(slr==T)))
  ologit <- ologit %>%
    bind_rows(temp %>% mutate(name = i))
  j <- j+1
  
}

# traffic experiment
ologit %>%
  filter(term=='treat_trafTRUE') %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment"),
         name = dv.naming(name)) %>%
  filter(!is.na(name) & !is.na(term)) %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=name, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error))) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  #facet_wrap(~name, nrow=1) + 
  labs(x='', y = 'Effect of Traffic Treatment') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'traffic_main_results_ologit.pdf'), width=11, height=8)


data %>%
  #filter(!(Q89 %in% c('Not at all credible', 'Not very credible')) & treat_map==0) %>%
  mutate(above_median = (traf > median(traf, na.rm=T))*1,
         above_median = ifelse(is.na(above_median), 0, above_median)) %>%
  select(starts_with('treat_'), starts_with('dv_'),  geo, slr, above_median) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name) %>%
  do(tidy(lm_robust(value ~ treat_traf + above_median, 
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Below Median\nTraffic Treatment",
                          term=='above_median' ~ "Above Median\nTraffic Treatment",
                          term=='treat_mapTRUE' ~ "Out of Flood Zone",
                          term=='slrTRUE' ~ "Sea Level Rise",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = dv.naming(name)) %>%
  filter(!is.na(name) & !is.na(term)) %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=name, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=term), position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  #facet_wrap(~name, nrow=1) + 
  labs(x='', y = '', caption='Median traffic treatment is 11 minutes.') +
  scale_color_manual(values=c('black', 'grey')) + 
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'traffic_time_split.pdf'), width=11, height=8)

data %>%
  select(starts_with('treat_'), starts_with('dv_'),  geo, slr, traf) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name) %>%
  filter(!is.na(treat_traf) & treat_map==0 & !str_detect(name, '_dk')& !str_detect(name, 'policy')) %>%
  mutate(treat_traf = ifelse(treat_traf==T, 'Traffic Treatment', 'Control'),
         name = dv.naming(name)) %>%
  ggplot(aes(x=traf, y=value, col=treat_traf)) + 
  geom_point(alpha=.1) + 
  geom_smooth() + 
  geom_vline(xintercept=11, lty=2) + 
  scale_color_manual(values=c('blue', 'red')) + 
  xlim(0,60) + 
  facet_wrap(~name) + 
  labs(x='Traffic Treatment (minutes)', y = 'Level of concern') + 
  theme_classic() + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'traffic_scatterplot.pdf'), width=11, height=8)

data %>%
  #filter(!(Q89 %in% c('Not at all credible', 'Not very credible'))) %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr, traf) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = dv.naming(name),
         treat_traf = ifelse(treat_traf==TRUE, 'Map Treatment', "Control"),
         bucket = round(traf/5,0)*5) %>%
  group_by(bucket, name) %>%
  filter(n()>=15 & !is.na(name) &!is.na(bucket) & bucket<=60) %>%
  do(tidy(lm_robust(value ~ treat_traf,
                    fixed_effects = ~geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(below_zero = ifelse(bucket<=0, 'a', 'b'),
         sig = ifelse(p.value <= 0.05, 'sig', 'ns')) %>%
  ggplot() + 
  geom_pointrange(aes(x=bucket, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error),
                      col = sig)) + 
  geom_smooth(aes(x=bucket, y = estimate), col='red', se=F) + 
  geom_vline(xintercept=0, lty=2) + 
  geom_hline(yintercept=0, lty=2) + 
  labs(x='Projected Traffic Time Increase',
       y='Treatment Effect of Traffic Vignette',
       caption = 'Points show regression coefficients at 5 minute binned intervals
       Error bars show 95/% confidence intervals. Black bars are significant at p < 0.05') + 
  scale_color_manual(values = c('grey', 'black')) + 
  facet_wrap(~name) + 
  theme_classic() +
  theme(legend.position='none', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'traf_dist_bingreg.pdf'), width=11, height=8)

# with means
data %>%
  filter(treat_map==0) %>%
  mutate(traf_med = ifelse(traf>11, 'Above Median Traffic Increase', 'Below Median Traffic Increase')) %>%
  group_by(treat_traf, traf_med) %>%
  summarise_at(vars(starts_with('dv_')), mean, na.rm=T) %>%
  filter(!is.na(traf_med)) %>%
  select(treat_traf:dv_future) %>%
  pivot_longer(cols = c(dv_personal:dv_future)) %>%
  mutate(colname = ifelse(treat_traf==TRUE, 'x_t', 'x_c')) %>%
  select(-treat_traf) %>%
  pivot_wider(id_cols=c(traf_med, name), names_from=colname, values_from = value) %>%
  mutate(name=dv.naming(name)) %>%
  ggplot() + 
  geom_point(aes(y = name, x=x_t, col=traf_med),
             pch=18, size=5,
             position = position_dodge(width=.2)) + 
  geom_linerange(aes(y = name, xmin=x_c, xmax=x_t,  col=traf_med),
                 position = position_dodge(width=.2)) + 
  labs(x='Average Perceived Harm', y = '', caption = 'Arrows travel from control mean (no map shown) to treatment mean (map shown).') +
  theme_classic() +
  scale_color_manual(values=c('red', 'blue')) + 
  scale_x_continuous(breaks = c(0:3), limits = c(0,3),
                     labels = c('Not\nat all','Only a\nlittle','A moderate\namount','A great\ndeal')) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'traf_split_means.pdf'), width=11, height=8)

# Interaction -------------------------------------------------------------


# main results -- presence of treatment + interaction
data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name) %>%
  do(tidy(lm_robust(value ~ treat_map + treat_traf + treat_map*treat_traf, 
                    fixed_effects = ~ geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          term=='treat_mapTRUE' ~ "Map Treatment",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = dv.naming(name))  -> reg_results
  ggplot(reg_results) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error))) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  labs(x='', y = '') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
  
  

# covariate adjusted
data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo,
         race_asian,race_black,race_hispanic,race_white,edu,gender,ideo, pid, rent) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name) %>%
  do(tidy(lm_robust(value ~ treat_map + treat_traf + treat_map*treat_traf + 
                      race_asian + race_black + race_hispanic + race_white + edu + gender + ideo + pid + rent, 
                    fixed_effects = ~ geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          term=='treat_mapTRUE' ~ "Map Treatment",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = case_when(name=='dv_personal' ~ "Personally",
                          name=='dv_community' ~ "Local\nCommunity",
                          name=='dv_us' ~ "United\nStates",
                          name=='dv_developing' ~ "Developing\nCountries",
                          name=='dv_future' ~ "Future\nGenerations"),
         type = "Covariate Adjusted") %>%
  bind_rows(reg_results %>%
              mutate(type = 'No Covariates')) %>%
  filter(!is.na(term) & !is.na(name)) %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=type),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  scale_color_manual(values = c('black', 'grey')) + 
  labs(x='', y = '', caption = 'All regressions contain fixed effects for location (CA, FL, NJ, VA).
       Covariates include race, education, gender, ideology, and party ID (including leaners).') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 


# DK Outcome --------------------------------------------------------------


# looking at dk as outcome
data %>%
  select(starts_with('treat_'), ends_with('_dk'), geo) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name) %>%
  do(tidy(lm_robust(value ~ treat_map + treat_traf + treat_map*treat_traf, 
                    fixed_effects = ~ geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          term=='treat_mapTRUE' ~ "Map Treatment",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = case_when(name=='dv_personal_dk' ~ "Personally",
                          name=='dv_community_dk' ~ "Local\nCommunity",
                          name=='dv_us_dk' ~ "United\nStates",
                          name=='dv_developing_dk' ~ "Developing\nCountries",
                          name=='dv_future_dk' ~ "Future\nGenerations")) %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error))) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  labs(x='', y = "Likelihood of Answering Don't Know") +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 

# traffic treatment
hist(data$traf)
summary(data$traf)

ggplot(data=data, aes(x=traf, y = dv_personal)) + 
  geom_point(alpha=.1) +
  geom_smooth() + 
  theme_classic()

med_level <- median(data$traf, na.rm=T)

lm_robust(dv_personal ~ above_median, 
          fixed_effects = ~ geo,
          se_type = 'HC1',
          data=data %>%
            mutate(above_median = ifelse(traf>med_level, 1,0)))

lm_robust(dv_personal ~ traf, 
          fixed_effects = ~ geo,
          se_type = 'HC1',
          data=data %>%
            mutate(traf = ifelse(is.na(traf), 0, traf),
                   traf = ifelse(traf<0, 0, traf)))

lm_robust(dv_personal ~ traf, 
          fixed_effects = ~ geo,
          se_type = 'HC1',
          data=data)



# WTP Experiment ----------------------------------------------------------

table(data$WTP_use)
summary(data$WTP_money)
table(data$wtpexperiment, data$wtpstrength)
table(data$wtp_dv)

ggplot(data=data, aes(x=WTP_money, wtp_dv)) + 
  geom_point(position='jitter', alpha=.1) + 
  geom_smooth(method='lm') + 
  theme_classic()

lm_robust(wtp_dv ~ WTP_money,
          fixed_effects=WTP_use,
          se_type = 'HC1',
          data=data)

lm_robust(wtp_dv ~ WTP_money + treat_map + slr,
          fixed_effects=WTP_use,
          se_type = 'HC1',
          data=data)

data %>%
  mutate(color_manual = case_when(treat_map==0 & slr==1 ~ 'C -- In SLR Zone',
                                  treat_map==0 & slr==0 ~ 'C -- Out of SLR Zone',
                                  treat_map==1 & slr==1 ~ 'T -- In SLR Zone',
                                  treat_map==1 & slr==0 ~ 'T -- Out of SLR Zone')) %>%
  ggplot(aes(x=WTP_money, wtp_dv, col=color_manual, lty=color_manual, pch=color_manual)) + 
  geom_point(position='jitter', alpha=.1) + 
  geom_smooth(method='lm', se=F) + 
  labs(x='Cost of Policy ($)', y='Support for Policy') + 
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'wtp_scatterplot_condition.pdf'), width=11, height=8)

data %>%
  mutate(color_manual = case_when(treat_map==0 & slr==1 ~ 'Control -- In SLR Zone',
                                  treat_map==0 & slr==0 ~ 'Control -- Out of SLR Zone',
                                  treat_map==1 & slr==1 ~ 'Treatment -- In SLR Zone',
                                  treat_map==1 & slr==0 ~ 'Treatment -- Out of SLR Zone')) %>%
  group_by(color_manual) %>%
  filter(!is.na(color_manual)) %>%
  do(tidy(lm_robust(wtp_dv ~ WTP_money,
                    fixed_effects = ~ WTP_use,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(group = 'Map Experiment') %>%
  bind_rows(data %>%
              mutate(color_manual = case_when(treat_traf==0 ~ 'Control',
                                              traf > 11 ~ 'Above Median\nTraffic Treatment',
                                              traf <= 11 ~ 'Below Median\nTraffic Treatment')) %>%
              group_by(color_manual) %>%
              do(tidy(lm_robust(wtp_dv ~ WTP_money,
                                fixed_effects = ~ WTP_use,
                                se_type = 'HC1',
                                data=.)))%>%
              mutate(group = 'Traffic Experiment')) %>%
  filter(term=="WTP_money") %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=color_manual, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error))) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~group, nrow=2, scales='free') + 
  labs(x='', y = "Linear Decline in Willingness to Pay") +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'wtp_reg_condition.pdf'), width=11, height=8)

data %>%
  group_by(WTP_use) %>%
  do(tidy(lm_robust(wtp_dv ~ WTP_money, se_type = 'HC1',data=.))) %>%
  filter(term=="WTP_money") %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=WTP_use, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error))) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  #facet_wrap(~name, nrow=1) + 
  labs(x='', y = "Linear Decline in Willingness to Pay") +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'wtp_reg_proposal.pdf'), width=11, height=8)


# Policy Outcomes ---------------------------------------------------------

# no effect on belief in link between SLR and GW
data %>%
  filter(!is.na(slr)) %>%
  group_by(slr) %>%
  do(tidy(lm_robust(belief_gq ~ treat_map,
                    se_type = 'HC1',
                    fixed_effects = ~ geo,
                    data=.)))

lm_robust(belief_gq ~ treat_traf*pid,
          se_type = 'HC1',
          fixed_effects = ~ geo,
          data=data) %>%
  tidy()

# no effect on belief in discussion among social cicle
lm_robust(social_talk ~ treat_traf*pid,
          fixed_effects = ~ geo,
          se_type = 'HC1',
          data=data)

data %>%
  filter(!is.na(slr)) %>%
  group_by(slr) %>%
  do(tidy(lm_robust(social_talk ~ treat_map,
                    fixed_effects = ~ geo,
                    se_type = 'HC1',
                    data=.)))

data %>%
  select(starts_with('treat_'), starts_with('dv_policy'), geo, slr) %>%
  pivot_longer(cols=starts_with('dv_policy')) %>%
  filter(!is.na(slr) & !is.na(value) & treat_map == 0) %>%
  group_by(name) %>%
  do(tidy(t.test(value ~ slr, data = .)))

# policies
data %>%
  select(starts_with('treat_'), starts_with('dv_policy'), geo, slr) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name, slr) %>%
  filter(!is.na(slr)) %>%
  do(tidy(lm_robust(value ~ treat_map, 
                    fixed_effects = ~ geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          slr==FALSE & term=="treat_mapTRUE" ~ "Live Outside\nof SLR Zone",
                          slr==TRUE & term=="treat_mapTRUE" ~ "Live Within\nSLR Zone",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = policy.naming(name)) %>%
  filter(!is.na(name) & !is.na(term)) -> reg_results
ggplot(reg_results) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error))) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  labs(x='', y = '') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_policy_nocovar.pdf'), width=12, height=4)

# covariate adjusted
data %>%
  select(starts_with('treat_'), starts_with('dv_policy'), geo, slr,
         race_asian,race_black,race_hispanic,race_white,edu,gender,ideo, pid, rent) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name, slr) %>%
  filter(!is.na(slr)) %>%
  do(tidy(lm_robust(value ~ treat_map + 
                      race_asian + race_black + race_hispanic + race_white + edu + gender + ideo + pid + rent, 
                    fixed_effects = ~ geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          slr==FALSE & term=="treat_mapTRUE" ~ "Out of SLR Zone",
                          slr==TRUE & term=="treat_mapTRUE" ~ "In SLR Zone",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = policy.naming(name),
         type = "Covariate Adjusted") %>%
  bind_rows(reg_results %>%
              mutate(type = 'No Covariates')) %>%
  filter(!is.na(term) & !is.na(name)) -> reg_results2

ggplot(reg_results2) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=type),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  scale_color_manual(values = c('black', 'grey')) + 
  labs(x='', y = '', caption = 'Regressions compare two treatments to control of seeing no map at all.
  All regressions contain fixed effects for location (CA, FL, NJ, VA).
       Covariates include race, education, gender, ideology, and party ID (including leaners).') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_policy_results.pdf'), width=11, height=8)

# by state (just map treatment)
data %>%
  select(starts_with('treat_'), starts_with('dv_policy'), geo, slr) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name, slr, geo) %>%
  filter(!is.na(slr)) %>%
  do(tidy(lm_robust(value ~ treat_map, 
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          slr==FALSE & term=="treat_mapTRUE" ~ "Out of SLR Zone",
                          slr==TRUE & term=="treat_mapTRUE" ~ "In SLR Zone",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = policy.naming(name)) %>%
  filter(!is.na(name) & !is.na(term)) %>%
  #filter(term=='Map Treatment') %>%
  filter(!is.na(name) & !is.na(term)) %>%
  bind_rows(reg_results2 %>% 
              filter(type=='No Covariates') %>%
              mutate(geo='All States')) %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=geo, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col = term),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_grid(~name) + 
  labs(x='', y = 'Effect of Map Treatment', caption =  'Regressions compare two treatments to control of seeing no map at all.
       Covariates include race, gender, and party ID (including leaners).') +
  scale_color_manual(values=c('grey', 'black')) + 
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_policy_state_results.pdf'), width=11, height=8)


data %>%
  filter(treat_map==0) %>%
  mutate(above_median = (traf > median(traf, na.rm=T))*1,
         above_median = ifelse(is.na(above_median), 0, above_median)) %>%
  select(starts_with('treat_'), starts_with('dv_'),  geo, slr, above_median) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name) %>%
  do(tidy(lm_robust(value ~ treat_traf, 
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Below Median\nTraffic Treatment",
                          term=='above_median' ~ "Above Median\nTraffic Treatment",
                          term=='treat_mapTRUE' ~ "Out of Flood Zone",
                          term=='slrTRUE' ~ "Sea Level Rise",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = policy.naming(name),
         group = 'Policies') %>%
  filter(!is.na(name) & !is.na(term)) %>%
  bind_rows(data %>%
              select(starts_with('treat_'), starts_with('dv_'),  geo, slr) %>%
              pivot_longer(cols=starts_with('dv_')) %>%
              group_by(name) %>%
              #filter(treat_map==0) %>%
              do(tidy(lm_robust(value ~ treat_traf + treat_map, 
                                se_type = 'HC1',
                                data=.))) %>%
              mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment"),
                     name = dv.naming(name),
                     group = 'Perceived Harm') %>%
              filter(!is.na(name) & !is.na(term))) %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=name, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error)), position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~group, nrow=1, scales = 'free') + 
  labs(x='', y = '') +
  scale_color_manual(values=c('black', 'grey')) + 
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'traffic_policy.pdf'), width=11, height=5)

data %>%
  filter(treat_map==0) %>%
  mutate(above_median = (traf > median(traf, na.rm=T))*1,
         above_median = ifelse(is.na(above_median), 0, above_median)) %>%
  select(starts_with('treat_'), starts_with('dv_policy'),  geo, slr, above_median) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name) %>%
  do(tidy(lm_robust(value ~ treat_traf + above_median, 
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Below Median\nTraffic Treatment",
                          term=='above_median' ~ "Above Median\nTraffic Treatment",
                          term=='treat_mapTRUE' ~ "Out of Flood Zone",
                          term=='slrTRUE' ~ "Sea Level Rise",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = policy.naming(name)) %>%
  filter(!is.na(name) & !is.na(term)) %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=name, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=term), position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  #facet_wrap(~name, nrow=1) + 
  labs(x='', y = '', caption='Median traffic treatment is 11 minutes.
       Control group includes respondents who got neither traffic nor map treatments') +
  scale_color_manual(values=c('black', 'grey')) + 
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'traffic_policy_split.pdf'), width=11, height=8)

# with means
data %>%
  group_by(treat_map, slr) %>%
  summarise_at(vars(starts_with('dv_') & !ends_with('_dk') & contains('policy')), 
               list(mean = mean, sd = sd), na.rm=T) %>%
  filter(!is.na(slr)) %>%
  pivot_longer(cols = c(dv_policy1_mean:dv_policy4_sd)) %>%
  mutate(type = ifelse(str_detect(name, '_mean'), 'mean', 'sd'),
         name = str_remove_all(name, '_mean|_sd')) %>%
  pivot_wider(id_cols = c(treat_map, slr, name), names_from = type) %>%
  left_join(data %>%
              group_by(treat_map, slr) %>%
              summarise(n=n())) %>%
  mutate(sd = sd/sqrt(n),
         name=policy.naming(name),
         slr = ifelse(slr==TRUE, 'In\nFlood Zone', 'Out of\nFlood Zone'),
         treat_map = ifelse(treat_map==T, 'Map Treatment', 'Control')) %>%
  ggplot() + 
  geom_point(aes(y = slr, x=mean, pch = treat_map, col = slr), size=3,
             position=position_dodge(width=.2)) + 
  geom_linerange(aes(y = slr, xmin=mean - 1.96*sd, xmax=mean + 1.96*sd, group = treat_map, col=slr),
                 size = 1, position = position_dodge(width=.2)) + 
  facet_wrap(~name) + 
  labs(x='Average Support for Policy', y = '') +
  theme_classic() +
  coord_flip() + 
  scale_color_grey(guide='none') + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_policy_means.pdf'), width=11, height=8)

data %>%
  #filter(!(Q89 %in% c('Not at all credible', 'Not very credible'))) %>%
  select(starts_with('treat_'), starts_with('dv_policy'), geo, slr, traf) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = policy.naming(name),
         treat_traf = ifelse(treat_traf==TRUE, 'Map Treatment', "Control"),
         bucket = round(traf/5,0)*5) %>%
  group_by(bucket, name) %>%
  filter(n()>=15 & !is.na(name) &!is.na(bucket) & bucket<=60) %>%
  do(tidy(lm_robust(value ~ treat_traf,
                    fixed_effects = ~geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(below_zero = ifelse(bucket<=0, 'a', 'b'),
         sig = ifelse(p.value <= 0.05, 'sig', 'ns')) %>%
  ggplot() + 
  geom_pointrange(aes(x=bucket, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error),
                      col = sig)) + 
  geom_smooth(aes(x=bucket, y = estimate), col='red', se=F) + 
  geom_vline(xintercept=0, lty=2) + 
  geom_hline(yintercept=0, lty=2) + 
  labs(x='Projected Traffic Time Increase',
       y='Treatment Effect of Traffic Vignette',
       caption = 'Points show regression coefficients at 5 minute binned intervals
       Error bars show 95/% confidence intervals. Black bars are significant at p < 0.05') + 
  scale_color_manual(values = c('grey', 'black')) + 
  facet_wrap(~name) + 
  theme_classic() +
  theme(legend.position='none', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'traf_policy_dist_bingreg.pdf'), width=11, height=8)

# Heterogeneous Effects ---------------------------------------------------------

## GEOGRAPHY
# with means -- by state
data %>%
  group_by(treat_map, slr, geo) %>%
  summarise_at(vars(starts_with('dv_') & !ends_with('_dk') & !contains('policy')), 
               list(mean = mean, sd = sd), na.rm=T) %>%
  filter(!is.na(slr)) %>%
  pivot_longer(cols = c(dv_personal_mean:dv_future_mean, dv_personal_sd:dv_future_sd)) %>%
  mutate(type = ifelse(str_detect(name, '_mean'), 'mean', 'sd'),
         name = str_remove_all(name, '_mean|_sd')) %>%
  pivot_wider(id_cols = c(treat_map, slr, geo, name), names_from = type) %>%
  left_join(data %>%
              group_by(treat_map, slr, geo) %>%
              summarise(n=n())) %>%
  mutate(sd = sd/sqrt(n),
         name=dv.naming(name),
         slr = ifelse(slr==TRUE, 'In\nFlood Zone', 'Out of\nFlood Zone'),
         treat_map = ifelse(treat_map==T, 'Map Treatment', 'Control')) %>%
  ggplot() + 
  geom_point(aes(y = slr, x=mean, pch = treat_map, col = slr), size=2,
             position=position_dodge(width=.2)) + 
  geom_linerange(aes(y = slr, xmin=mean - 1.96*sd, xmax=mean + 1.96*sd, group = treat_map, col=slr),
                 size = 1, position = position_dodge(width=.2)) + 
  facet_grid(name~ geo) + 
  labs(x='Average Perceived Harm', y = '') +
  theme_classic() +
  scale_color_grey(guide='none') + 
  scale_x_continuous(breaks = c(0:3), limits = c(0,3),
                     labels = c('Not\nat all','Only a\nlittle','A moderate\namount','A great\ndeal')) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_main_means_state.pdf'), width=11, height=8)


# by state (just map treatment)
data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  group_by(name, slr, geo) %>%
  filter(!is.na(slr)) %>%
  do(tidy(lm_robust(value ~ treat_map, 
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          slr==FALSE & term=="treat_mapTRUE" ~ "Out of SLR Zone",
                          slr==TRUE & term=="treat_mapTRUE" ~ "In SLR Zone",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = dv.naming(name)) %>%
  filter(!is.na(name) & !is.na(term)) %>%
  bind_rows(reg_results2 %>% 
              filter(type=='No Covariates') %>%
              mutate(geo='All States')) %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col = geo, shape=geo),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_grid(~name) + 
  labs(x='', y = 'Effect of Map Treatment') +
  #scale_color_manual(values=c('grey', 'black')) + 
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_state_results.pdf'), width=14, height=6)

data('fips_codes')
# by county (just map treatment)
data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, COUNTYFP, slr) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = ifelse(name %in% c('dv_us', 'dv_developing', 'dv_future'), 'dv_distant', name)) %>%
  group_by(name, slr, geo, COUNTYFP) %>%
  filter(!is.na(slr)) %>%
  do(tidy(lm_robust(value ~ treat_map, 
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          slr==FALSE & term=="treat_mapTRUE" ~ "Live Outside of\nSLR Zone",
                          slr==TRUE & term=="treat_mapTRUE" ~ "Live Within\nSLR Zone",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = case_when(name=='dv_personal' ~ "1. Personally",
                          name=='dv_community' ~ "2. Local\nCommunity",
                          name=='dv_distant' ~ "3. Distant\nGroups"),
         COUNTYFP = as.character(COUNTYFP)) %>%
  filter(!is.na(name) & !is.na(term)) %>%
  filter(!COUNTYFP %in% c('55', '75', 'NA')) %>% 
  filter(!is.na(COUNTYFP)) %>% 
  left_join(fips_codes %>%
              mutate(county_code = as.character(as.numeric(county_code))), 
            by = c('geo' = 'state', 'COUNTYFP' = 'county_code')) %>%
  mutate(county = paste0(str_remove(county, ' County'), ' (', geo, ')')) %>% 
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col = county),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_grid(~name) + 
  labs(x='', y = 'Effect of Map Treatment') +
  scale_color_viridis_d() + 
  #scale_color_manual(values=c('grey', 'black')) + 
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_county_results.pdf'), width=14, height=6)

data(fips_codes)

county


data %>%
  select(starts_with('treat_'), starts_with('dv_'),  geo, slr, COUNTYFP) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  filter(treat_map==0) %>%
  group_by(name, geo, COUNTYFP) %>%
  do(tidy(lm_robust(value ~ treat_traf, 
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = dv.naming(name),
         COUNTYFP = as.character(COUNTYFP)) %>%
  filter(!is.na(name) & !is.na(term) & !is.na(estimate)) %>%
  left_join(fips_codes %>%
              mutate(county_code = as.character(as.numeric(county_code))), 
            by = c('geo' = 'state', 'COUNTYFP' = 'county_code')) %>%
  mutate(county = paste0(str_remove(county, ' County'), ' (', geo, ')')) %>% 
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=name, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=county),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  #facet_wrap(~name, nrow=1) + 
  scale_color_viridis_d() + 
  labs(x='', y = 'Effect of Traffic Treatment', caption='Control group includes respondents who got neither traffic nor map treatments') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'traffic_county.pdf'), width=11, height=8)


## PID
data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr,
         race_asian,race_black,race_hispanic,race_white,edu,gender,ideo, pid, rent) %>%
  filter(!is.na(slr) & pid %in% c("D", "R", "I")) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = ifelse(name %in% c('dv_us', 'dv_developing', 'dv_future'), 'dv_distant', name)) %>%
  group_by(name, slr, pid) %>%
  do(tidy(lm_robust(value ~ treat_map, 
                    fixed_effects = ~ geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          slr==FALSE & term=="treat_mapTRUE" ~ "Live Outside of\nSLR Zone",
                          slr==TRUE & term=="treat_mapTRUE" ~ "Live Within\nSLR Zone",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = case_when(name=='dv_personal' ~ "1. Personally",
                          name=='dv_community' ~ "2. Local\nCommunity",
                          name=='dv_distant' ~ "3. Distant\nGroups"),
         type = "Covariate Adjusted") %>%
  filter(!is.na(term) & !is.na(name) & !is.na(pid)) -> reg_results2

ggplot(reg_results2) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=pid, shape=pid),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  scale_color_manual(values = c('blue', 'grey', 'red')) + 
  labs(x='', y = '') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_pid.pdf'), width=11, height=5)

data %>%
  filter(!is.na(pid)) %>%
  group_by(treat_map, slr, pid) %>%
  summarise_at(vars(starts_with('dv_') & !ends_with('_dk') & !contains('policy')), 
               list(mean = mean, sd = sd), na.rm=T) %>%
  filter(!is.na(slr)) %>%
  pivot_longer(cols = c(dv_personal_mean:dv_future_mean, dv_personal_sd:dv_future_sd)) %>%
  mutate(type = ifelse(str_detect(name, '_mean'), 'mean', 'sd'),
         name = str_remove_all(name, '_mean|_sd')) %>%
  pivot_wider(id_cols = c(treat_map, slr, pid, name), names_from = type) %>%
  left_join(data %>%
              group_by(treat_map, slr, pid) %>%
              summarise(n=n())) %>%
  mutate(sd = sd/sqrt(n),
         name=dv.naming(name),
         slr = ifelse(slr==TRUE, 'In\nFlood Zone', 'Out of\nFlood Zone'),
         treat_map = ifelse(treat_map==T, 'Map Treatment', 'Control')) %>%
  ggplot() + 
  geom_point(aes(y = slr, x=mean, pch = treat_map, col = slr), size=2,
             position=position_dodge(width=.2)) + 
  geom_linerange(aes(y = slr, xmin=mean - 1.96*sd, xmax=mean + 1.96*sd, group = treat_map, col=slr),
                 size = 1, position = position_dodge(width=.2)) + 
  facet_grid(name~ pid) + 
  labs(x='Average Perceived Harm', y = '') +
  theme_classic() +
  scale_color_grey(guide='none') + 
  scale_x_continuous(breaks = c(0:3), limits = c(0,3),
                     labels = c('Not\nat all','Only a\nlittle','A moderate\namount','A great\ndeal')) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_main_means_pid.pdf'), width=11, height=8)


## FLD ZONE
data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr, FLD_ZONE,
         race_asian,race_black,race_hispanic,race_white,edu,gender,ideo, pid, rent) %>%
  filter(!is.na(slr) & FLD_ZONE %in% c("AE", "X")) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = ifelse(name %in% c('dv_us', 'dv_developing', 'dv_future'), 'dv_distant', name)) %>%
  group_by(name, slr, FLD_ZONE) %>%
  do(tidy(lm_robust(value ~ treat_map, 
                    fixed_effects = ~ geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          slr==FALSE & term=="treat_mapTRUE" ~ "Live Outside of\nSLR Zone",
                          slr==TRUE & term=="treat_mapTRUE" ~ "Live Within\nSLR Zone",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = case_when(name=='dv_personal' ~ "1. Personally",
                          name=='dv_community' ~ "2. Local\nCommunity",
                          name=='dv_distant' ~ "3. Distant\nGroups"),
         type = "Covariate Adjusted") %>%
  filter(!is.na(term) & !is.na(name) & !is.na(FLD_ZONE)) -> reg_results2

ggplot(reg_results2) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=FLD_ZONE, shape=FLD_ZONE),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  #scale_color_manual(values = c('blue', 'grey', 'red')) + 
  scale_color_viridis_d() + 
  labs(x='', y = '') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_zone.pdf'), width=11, height=5)

data %>%
  select(starts_with('treat_'), starts_with('dv_'),  geo, slr, FLD_ZONE) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  filter(treat_map==0 & FLD_ZONE %in% c("AE", "X")) %>%
  group_by(name, FLD_ZONE) %>%
  do(tidy(lm_robust(value ~ treat_traf, 
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = dv.naming(name)) %>%
  filter(!is.na(name) & !is.na(term)) %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=name, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=FLD_ZONE, shape=FLD_ZONE),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  #facet_wrap(~name, nrow=1) + 
  scale_color_viridis_d() + 
  labs(x='', y = 'Effect of Traffic Treatment', caption='Control group includes respondents who got neither traffic nor map treatments') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'traffic_zone.pdf'), width=11, height=8)

# own/rent
data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr,
         race_asian,race_black,race_hispanic,race_white,edu,gender,ideo, pid, rent) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  filter(!is.na(slr) & !is.na(rent) & !str_detect(name, 'policy|dk')) %>%
  group_by(name, rent, slr) %>%
  do(tidy(lm_robust(value ~ treat_map,
                    fixed_effects = ~ geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          slr==FALSE & term=="treat_mapTRUE" ~ "Out of SLR Zone",
                          slr==TRUE & term=="treat_mapTRUE" ~ "In SLR Zone",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = dv.naming(name),
         type = "Covariate Adjusted") %>%
  #bind_rows(reg_results %>%
  #            mutate(type = 'No Covariates')) %>%
  filter(!is.na(term) & !is.na(name) & !is.na(rent)) -> reg_results2

ggplot(reg_results2) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=rent, shape=rent),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  scale_color_manual(values = c('black', 'grey')) + 
  labs(x='', y = '') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_ownrent.pdf'), width=11, height=8)

data %>%
  filter(!is.na(rent)) %>%
  group_by(treat_map, slr, rent) %>%
  summarise_at(vars(starts_with('dv_') & !ends_with('_dk') & !contains('policy')), 
               list(mean = mean, sd = sd), na.rm=T) %>%
  filter(!is.na(slr)) %>%
  pivot_longer(cols = c(dv_personal_mean:dv_future_mean, dv_personal_sd:dv_future_sd)) %>%
  mutate(type = ifelse(str_detect(name, '_mean'), 'mean', 'sd'),
         name = str_remove_all(name, '_mean|_sd')) %>%
  pivot_wider(id_cols = c(treat_map, slr, rent, name), names_from = type) %>%
  left_join(data %>%
              group_by(treat_map, slr, rent) %>%
              summarise(n=n())) %>%
  mutate(sd = sd/sqrt(n),
         name=dv.naming(name),
         slr = ifelse(slr==TRUE, 'In\nFlood Zone', 'Out of\nFlood Zone'),
         treat_map = ifelse(treat_map==T, 'Map Treatment', 'Control')) %>%
  ggplot() + 
  geom_point(aes(y = slr, x=mean, pch = treat_map, col = slr), size=2,
             position=position_dodge(width=.2)) + 
  geom_linerange(aes(y = slr, xmin=mean - 1.96*sd, xmax=mean + 1.96*sd, group = treat_map, col=slr),
                 size = 1, position = position_dodge(width=.2)) + 
  facet_grid(name~ rent) + 
  labs(x='Average Perceived Harm', y = '') +
  theme_classic() +
  scale_color_grey(guide='none') + 
  scale_x_continuous(breaks = c(0:3), limits = c(0,3),
                     labels = c('Not\nat all','Only a\nlittle','A moderate\namount','A great\ndeal')) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_main_means_ownrent.pdf'), width=11, height=8)

data %>%
  select(starts_with('treat_'), starts_with('dv_'),  geo, slr, rent) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  filter(treat_map==0 & !is.na(rent)) %>%
  group_by(name, rent) %>%
  do(tidy(lm_robust(value ~ treat_traf, 
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          term=='treat_mapTRUE' ~ "Out of Flood Zone",
                          term=='slrTRUE' ~ "Sea Level Rise",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = dv.naming(name)) %>%
  filter(!is.na(name) & !is.na(term)) %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=name, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), col=rent, shape=rent),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  #facet_wrap(~name, nrow=1) + 
  scale_color_manual(values = c('black', 'grey')) + 
  labs(x='', y = 'Effect of Traffic Treatment', caption='Control group includes respondents who got neither traffic nor map treatments') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'traffic_ownrent.pdf'), width=11, height=8)

# climate belief
data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr,
         race_asian,race_black,race_hispanic,race_white,edu,gender,ideo, pid, rent, climate_belief) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  filter(!is.na(climate_belief) & !is.na(slr) & !str_detect(name, 'policy|dk')) %>%
  group_by(name, climate_belief, slr) %>%
  do(tidy(lm_robust(value ~ treat_map,
                    fixed_effects = ~ geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          slr==FALSE & term=="treat_mapTRUE" ~ "Out of SLR Zone",
                          slr==TRUE & term=="treat_mapTRUE" ~ "In SLR Zone",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = dv.naming(name),
         type = "Covariate Adjusted") %>%
  filter(!is.na(term) & !is.na(name)) -> reg_results2

ggplot(reg_results2) + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=term, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), 
                      col=climate_belief, pch=climate_belief),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  facet_wrap(~name, nrow=1) + 
  scale_color_grey() + 
  labs(x='', y = '', caption = 'All regressions contain fixed effects for location (CA, FL, NJ, VA).') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_climatebelief.pdf'), width=11, height=8)

data %>%
  filter(!is.na(climate_belief)) %>%
  group_by(treat_map, slr, climate_belief) %>%
  summarise_at(vars(starts_with('dv_') & !ends_with('_dk') & !contains('policy')), 
               list(mean = mean, sd = sd), na.rm=T) %>%
  filter(!is.na(slr)) %>%
  pivot_longer(cols = c(dv_personal_mean:dv_future_mean, dv_personal_sd:dv_future_sd)) %>%
  mutate(type = ifelse(str_detect(name, '_mean'), 'mean', 'sd'),
         name = str_remove_all(name, '_mean|_sd')) %>%
  pivot_wider(id_cols = c(treat_map, slr, climate_belief, name), names_from = type) %>%
  left_join(data %>%
              group_by(treat_map, slr, climate_belief) %>%
              summarise(n=n())) %>%
  mutate(sd = sd/sqrt(n),
         name=dv.naming(name),
         slr = ifelse(slr==TRUE, 'In\nFlood Zone', 'Out of\nFlood Zone'),
         treat_map = ifelse(treat_map==T, 'Map Treatment', 'Control')) %>%
  ggplot() + 
  geom_point(aes(y = slr, x=mean, pch = treat_map, col = slr), size=2,
             position=position_dodge(width=.2)) + 
  geom_linerange(aes(y = slr, xmin=mean - 1.96*sd, xmax=mean + 1.96*sd, group = treat_map, col=slr),
                 size = 1, position = position_dodge(width=.2)) + 
  facet_grid(name~ climate_belief) + 
  labs(x='Average Perceived Harm', y = '') +
  theme_classic() +
  scale_color_grey(guide='none') + 
  scale_x_continuous(breaks = c(0:3), limits = c(0,3),
                     labels = c('Not\nat all','Only a\nlittle','A moderate\namount','A great\ndeal')) + 
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20)) 
ggsave(file=paste0(out, 'map_means_climatebelief.pdf'), width=11, height=8)

data %>%
  select(starts_with('treat_'), starts_with('dv_'),  geo, slr, climate_belief) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  filter(!is.na(climate_belief)) %>%
  group_by(name, climate_belief) %>%
  do(tidy(lm_robust(value ~ treat_traf, 
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(term = case_when(term=='treat_trafTRUE' ~ "Traffic Treatment",
                          term=='treat_mapTRUE' ~ "Out of Flood Zone",
                          term=='slrTRUE' ~ "Sea Level Rise",
                          term=='treat_mapTRUE:treat_trafTRUE' ~ "Interaction"),
         name = dv.naming(name)) %>%
  filter(!is.na(name) & !is.na(term)) %>%
  ggplot() + 
  geom_hline(yintercept=0, lty=2) + 
  geom_pointrange(aes(x=name, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error), 
                      col=climate_belief, pch=climate_belief),
                  position=position_dodge(width=.2)) + 
  #geom_pointrange(aes(x=var, y=estimate, ymin = (estimate - 1.65*std.error), ymax=(estimate + 1.65*std.error)), fatten=2, size=1.1) + 
  coord_flip() +  
  #facet_wrap(~name, nrow=1) + 
  scale_color_grey() + 
  labs(x='', y = 'Effect of Traffic Treatment') +
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'traffic_climatebelief.pdf'), width=11, height=8)

# Map Distance ----------------------------------------------------------

data %>%
  #filter(geo=="CA") %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr, dist_min) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = dv.naming(name),
         treat_map = ifelse(treat_map==TRUE, 'Map Treatment', "Control")) %>%
  filter(!is.na(name)) %>%
  ggplot(aes(x=dist_min, y=value, col=treat_map)) + 
  geom_point(alpha=.5) + 
  geom_smooth() + 
  facet_wrap(~name) +
  geom_vline(xintercept=0, lty=2) + 
  xlim(-500, 500) + 
  scale_color_manual(values=c('blue', 'red')) + 
  labs(x='Distance from Sea Level Rise Line\n(- is Below Water)',
       y='Perceived Harm') + 
  theme_classic() +
  theme(legend.position='bottom', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'map_dist_raw.pdf'), width=11, height=8)

data %>%
  #filter(geo=="CA") %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr, dist_min) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = dv.naming(name),
         treat_map = ifelse(treat_map==TRUE, 'Map Treatment', "Control"),
         bucket = round(dist_min/50,0)*50) %>%
  group_by(bucket, name) %>%
  filter(n()>=15 & !is.na(name)) %>%
  do(tidy(lm_robust(value ~ treat_map,
                    fixed_effects = ~geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(below_zero = ifelse(bucket<=0, 'a', 'b'),
         sig = ifelse(p.value <= 0.05, 'sig', 'ns')) %>%
  ggplot() + 
  geom_pointrange(aes(x=bucket, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error),
                      col = sig, size=df)) + 
  geom_smooth(aes(x=bucket, y = estimate, group=below_zero), col='red') + 
  geom_vline(xintercept=0, lty=2) + 
  geom_hline(yintercept=0, lty=2) + 
  labs(x='Distance from Sea Level Rise Line\n(- is Below Water)',
       y='Treatment Effect of Map',
       caption = 'Points show regression coefficients at 50m binned intervals
       Error bars show 95/% confidence intervals. Black bars are significant at p < 0.05') + 
  scale_color_manual(values = c('grey', 'black')) + 
  facet_wrap(~name) + 
  theme_classic() +
  theme(legend.position='none', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'map_dist_bingreg.pdf'), width=11, height=8)


data %>%
  mutate(dv_other = (dv_community + dv_us + dv_developing + dv_future)/4) %>%
  select(starts_with('treat_'), dv_personal, dv_other, geo, slr, dist_min) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = case_when(name=='dv_personal' ~ '1. Personally',
                          name=='dv_other' ~ '2. Distant Groups'),
         treat_map = ifelse(treat_map==TRUE, 'Map Treatment', "Control"),
         bucket = round(dist_min/50,0)*50) %>%
  group_by(bucket, name) %>%
  filter(n()>=15 & !is.na(name)) %>%
  do(tidy(lm_robust(value ~ treat_map,
                    fixed_effects = ~geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(below_zero = ifelse(bucket<=0, 'a', 'b'),
         sig = ifelse(p.value <= 0.05, 'sig', 'ns')) %>%
  ggplot() + 
  geom_pointrange(aes(x=bucket, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error),
                      col = sig, size=df)) + 
  geom_smooth(aes(x=bucket, y = estimate, group=below_zero), col='red') + 
  geom_vline(xintercept=0, lty=2) + 
  geom_hline(yintercept=0, lty=2) + 
  labs(x='Distance from Sea Level Rise Boundary\n(Negative Values are Inside SLR Zone)',
       y='Treatment Effect of Map') + 
  scale_color_manual(values = c('grey', 'black')) + 
  facet_wrap(~name) + 
  theme_classic() +
  theme(legend.position='none', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'map_dist_binreg_distant.pdf'), width=11, height=8)

data %>%
  mutate(dv_other = (dv_community + dv_us + dv_developing + dv_future)/4) %>%
  select(starts_with('treat_'), dv_personal, dv_other, geo, slr, dist_min) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = case_when(name=='dv_personal' ~ 'Outcome: Sense of Personal Harm'),
         treat_map = ifelse(treat_map==TRUE, 'Map Treatment', "Control"),
         bucket = round(dist_min/50,0)*50) %>%
  group_by(bucket, name) %>%
  filter(n()>=15 & !is.na(name)) %>%
  do(tidy(lm_robust(value ~ treat_map,
                    fixed_effects = ~geo,
                    se_type = 'HC1',
                    data=.))) %>%
  mutate(below_zero = ifelse(bucket<=0, 'a', 'b'),
         sig = ifelse(p.value <= 0.05, 'sig', 'ns')) %>%
  ggplot() + 
  geom_pointrange(aes(x=bucket, y=estimate, ymin = (estimate - 1.96*std.error), ymax=(estimate + 1.96*std.error),
                      col = sig)) + 
  geom_smooth(aes(x=bucket, y = estimate, group=below_zero), col='red') + 
  geom_vline(xintercept=0, lty=2) + 
  geom_hline(yintercept=0, lty=2) + 
  labs(x='Distance from Sea Level Rise Boundary\n(Negative Values are Inside SLR Zone)',
       y='Treatment Effect of Map') + 
  scale_color_manual(values = c('grey', 'black')) + 
  facet_wrap(~name) + 
  theme_classic() +
  xlim(-500,500) + 
  theme(legend.position='none', legend.title=element_blank(), 
        text=element_text(size=20))
ggsave(file=paste0(out, 'map_dist_binreg_distant.pdf'), width=11, height=8)


data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr, dist_min) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = dv.naming(name),
         treat_map = ifelse(treat_map==TRUE, 'Map Treatment', "Control")) %>%
  filter(abs(dist_min) <= 250 & !is.na(name) & !is.na(slr)) %>%
  group_by(name) %>%
  do(tidy(lm_robust(value ~ treat_map + slr + treat_map*slr,
                    fixed_effects = ~geo,
                    se_type = 'HC1',
                    data=.)))

data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr, dist_min) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = dv.naming(name),
         treat_map = ifelse(treat_map==TRUE, 'Map Treatment', "Control")) %>%
  filter(abs(dist_min) <= 250 & !is.na(name) & !is.na(slr)) %>%
  group_by(name) %>%
  do(tidy(lm_robust(value ~ treat_map + slr + treat_map*dist_min,
                    fixed_effects = ~geo,
                    se_type = 'HC1',
                    data=.)))

data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr, dist_min) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = dv.naming(name),
         treat_map = ifelse(treat_map==TRUE, 'Map Treatment', "Control")) %>%
  filter(dist_min >= -250 & slr==T & !is.na(name) & !is.na(slr)) %>%
  group_by(name) %>%
  do(tidy(lm_robust(value ~ treat_map,
                    fixed_effects = ~geo,
                    se_type = 'HC1',
                    data=.)))

data %>%
  select(starts_with('treat_'), starts_with('dv_'), geo, slr, dist_min) %>%
  pivot_longer(cols=starts_with('dv_')) %>%
  mutate(name = dv.naming(name),
         treat_map = ifelse(treat_map==TRUE, 'Map Treatment', "Control")) %>%
  filter(dist_min <= 250 & slr==F & !is.na(name) & !is.na(slr)) %>%
  group_by(name) %>%
  do(tidy(lm_robust(value ~ treat_map,
                    fixed_effects = ~geo,
                    se_type = 'HC1',
                    data=.)))


# Manipulation and Balance ------------------------------------------------------

# manipulation check -- is treatment in right place on map
table(data$exp_emphasis,
      data$geo)
table(data$exp_emphasis,
      data$slr)

# do they find treatment credible?
table(data$SysRiskElsUncertain,
      data$slr) # more people likely to find treatment non-credible if they are shown being above water

data %>%
  mutate(credible_num = case_when(SysRiskElsUncertain=="Very credible" ~ 1,
                                  SysRiskElsUncertain=="Somewhat credible" ~ 0.66,
                                  SysRiskElsUncertain=="Not very credible" ~ 0.33,
                                  SysRiskElsUncertain=="Not at all credible" ~ 0)) %>%
lm_robust(credible_num ~ slr*pid, 
          se_type = 'HC1', data = .)

table(data$gwhuman, data$SysRiskElsUncertain) # also a lot of those who don't find treatment credible don't think GW is happening

#credibility for traffic
table(data$Q89)
table(data$gwhuman, data$Q89)

# how long do they spend on page
data %>%
  mutate(time_map = as.numeric(`Q63_Page Submit`),
         time_traf = as.numeric(`Q88_Page Submit`)) %>%
  select(time_map, time_traf) %>% 
  pivot_longer(cols = c(time_map, time_traf)) %>%
  mutate(name = case_when(name=='time_map' ~ 'Map Treatment',
                          name=='time_traf' ~ 'Traffic Treatment')) %>%
  ggplot() +
  geom_histogram(aes(value)) +
  #geom_vline(aes(xintercept=median(value, na.rm=T)), col='grey', lty=2) + 
  labs(x= 'Time Spent on Page (Seconds)') + 
  facet_wrap(~name) + 
  xlim(0,120) +
  theme_classic()
ggsave(file=paste0(out, 'hist_time_map.pdf'), width=8, height=11)

median(as.numeric(data$`Q63_Page Submit`),
       na.rm=T)

lm_robust(flood_impact ~ treat_traf + traf,
          se_type = 'HC1',
   data=data)
lm_robust(flood_impact ~ treat_map + slr,
          se_type = 'HC1',
          data=data)

# response rate as a function of distance from cutpoint
dist %>%
  mutate(dist_min = ifelse(str_detect(RecipientEmail, 'SLR'), dist_min*-1, dist_min),
         slr = ifelse(dist_min<=0, 1, 0)) %>%
  left_join(data %>%
              select(RecipientEmail) %>%
              mutate(response = 1)) %>%
  mutate(response = ifelse(is.na(response), 0, response)) -> response_rate

response_rate %>%
  mutate(name='Invited') %>%
  bind_rows(data %>%
              mutate(name='Responded')) %>%
  ggplot(aes(x=dist_min, fill=name)) + 
  geom_histogram(position='identity') + 
  xlim(-1000, 1000) + 
  labs(x='Distance from Sea Level Rise Line\n(- is Below Water)', y='', fill='') + 
  theme_classic()

response_rate %>%
  mutate(name='Invited') %>%
  bind_rows(data %>%
              mutate(name='Responded')) %>%
  ggplot(aes(x=dist_min, fill=name)) + 
  geom_histogram(position='identity') + 
  xlim(-1000, 1000) + 
  coord_cartesian(ylim=c(0,2000)) + 
  labs(x='Distance from Sea Level Rise Line\n(- is Below Water)', y='', 
       caption = 'Note: y axis is truncated for legibility', fill='') + 
  theme_classic()

ggplot(data=response_rate,
       aes(x=dist_min, y=response, group=slr)) + 
  geom_smooth(col='black') + 
  xlim(-1000, 1000) + 
  theme_classic()

response_rate %>%
  group_by(slr) %>%
  summarise(mean(response))

response_rate %>%
  mutate(bucket = round(dist_min/100, 0)*100) %>%
  group_by(bucket) %>%
  summarise(response = mean(response),
            n=n()) %>%
  filter(n>=10) %>%
  ggplot(aes(x=bucket, y=response)) +
  geom_vline(xintercept=0, lty=2) + 
  geom_col() + 
  coord_cartesian(xlim=c(-1000,2000)) + 
  labs(x = 'Distance from Sea Level Rise Boundary (Meters)',
       y = "Response Rate") + 
  theme_classic()
ggsave(paste0(out, 'response_rates.pdf'))
  
# balance around cutpoint
data %>%
  filter(abs(dist_min) <= 250) %>%
  mutate(male = ifelse(gender=='Male', 1, 0),
         homeowner = ifelse(rent=='Own my home', 1, 0),
         edu_hs = ifelse(edu %in% c('Less than High School', 'High School / GED'), 1, 0),
         edu_some_college = ifelse(edu %in% c('Some college', '2-year College Degree', 'Trade school'), 1, 0),
         edu_4yr_college = ifelse(edu=='4-year College Degree', 1, 0),
         edu_advanced = ifelse(edu %in% c('Masters Degree', 'Professional Degree (JD, MD)', 'Doctoral Degree'), 1, 0),
         zone_AE = ifelse(FLD_ZONE == "AE", 1, 0),
         zone_X = ifelse(FLD_ZONE == 'X', 1, 0)) %>%
  select(slr, race_asian,race_black, race_hispanic, race_white, edu_hs, edu_some_college, edu_4yr_college, edu_advanced, male, ideo_num, pid_num, homeowner, zone_AE, zone_X) %>%
  pivot_longer(cols = race_asian:zone_X) %>%
  group_by(name) %>%
  do(tidy(t.test(value ~ slr, data = .))) %>%
  mutate(name = case_when(name == 'edu_4yr_college' ~ 'Edu: college',
                          name == 'edu_advanced' ~ 'Edu: postgrad',
                          name == 'edu_hs' ~ 'Edu: some HS',
                          name == 'edu_some_college' ~ 'Edu: some college',
                          name == 'homeowner' ~ 'Homeowner',
                          name == 'ideo_num' ~ 'Ideology',
                          name == 'male' ~ 'Gender: Male',
                          name == 'pid_num' ~ 'Party ID',
                          name == 'race_asian' ~ 'Race: Asian',
                          name == 'race_black' ~ 'Race: Black',
                          name == 'race_hispanic' ~ 'Race/: Hispanic',
                          name == 'race_white' ~ 'Race: White')) %>%
           cbind(order = c(6,7,8,9,10,12,1,11,2,3,4,5)) %>%
  arrange(order) %>%
  select(` `=name, `In SLR` = estimate1, `Out SLR` = estimate2, `p value` = p.value) %>%
  mutate_all(function(x){round(x, 2)}) -> balance_test

stargazer(balance_test, summary=F, out = paste0(out, './balance.tex'), style='apsr', digits=2, rownames=F, 
          title = '\\textbf{Respondents on Either Side of Sea Level Rise Boundary Similar on Observed Characteristics}: Table shows mean values of demographic and political characteristics of respondents within 250m of sea level rise boundary. p value calculated from a t-test of difference in means.')

# balance between census and respondents
fl_shp <- st_read('./Data/shapefiles_matto/shapefiles_to_use/fl_sample_2_km_intersection.shp')
nj_shp <- st_read('./Data/shapefiles_matto/shapefiles_to_use/nj_sample_2_km_intersection.shp')
ca_shp <- st_read('./Data/shapefiles_matto/shapefiles_to_use/sf_bay_sample_2_km_intersection.shp')
va_shp <- st_read('./Data/shapefiles_matto/shapefiles_to_use/va_sample_2_km_intersection.shp')

tracts <- fl_shp %>%
  as.data.frame() %>%
  group_by(GEOID) %>%
  summarise() %>%
  bind_rows(nj_shp %>%
              as.data.frame() %>%
              group_by(GEOID) %>%
              summarise()) %>%
  bind_rows(ca_shp %>%
              as.data.frame() %>%
              group_by(GEOID) %>%
              summarise()) %>%
  bind_rows(va_shp %>%
              as.data.frame() %>%
              group_by(GEOID) %>%
              summarise())

acs <- read_csv('./Data/nhgis0036_csv/nhgis0036_ds244_20195_tract.csv') %>%
  mutate(GEOID = str_remove_all(GEOID, '14000US')) %>%
  inner_join(tracts)

census_demo <- acs %>%
  rowwise() %>% 
  #mutate(edu_total = sum(across(ALWGE002:ALWGE025)),
  #       race_total = sum(across(ALUKE002, ALUKE012))) %>%
  mutate(edu_total = ALWGE001,
         race_total = ALUKE001) %>%
  ungroup() %>%
  #group_by(STUSAB) %>%
  summarise(male = sum(ALT0E002)/sum(ALT0E002 + ALT0E026),
            homeowner = sum(ALZLE002)/sum(ALZLE001),
            race_black = sum(ALUKE004)/sum(race_total),
            race_asian = sum(ALUKE006)/sum(race_total),
            race_white = sum(ALUKE003)/sum(race_total),
            race_hispanic = sum(ALUKE012)/sum(race_total),
            edu_hs = sum(ALWGE017)/sum(edu_total),
            edu_some_college = (sum(ALWGE019+ALWGE020+ALWGE021))/sum(edu_total),
            edu_4yr_college = sum(ALWGE022)/sum(edu_total),
            edu_advanced = (sum(ALWGE023+ALWGE024+ALWGE025))/sum(edu_total))

data %>%
  group_by(geo) %>%
  mutate(male = ifelse(gender=='Male', 1, 0),
         homeowner = ifelse(rent=='Own my home', 1, 0),
         edu_hs = ifelse(edu %in% c('Less than High School', 'High School / GED'), 1, 0),
         edu_some_college = ifelse(edu %in% c('Some college', '2-year College Degree', 'Trade school'), 1, 0),
         edu_4yr_college = ifelse(edu=='4-year College Degree', 1, 0),
         edu_advanced = ifelse(edu %in% c('Masters Degree', 'Professional Degree (JD, MD)', 'Doctoral Degree'), 1, 0)) %>%
  select(slr, race_asian,race_black, race_hispanic, race_white, edu_hs, edu_some_college, edu_4yr_college, edu_advanced, male, ideo_num, pid_num, homeowner) %>%
  summarise_all(mean, na.rm=T) %>%
  select(geo, race_asian:male) %>%
  pivot_longer(data=., cols = race_asian:male) %>%
  pivot_wider(names_from=geo, values_from = value) %>%
  inner_join(census_demo %>%
              pivot_longer(data=., cols = male:edu_advanced)) %>% 
  mutate(name = case_when(name == 'edu_4yr_college' ~ 'Edu: college',
                          name == 'edu_advanced' ~ 'Edu: postgrad',
                          name == 'edu_hs' ~ 'Edu: some HS',
                          name == 'edu_some_college' ~ 'Edu: some college',
                          name == 'homeowner' ~ 'Homeowner',
                          name == 'ideo_num' ~ 'Ideology',
                          name == 'male' ~ 'Gender: Male',
                          name == 'pid_num' ~ 'Party ID',
                          name == 'race_asian' ~ 'Race: Asian',
                          name == 'race_black' ~ 'Race: Black',
                          name == 'race_hispanic' ~ 'Race/: Hispanic',
                          name == 'race_white' ~ 'Race: White')) %>%
  select(1,2,6,3,7,4,8,5,9)

census_demo <- acs %>%
  rowwise() %>% 
  #mutate(edu_total = sum(across(ALWGE002:ALWGE025)),
  #       race_total = sum(across(ALUKE002, ALUKE012))) %>%
  mutate(edu_total = ALWGE001,
         race_total = ALUKE001) %>%
  ungroup() %>%
  #group_by(STUSAB) %>%
  summarise(male_mean = sum(ALT0E002)/sum(ALT0E002 + ALT0E026),
            homeowner_mean = sum(ALZLE002)/sum(ALZLE001),
            race_black_mean = sum(ALUKE004)/sum(race_total),
            race_asian_mean = sum(ALUKE006)/sum(race_total),
            race_white_mean = sum(ALUKE003)/sum(race_total),
            race_hispanic_mean = sum(ALUKE012)/sum(race_total),
            edu_hs_mean = sum(ALWGE017)/sum(edu_total),
            edu_some_college_mean = (sum(ALWGE019+ALWGE020+ALWGE021))/sum(edu_total),
            edu_4yr_college_mean = sum(ALWGE022)/sum(edu_total),
            edu_advanced_mean = (sum(ALWGE023+ALWGE024+ALWGE025))/sum(edu_total),
            male_sd1 = sum(ALT0M002),
            homeowner_sd1 = sum(ALZLM002),
            race_black_sd1 = sum(ALUKM004),
            race_asian_sd1 = sum(ALUKM006),
            race_white_sd1 = sum(ALUKM003),
            race_hispanic_sd1 = sum(ALUKM012),
            edu_hs_sd1 = sum(ALWGM017),
            edu_some_college_sd1 = (sum(ALWGM019+ALWGM020+ALWGM021)),
            edu_4yr_college_sd1 = sum(ALWGM022),
            edu_advanced_sd1 = (sum(ALWGM023+ALWGM024+ALWGM025)),
            male_sd2 = sum(ALT0M002 + ALT0M026),
            homeowner_sd2 = sum(ALZLM001),
            race_black_sd2 = sum(ALUKM001),
            race_asian_sd2 = sum(ALUKM001),
            race_white_sd2 = sum(ALUKM001),
            race_hispanic_sd2 = sum(ALUKM001),
            edu_hs_sd2 = sum(ALWGM001),
            edu_some_college_sd2 = sum(ALWGM001),
            edu_4yr_college_sd2 = sum(ALWGM001),
            edu_advanced_sd2 = sum(ALWGM001),
            male_n = sum(ALT0E002 + ALT0E026),
            homeowner_n = sum(ALZLE001),
            race_black_n = sum(ALUKE001),
            race_asian_n = sum(ALUKE001),
            race_white_n = sum(ALUKE001),
            race_hispanic_n = sum(ALUKE001),
            edu_hs_n = sum(ALWGE001),
            edu_some_college_n = sum(ALWGE001),
            edu_4yr_college_n = sum(ALWGE001),
            edu_advanced_n = sum(ALWGE001)) %>%
  pivot_longer(cols = male_mean:edu_advanced_n) %>%
  mutate(stat = case_when(str_detect(name, '_mean') ~ 'mean',
                          str_detect(name, '_sd1') ~ 'sd1',
                          str_detect(name, '_sd2') ~ 'sd2',
                          T ~ 'n'),
         name = str_remove(name, '_sd\\d|_mean|_n')) %>%
  pivot_wider(values_from = value, names_from=stat) %>%
  mutate(sd1 = sd1/1.645,
         sd2 = sd2/1.645,
         sd = 1/n * sqrt(sd1^2 - mean^2*sd2^2)) %>%
  select(-sd1, -sd2)


library(BSDA)
data %>%
  #group_by(geo) %>%
  mutate(male = ifelse(gender=='Male', 1, 0),
         homeowner = ifelse(rent=='Own my home', 1, 0),
         edu_hs = ifelse(edu %in% c('Less than High School', 'High School / GED'), 1, 0),
         edu_some_college = ifelse(edu %in% c('Some college', '2-year College Degree', 'Trade school'), 1, 0),
         edu_4yr_college = ifelse(edu=='4-year College Degree', 1, 0),
         edu_advanced = ifelse(edu %in% c('Masters Degree', 'Professional Degree (JD, MD)', 'Doctoral Degree'), 1, 0),
         zone_x = ifelse(FLD_ZONE == 'X', 1, 0),
         zone_ae = ifelse(FLD_ZONE == 'AE', 1, 0)) %>%
  select(race_asian,race_black, race_hispanic, race_white, edu_hs, edu_some_college, edu_4yr_college, edu_advanced, male, homeowner, zone_x, zone_ae) %>%
  summarise_all(list(mean = function(x){mean(x, na.rm=T)}, sd = function(x){sd(x, na.rm=T)}, n = function(x){sum(!is.na(x))})) %>%
  pivot_longer(data=., cols = race_asian_mean:zone_ae_n) %>%
  mutate(stat = case_when(str_detect(name, '_mean') ~ 'mean',
                          str_detect(name, '_sd') ~ 'sd',
                          T ~ 'n'),
         name = str_remove(name, '_sd|_mean|_n')) %>%
  pivot_wider(values_from = value, names_from=stat) %>%
  inner_join(census_demo %>%
               bind_rows(bind_cols(fld_zone %>%
                                     summarise(zone_x_mean = mean(FLD_ZONE == 'X', na.rm=T),
                                               zone_ae_mean = mean(FLD_ZONE == 'AE', na.rm=T),
                                               zone_x_sd = sd(FLD_ZONE == 'X', na.rm=T),
                                               zone_ae_sd = sd(FLD_ZONE == 'AE', na.rm=T),
                                               zone_x_n = n(),
                                               zone_ae_n = n())) %>%
                           pivot_longer(cols = zone_x_mean:zone_ae_n) %>% 
                           mutate(stat = case_when(str_detect(name, '_mean') ~ 'mean',
                                                   str_detect(name, '_sd') ~ 'sd',
                                                   T ~ 'n'),
                                  name = str_remove(name, '_sd|_mean|_n')) %>%
                           pivot_wider(values_from = value, names_from=stat)),
             by = 'name') %>%
  mutate(p = tsum.test(mean.x = mean.x, s.x = sd.x, n.x=n.x,
                   mean.y = mean.y, s.y = sd.y, n.y=n.y)$p.value) %>%
  mutate(name = case_when(name == 'edu_4yr_college' ~ 'Edu: college',
                          name == 'edu_advanced' ~ 'Edu: postgrad',
                          name == 'edu_hs' ~ 'Edu: some HS',
                          name == 'edu_some_college' ~ 'Edu: some college',
                          name == 'homeowner' ~ 'Homeowner',
                          name == 'ideo_num' ~ 'Ideology',
                          name == 'male' ~ 'Gender: Male',
                          name == 'pid_num' ~ 'Party ID',
                          name == 'race_asian' ~ 'Race: Asian',
                          name == 'race_black' ~ 'Race: Black',
                          name == 'race_hispanic' ~ 'Race/: Hispanic',
                          name == 'race_white' ~ 'Race: White',
                          name == 'zone_x' ~ 'FEMA Zone: Moderate',
                          name == 'zone_ae' ~ 'FEMA Zone: High')) %>%
  select(` ` = name, `Sample` = mean.x, `Mean` = mean.y, `p.value`=p) %>%
  mutate_at(vars(2:4), function(x){round(x, 2)}) ->  balance_test2 

 
balance_test %>%
  full_join(balance_test2 %>%
            filter(!str_detect(` `, 'FEMA'))) %>%
  mutate_all(function(x){ifelse(is.na(x), "", paste(x))}) -> balance_combined

stargazer(balance_combined, summary=F, out = paste0('/Users/alexandersahn/Library/CloudStorage/Dropbox/Apps/Overleaf/sea_level_rise/figures/balance2.tex'), style='apsr', digits=2, rownames=F, 
          title = '\\textbf{Respondents on Either Side of Sea Level Rise Boundary Similar on Observed Characteristics; Whites, College Graduates, Homeowners Overrepresented in Sample Relative to Population}: First three columns of table show mean values of demographic and political characteristics of respondents within 250m of sea level rise boundary. p value calculated from a t-test of difference in means. Right 3 columns show mean values of demographic characteristics of survey respondents relative to census tracts in sampling frame. Census data come from 2015-2019 ACS estimates; respondent data is self-reported. p value calculated from a t-test of difference in means.')

# Descriptives ------------------------------------------------------

ggplot(data, aes(traf)) + 
  geom_histogram() + 
  labs(x='Minutes of Increased Traffic') + 
  geom_vline(xintercept=11, lty=2) + 
  theme_classic() + 
  theme(text=element_text(size=20))
ggsave(file=paste0(out, 'hist_traf.pdf'), width=8, height=11)

table(data$femaknowledge)/sum(!is.na(data$femaknowledge))
table(data$femaknowledge, data$FLD_ZONE)

table(data$FLD_ZONE)/sum(!is.na(data$FLD_ZONE))

data %>%
  mutate(fema_combo = ifelse(is.na(fematype), fematype_guess, fematype)) %>%
  mutate(guess_correct = case_when(femaknowledge=='I do not live in a flood zone' & FLD_ZONE=='X' ~ "In moderate zone, don't think in flood zone",
                                   femaknowledge=='I do not live in a flood zone' & FLD_ZONE!='X' ~ "In high risk zone, don't think in flood zone",
                                   fema_combo=='I live in a high risk flood zone (Zones that start with the letters A or V, called Special Flood Hazard Areas )' & FLD_ZONE=='X' ~ "In moderate zone, thinks in high risk",
                                   fema_combo=='I live in an Undetermined Risk Area (Zone D, where there are possible but undetermined flood hazards)' & FLD_ZONE=='X' ~ "In moderate zone, thinks in undertermined",
                                   fema_combo=='I live in a moderate or low risk zone (Zones B, C, or X,  called Non-Special Flood Hazard Areas)' & FLD_ZONE!='X' ~ "In high risk zone, thinks in moderate",
                                   fema_combo=='I live in an Undetermined Risk Area (Zone D, where there are possible but undetermined flood hazards)' & FLD_ZONE!='X' ~ "In high risk zone, thinks in undertermined",
                                   fema_combo=='I know I live in a  flood zone, but I do not know what type of flood zone I live in' ~ "Know in a flood zone but not which",
                                   fema_combo=='I live in a high risk flood zone (Zones that start with the letters A or V, called Special Flood Hazard Areas )' & FLD_ZONE %in% c('AE', "AH", "AO", "VE") ~ 'Knows in high risk',
                                   fema_combo=='I live in a moderate or low risk zone (Zones B, C, or X,  called Non-Special Flood Hazard Areas)' & FLD_ZONE=='X' ~ 'Knows in moderate risk, knows',
                                   is.na(FLD_ZONE) ~ NA)) %>%
  group_by(guess_correct) %>%
  summarise(n=n()) %>% 
  filter(!is.na(guess_correct)) %>%
  arrange(desc(n)) %>% 
  mutate(pct = n/sum(n))




