library(broom)
#####################################################
# Economic responses to climate impacts on biophysical yield
ANOVA<- Fig1_2 %>% group_by(scen0,region, crop,variable) %>%   
  mutate(valueR = log(value / lag(value))) %>% 
  ungroup() %>% separate(scen0, c("exp","ccs")) %>% 
  mutate(ccs = replace(ccs, ccs == "REF","FF"), GCM = substr(ccs,1,1),GGCM = substr(ccs,2,2)) %>%
  filter(is.na(valueR) == F,crop != "Aggregated") %>%
  mutate(year = as.character(year))

ANOVA1 <- ANOVA %>% filter(ccs != "FF") %>% dplyr::select(exp,GCM,GGCM,ccs,region,crop,year, variable, value, valueR) %>% 
  left_join(ANOVA %>% filter(ccs == "FF") %>% dplyr::select(exp,region,crop,year, variable, value, valueR) %>% 
              rename(value1 = value, valueR1 = valueR), by = c("exp", "region", "crop", "year", "variable")) %>%
  mutate(value0= value/value1,value = (valueR - valueR1)*100) %>% within(rm(value0, valueR, valueR1, value1))


#This code is commented out as it takes hours (e.g., ~8 hour depending on many factors) to run
##### ANOVA GOOD (takes hours to run)
# for (vv0 in seq(1,8)) {  tryCatch({
#   vv = c("price","area","yield","Byield","prod","import", "export", "consume")[vv0]
#   mod1_1 = aov(value ~ GCM + GGCM + GCM * region + GCM * year + GCM * crop +
#                  GGCM * region + GGCM * year + GGCM * crop + GCM * GGCM +
#                  GCM * GGCM * year +  GCM * GGCM * region + GCM * GGCM * crop +
#                  GCM * GGCM * crop * region + GCM * GGCM * crop * year + GCM * GGCM * region * year , data = ANOVA1 %>%
#                  filter(exp =="AEp1", variable == vv))
# 
# sink(paste0("output/ANOVA/",vv,"climateimpactsAEp1_ANOVA.txt"))
# print(summary(mod1_1))
# sink() },error = function(e){}) }


ANOVA2 <- ANOVA1 %>% dplyr::select(exp,ccs,region,crop,year, variable, value) %>%
  left_join(ANOVA1 %>% filter(variable == "Byield") %>% dplyr::select(exp,ccs,region,crop,year, Byield = value)) %>%
  filter(is.na(Byield) ==F, value != 0, Byield!=0)

#recent version of broom
beta_pool = ANOVA2 %>% nest_by(exp, ccs, region, crop, variable) %>%
  mutate(fit = list(lm(value ~ Byield, data = data)))

Coef_pool = beta_pool %>% summarise(tidy(fit)) %>% 
  left_join(beta_pool %>% summarise(glance(fit)) %>% dplyr::select(variable, exp, ccs, region, crop, r.squared)) %>% 
  mutate(p.value =round(p.value,3))%>% filter(term =="Byield") %>% 
  dplyr::select(variable, exp, ccs, region, crop, estimate,p.value, r.squared)

