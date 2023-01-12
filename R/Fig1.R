dir.create("output/plot/Fig1", showWarnings = FALSE)
scenLevel <- c("AEp1_HE", "AEp1_GE","AEp1_HL","AEp1_GL")
scenlabel <- c("HE","GE","HL","GL")
VariableLevel <- c("area","prod","consume","price","export","import","yield","Byield")
Variablelable <- c("Harvested area", "Production", "Consumption","Price","Export","Import","Realized yield","Biophysical yield")
CropLevel <- c("corn", "wheat", "rice", "oilcrop", "fibercrop", "misccrop", "othergrain", "palmfruit",   "root_tuber", "sugarcrop")
Croplabel <- c("Corn", "Wheat", "Rice", "Oil crops", "fibercrop", "misccrop",  "othergrain", "palmfruit",   "root_tuber", "sugarcrop")


Fig1_2A <- Fig1_2 %>% 
  separate(scen0, c("exp","ccs"), remove = F) %>% 
  filter(ccs != "REF") %>% left_join(
    Fig1_2 %>% separate(scen0, c("exp","ccs")) %>% 
      filter(ccs == "REF") %>% within(rm(ccs)) %>% 
      rename(REF = value)) %>% 
  mutate(value1 = log(value / REF)*100, value2 = (value/REF-1)*100, value = (value/REF)) %>% within(rm(REF)) %>% 
  group_by(scen0, region, crop, variable) %>% 
  mutate(valueR = value1 - lag(value1)) %>% 
  ungroup() %>% filter(exp == "AEp1")

Fig1_2A$variable <-  factor(Fig1_2A$variable, levels = VariableLevel, labels = Variablelable)
Fig1_2A$ccs <-  factor(Fig1_2A$ccs, levels = scenlabel)

qt = 0.9
Fig1_4 <- Fig1_2A%>% filter(crop != "Aggregated") %>%
  group_by(scen0, exp, ccs, variable, year) %>% 
  summarise(value = mean(value)) %>% 
  mutate(region = "average", crop = "Aggregated") %>% ungroup()  %>% bind_rows(
    Fig1_2A%>% filter(crop != "Aggregated") %>%
      group_by(variable, year) %>% 
      summarise(value = mean(value)) %>% ungroup() %>%
      mutate(region = "Waverage", crop = "Aggregated", ccs = "all")
  ) %>%  bind_rows(
    Fig1_2A %>% filter(crop != "Aggregated") %>%
      group_by(scen0, variable, year) %>% 
      mutate(val01 = quantile(value, probs = 1-qt),
             val09 = quantile(value, probs = qt)) %>%
      filter(value <= val09, value >= val01) %>%
      ungroup() %>% within(rm(val09,val01)) %>%
      dplyr::select(scen0, exp, ccs, variable, region, crop, year, value)) %>%
  bind_rows(
    Fig1_2A %>% filter(crop != "Aggregated") %>%
      group_by(scen0, exp, ccs, variable, year) %>% 
      summarise(value = quantile(value, probs = 0.5)) %>%
      ungroup() %>% mutate(region = "median", crop = "median") )

Fig1_5 <- Fig1_4 %>% filter(year == 2050, ccs %in% scenlabel[1]) %>% 
  bind_rows(Fig1_4 %>% filter(year == 2050, ccs %in% scenlabel[c(1,2)]) %>% mutate(year = 2051)) %>%
  bind_rows(Fig1_4 %>% filter(year == 2050, ccs %in% scenlabel[c(2,3)]) %>% mutate(year = 2052)) %>%
  bind_rows(Fig1_4 %>% filter(year == 2050, ccs %in% scenlabel[c(3,4)]) %>% mutate(year = 2053)) %>%
  bind_rows(Fig1_4 %>% filter(year == 2050, ccs %in% scenlabel[c(4)]) %>% mutate(year = 2054)) %>%
  bind_rows(Fig1_4 %>% filter(year == 2050, ccs == "all") %>% mutate(year = 2050)) %>% 
  bind_rows(Fig1_4 %>% filter(year == 2050, ccs == "all") %>% mutate(year = 2054)) %>% 
  mutate(RC = paste0(ccs, region,crop))
Fig1_5$ccs <-  factor(Fig1_5$ccs, levels = scenlabel)

xscale = scale_x_continuous(expand = c(0, 0),limits = c(2010, 2050),breaks = seq(2015, 2045, 10)) #
yscale = scale_y_continuous(expand = c(0, 0))
color4 <- c( rgb(24,113,185,max=255),rgb(204,28,27,max=255),rgb(19,140,71,max = 255),rgb(72,75,77,max = 255),rgb(101,86,164,max=255))
sty_reg <- c( scale_color_manual(values= c(color4)),
              scale_alpha_manual(values = c(rep(1,5))),
              scale_size_manual(values = c(rep(1,5),0.8)) ,
              scale_linetype_manual(values = c(1,1,1,1,5)))



#vv0 = 4
for (vv0 in  seq(1,8)) {  tryCatch({
  
  if (vv0 == 4) {LP = c(.24,0.77); leftmargin = 6} else {LP = "none"; leftmargin = 6} #legend position LP = c(.24,0.77)   c(.24,0.3)
  
  if (vv0 == 1|vv0 ==2|vv0 == 3) {limit0 <- c(0.6,1.3) ; BK = seq(0.7,1.2,0.1);limit1<-c(-10,10); BK1 = c(-5,0,5)} else if #area
  (vv0 ==4) {limit0 <- c(0.6,2.4); BK = seq(0.8, 2.2, 0.2);limit1<-c(-24,24); BK1 = c(-12,0,12)} else if  #price
  (vv0 ==5 |vv0 ==6) {limit0 <- c(0,3); BK = seq(0.2, 2.8, 0.4);limit1<-c(-70,70); BK1 = c(-35,0,35)} else if #export, import
  (vv0 ==7 |vv0 ==8) {limit0 <- c(0.5,1.2) ; BK = seq(0.6,1.1,0.1);limit1<-c(-10,10); BK1 = c(-5,0,5)} else # realized yield
  {limit0 <- c(-500,500); BK = seq(-200,120, 50);limit1<-c(-50,50); BK1 = c(-10,0,10) }
  
  vv = Variablelable[vv0]; AA = chartr("12345678", "abcdefgh", vv0)
  assign(paste0("Fig1_plt_",vv), Fig1(data = Fig1_2A %>% filter(variable == vv), 
                                      leftmargin = 6, LP = LP, vv = Variablelable[vv0], group = "ccs"))
  
  png(paste0("output/plot/Fig1/Fig1_",vv,".png"), width = 3000, height = 4000, res = 600)
  print(get(paste0("Fig1_plt_",vv)))
  dev.off()
  
}, error = function(e){}) }


T1 <-ggplot() + annotate("text", x= 0, y=c(0.65,2.4), label= c("Interannual \nimpact (%)", "Climate impact \n(2010 = 1)"),angle = 90, size = 5) + 
  theme_bw() +  scale_y_continuous(expand = c(0,0),limits = c(0,4)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) ;T1
T2 <-ggplot() + annotate("text", y = 0, x=c(1.55), label= c("Year"),angle = 0, size = 5) + 
  theme_bw() + scale_x_continuous(expand = c(0,0),limits = c(0,3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),plot.margin = margin(t = 0, r = 0, b = 0, l = 0));T2 

GG<- plot_grid(`Fig1_plt_Harvested area`,Fig1_plt_Production,`Fig1_plt_Consumption`, Fig1_plt_Price, nrow = 1)
GG1 <- plot_grid(T1, GG, nrow = 1, rel_widths = c(0.04, 1),align="hv")
GG2 <- plot_grid(GG1, T2, nrow = 2, rel_heights = c(1, 0.04),align="hv")
png(paste0("output/plot/Fig1/Fig1_1.png"), width = 9000, height = 3200, res = 600)
print(GG2)
dev.off()



GG<- plot_grid(`Fig1_plt_Export`,Fig1_plt_Import,`Fig1_plt_Realized yield`,`Fig1_plt_Biophysical yield`, nrow = 1)
GG1 <- plot_grid(T1, GG, nrow = 1, rel_widths = c(0.04, 1),align="hv")
GG2 <- plot_grid(GG1, T2, nrow = 2, rel_heights = c(1, 0.04),align="hv")
png(paste0("output/plot/Fig1/Fig1_2",".png"), width = 9000, height = 3200, res = 600)
print(GG2)
dev.off() 



T1 <-ggplot() + annotate("text", x= 0, y=c(0.65, 2.4, 4.65, 6.4), label= c("Interannual \nimpact (%)", "Climate impact \n(2010 = 1)",
                                                                           "Interannual \nimpact (%)", "Climate impact \n(2010 = 1)"),
                         angle = 90, size = 5) + 
  theme_bw() +  scale_y_continuous(expand = c(0,0),limits = c(0,8)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) ;T1

T2 <-ggplot() + annotate("text", y = 0, x=c(1.55), label= c("Year"),angle = 0, size = 5) + 
  theme_bw() + scale_x_continuous(expand = c(0,0),limits = c(0,3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),plot.margin = margin(t = 0, r = 0, b = 0, l = 0));T2 

GG<- plot_grid(`Fig1_plt_Harvested area`,Fig1_plt_Production,`Fig1_plt_Consumption`, Fig1_plt_Price,
               `Fig1_plt_Export`,Fig1_plt_Import,`Fig1_plt_Realized yield`,`Fig1_plt_Biophysical yield`, 
               nrow = 2, ncol = 4)
GG1 <- plot_grid(T1, GG, nrow = 1, rel_widths = c(0.05, 1),align="hv")
GG2 <- plot_grid(GG1, T2, nrow = 2, rel_heights = c(1, 0.04),align="hv")
png(paste0("output/plot/Fig1/Fig2",".png"), width = 9000, height = 6000, res = 600)
print(GG2)
dev.off() 



