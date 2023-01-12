dir.create("output/plot/Fig3", showWarnings = FALSE)

scenlabel <- c("HE","GE","HL","GL")
library(broom)
library(scales)
show_col(hue_pal()(5))
col5 <- c("#F8766D","#00BF7D","#B79F00", "#00B0F6", "gray", "#E76BF3") 


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


ANOVA2 <- ANOVA1 %>% dplyr::select(exp,ccs,region,crop,year, variable, value) %>%
  left_join(ANOVA1 %>% filter(variable == "Byield") %>% dplyr::select(exp,ccs,region,crop,year, Byield = value)) %>%
  filter(is.na(Byield) ==F, value != 0, Byield!=0)

#old version of broom
#beta_pool = ANOVA2 %>% group_by(exp, ccs, region, crop, variable) %>%
#  do(fit = lm(value ~ Byield, data = .))
# Coef_pool = tidy(beta_pool, fit) %>% 
#   left_join(glance(beta_pool,fit) %>% dplyr::select(variable, exp, ccs, region, crop, r.squared)) %>% 
#   mutate(p.value =round(p.value,3))%>% filter(term =="Byield") %>% 
#   dplyr::select(variable, exp, ccs, region, crop, estimate,p.value, r.squared)


#recent version of broom
beta_pool = ANOVA2 %>% nest_by(exp, ccs, region, crop, variable) %>%
 mutate(fit = list(lm(value ~ Byield, data = data)))

Coef_pool = beta_pool %>% summarise(tidy(fit)) %>% 
  left_join(beta_pool %>% summarise(glance(fit)) %>% dplyr::select(variable, exp, ccs, region, crop, r.squared)) %>% 
  mutate(p.value =round(p.value,3))%>% filter(term =="Byield") %>% 
  dplyr::select(variable, exp, ccs, region, crop, estimate,p.value, r.squared)



exp0<- c("AEp1", "PF");
lab = c("a Adaptive expectation","b Perfect foresight")
#change scale here to change truncation
for (scale in c("truncate", "all")) {
  if (scale == "truncate"){yscale = scale_y_continuous(expand = c(0, 0),limits = c(-1,1),breaks = c(-0.5,0,0.5))}else
    if (scale == "all"){yscale = scale_y_continuous(expand = c(0, 0),limits = c(-13,13))}
  #change n here to change expectation
for (n in seq(1,2)) {
  
  #c(.11,0.8),  c(.25,0.82),
  
  if (n == 1) {LP = c(.25,0.82)} else {LP = "none"} 
  
  Fig2_0 <- Coef_pool %>% filter(exp %in% exp0[n], variable %in% c("area","prod", "export" ,"price", "import", "consume")) %>% 
    filter( interaction(region,crop) !="EU-15.palmfruit")
  Fig2_0$variable <-  factor(Fig2_0$variable, levels =  c("area","prod", "export", "price", "import","consume"), 
                             labels = c("Harvested area","Production", "Export","Price", "Import","Consumption"))
  
  Fig2_1 <- Fig2_0 %>%
    mutate(r = abs(estimate)/estimate  *r.squared^0.5, sd = estimate/r)%>% group_by(exp, ccs, variable) %>%
    mutate(value01 = quantile(sd, probs = 0.1),
           value09 = quantile(sd, probs = 0.9)) %>%
    filter(sd >= value01, sd <= value09) %>% ungroup() %>% within(rm(value01, value09))
  
  
  Fig2_2 <- ANOVA1 %>% filter(is.na(value) ==F, value != 0, interaction(region,crop) !="EU-15.palmfruit") %>%  
    filter(exp ==exp0[n],  variable %in% c("Byield","area","prod", "export" ,"price", "import", "consume")) %>%
    group_by(exp,ccs,region,crop,variable) %>% summarise(sd = sd(value), mean = mean(value)) %>% ungroup() %>%
    group_by(exp,  variable) %>% 
    summarise(Msd = mean(sd)) %>% ungroup()%>% mutate(RelMsd = Msd/Msd[variable =="Byield"]) %>%
    filter(variable != "Byield") #unweighted average
  
  ####################slope
  df <- ANOVA1 %>% filter(is.na(value) ==F, value != 0, interaction(region,crop) !="EU-15.palmfruit") %>%  
    filter(exp ==exp0[n],  variable %in% c("Byield","area","prod", "export" ,"price", "import", "consume")) %>%
    group_by(exp,ccs,region,crop,variable) %>% summarise(sd = sd(value)) %>%
    ungroup()
  Fig2_2 <- df %>% filter(variable != "Byield") %>% 
    left_join(df %>% filter(variable == "Byield") %>% rename(bsd = sd) %>% within(rm(variable)), 
              by = c("exp", "ccs", "region", "crop")) %>% filter(is.na(bsd) == F) %>%
    mutate(Relsd = sd / bsd) %>% 
    group_by(exp, variable) %>% 
    summarise(RelMsd = mean(Relsd)) %>% ungroup() 
  ##################
  
  Fig2_3 <- Fig2_2 %>% mutate(r = 1) %>% 
    bind_rows(Fig2_2 %>% mutate(RelMsd = - RelMsd, r = -1)) %>%
    bind_rows(Fig2_2 %>% mutate(r = -1 / RelMsd, RelMsd = -1)) %>%
    bind_rows(Fig2_2 %>% mutate(r = 1 / RelMsd, RelMsd = 1)) 
  
  
  Fig2_3$variable <-  factor(Fig2_3$variable, levels =  c("area","prod", "export", "price", "import","consume"), 
                             labels = c("Harvested area","Production", "Export","Price", "Import","Consumption"))
  
  xscale = scale_x_continuous(expand = c(0, 0),limits = c(-1,1), breaks = c(-0.5,0,0.5)) #

  A1 <- ggplot(Fig2_1) + #facet_grid(cols = vars(ccs)) +
    geom_point(aes(x= r, y =estimate, group = variable, color =  variable), alpha = 0.2,  size = 1) + 
    scale_color_manual(values=c(col5))+
    geom_line(data = Fig2_3, aes(x=r, y=RelMsd, group = variable, color = variable),size = 1, alpha = 0.8)+
    geom_hline(yintercept=0, linetype=1, color = "gray", size = 0.5) + 
    geom_vline(xintercept=0, linetype=1, color = "gray", size =0.5) +
    #geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1),linetype=2, color = "gray", size = 0.5) +
    #geom_segment(aes(x = 0, y = -1, xend = -1, yend = -1),linetype=2, color = "gray", size = 0.5) +
    geom_abline(intercept = 0, slope = 1, color="black", linetype=2, size=1, alpha = 0.5) + xscale+ yscale + 
    theme_bw() + theme0 + theme_leg +   theme(axis.ticks = element_blank(), legend.position = LP,  
                                              legend.key.size = unit(1, "cm"),
                                              legend.key.height=unit(1,"line"),
                                              legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 12),
                                              axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 5)),
                                              axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 5), vjust= 0.5),
                                              plot.margin = margin(t = 0, r = 10, b = 0, l = 0),
                                              axis.title = element_blank()) + 
    labs(y ="Interannual economic response \nto biophysical shock" , x = "Correlation coefficient") ; A1
  
  #A2 has 10-90 perc. for whisker!
  A2 <- ggplot(Fig2_0 %>% group_by(exp, variable) %>% 
                 summarise(r01 = quantile(r.squared, probs = 0.1),
                           r25 = quantile(r.squared, probs = 0.25),
                           r05 = quantile(r.squared, probs = 0.5),
                           r75 = quantile(r.squared, probs = 0.75),
                           r09 = quantile(r.squared, probs = 0.9)) %>% ungroup(),
               aes(x =reorder(variable, desc(variable)))) + # facet_grid(cols = vars(ccs)) +
    geom_boxplot(aes(ymin = r01, lower = r25, middle = r05, upper = r75, 
                     ymax = r09, fill =  variable), alpha=0.7, stat = "identity")+ coord_flip() +
    stat_summary(data = Fig2_0, fun.y=mean, aes(x =reorder(variable, desc(variable)), y =r.squared), geom="point", shape=18, size=1) +
    theme_bw() + theme0 + theme_leg +   theme(axis.ticks = element_blank(), legend.position = "none") + 
    scale_y_continuous(expand = c(0, 0),limits = c(-1,1)) + 
    scale_fill_manual(values=c(col5))  +   
    theme(panel.border = element_blank(), 
          axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),axis.title.x = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),plot.margin = margin(t = 0, r = 10, b = 0, l = 0));A2
  
  A3 <-ggplot() + annotate("text", y = 0, x=c(1.5), label= lab[n],angle = 0, size = 6, fontface = "bold") + 
    theme_bw() + scale_x_continuous(expand = c(0,0),limits = c(0,3)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
          axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),axis.title.x = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),plot.margin = margin(t = 0, r = 0, b = 0, l = 0));A3
  
  
  assign(paste0("plt_",exp0[n]), plot_grid(A3, A2, A1 + theme(axis.title.y = element_blank(),axis.title.x = element_blank()),
                                           ncol = 1,  nrow = 3, rel_heights = c(0.1, 0.11, 0.9), align="v",greedy = F))
  png(paste0("output/plot/Fig3/Fig3_", scale, "_", exp0[n], ".png"), width = 3000, height = 3200, res = 600)
  print(get(paste0("plt_",exp0[n])))
  dev.off() 
  
}


T1 <-ggplot() + annotate("text", x= 0, y=c(1.35), label= c("Interannual economic response \nagainst biophysical shock"),
                         angle = 90, size = 5) + 
  theme_bw() +  scale_y_continuous(expand = c(0,0),limits = c(0,3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) ;T1
T2 <-ggplot() + annotate("text", y = 0, x=c(1.65), label= c("Correlation coefficient between biophysical shock and economic response"),angle = 0, size = 5) + 
  theme_bw() + scale_x_continuous(expand = c(0,0),limits = c(0,3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),plot.margin = margin(t = 0, r = 0, b = 0, l = 0));T2 

GG2<- plot_grid(T1, plt_AEp1, NULL,plt_PF, ncol = 4,  nrow = 1, rel_widths = c(0.08, 0.5,0.01,0.5), align="h",greedy = F);GG2
GG3<- plot_grid(GG2, T2, ncol = 1,  nrow = 2, rel_heights = c(0.9,0.06), align="v",greedy = F);GG3

png(paste0("output/plot/Fig3/", "Fig3_", scale, "_.png"), width = 6000, height = 3400, res = 600)
print(GG3)
dev.off() 

}

#################################################
# figure S9 
#scenarios
exp0<- c("AEp1", "PF");
Fig2_0S <- Coef_pool %>% filter(exp %in% exp0, variable %in% c("area","prod", "export" ,"price", "import", "consume")) %>% 
  filter( interaction(region,crop) !="EU-15.palmfruit")

Fig2_0S$variable <-  factor(Fig2_0S$variable, levels =  c("area","prod", "export", "price", "import","consume"), 
                            labels = c("Harvested area","Production", "Export","Price", "Import","Consumption"))
Fig2_0S$exp <-  factor(Fig2_0S$exp, levels =  exp0, 
                       labels = c("Adaptive expectation","Perfect foresight"))

Fig2_0S$ccs <-  factor(Fig2_0S$ccs, levels = scenlabel)

Fig2_1S <- Fig2_0S %>%
  mutate(r = abs(estimate)/estimate  *r.squared^0.5, sd = estimate/r)%>% group_by(exp, ccs, variable) %>%
  mutate(value01 = quantile(sd, probs = 0.1),
         value09 = quantile(sd, probs = 0.9)) %>%
  filter(sd >= value01, sd <= value09) %>% ungroup() %>% within(rm(value01, value09))

###
Fig2_2S <- ANOVA1 %>% filter(is.na(value) ==F, value != 0) %>%  
  filter(exp %in% exp0,  variable %in% c("Byield","area","prod", "export" ,"price", "import", "consume")) %>%
  group_by(exp,ccs,region,crop,variable) %>% summarise(sd = sd(value), mean = mean(value)) %>% ungroup() %>%
  group_by(exp,ccs, variable) %>% 
  summarise(Msd = mean(sd)) %>% ungroup()%>% mutate(RelMsd = Msd/Msd[variable =="Byield"]) %>%
  filter(variable != "Byield") #unweighted average
###
df <- ANOVA1 %>% filter(is.na(value) ==F, value != 0, interaction(region,crop) !="EU-15.palmfruit") %>%  
  filter(exp %in% exp0,  variable %in% c("Byield","area","prod", "export" ,"price", "import", "consume")) %>%
  group_by(exp,ccs,region,crop,variable) %>% summarise(sd = sd(value)) %>%
  ungroup()
Fig2_2S <- df %>% filter(variable != "Byield") %>% 
  left_join(df %>% filter(variable == "Byield") %>% rename(bsd = sd) %>% within(rm(variable)), 
            by = c("exp", "ccs", "region", "crop")) %>% filter(is.na(bsd) == F) %>%
  mutate(Relsd = sd / bsd) %>% 
  group_by(exp, ccs, variable) %>% 
  summarise(RelMsd = mean(Relsd)) %>% ungroup() 

Fig2_3S <- Fig2_2S %>% mutate(r = 1) %>% 
  bind_rows(Fig2_2S %>% mutate(RelMsd = - RelMsd, r = -1)) %>%
  bind_rows(Fig2_2S %>% mutate(r = -1 / RelMsd, RelMsd = -1)) %>%
  bind_rows(Fig2_2S %>% mutate(r = 1 / RelMsd, RelMsd = 1)) 

Fig2_3S$variable <-  factor(Fig2_3S$variable, levels =  c("area","prod", "export", "price", "import","consume"), 
                            labels = c("Harvested area","Production", "Export","Price", "Import","Consumption"))
Fig2_3S$exp <-  factor(Fig2_3S$exp, levels =  exp0, 
                       labels = c("Adaptive expectation","Perfect foresight"))
Fig2_3S$ccs <-  factor(Fig2_3S$ccs, levels = scenlabel)

xscale = scale_x_continuous(expand = c(0, 0),limits = c(-1,1), breaks = c(-0.5,0,0.5)) #
yscale = scale_y_continuous(expand = c(0, 0),limits = c(-1,1),breaks = c(-0.5,0,0.5))
A1 <- ggplot(Fig2_1S) + facet_grid(cols = vars(ccs), rows = vars(exp)) +
  geom_point(aes(x= r, y =estimate, group = variable, color =  variable), alpha = 0.2,  size = 1) + 
  scale_color_manual(values=c(col5))+ guides(color = guide_legend(nrow = 1)) +
  geom_line(data = Fig2_3S, aes(x=r, y=RelMsd, group = variable, color = variable),size = 1, alpha = 0.8)+
  geom_hline(yintercept=0, linetype=1, color = "gray", size = 0.5) + 
  geom_vline(xintercept=0, linetype=1, color = "gray", size =0.5) +
  #geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1),linetype=2, color = "gray", size = 0.5) +
  #geom_segment(aes(x = 0, y = -1, xend = -1, yend = -1),linetype=2, color = "gray", size = 0.5) +
  geom_abline(intercept = 0, slope = 1, color="black", linetype=2, size=1, alpha = 0.5) + xscale+ yscale + 
  theme_bw() + theme0 + theme_leg +   theme(axis.ticks = element_blank(), legend.position ="top", #  c(.25,0.82), # #
                                            legend.key.size = unit(1, "cm"),
                                            legend.key.height=unit(1,"line"),
                                            legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 12),
                                            axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 5)),
                                            axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 5), vjust= 0.5),
                                            plot.margin = margin(t = 10, r = 10, b = 10, l = 10)) +
  labs(y ="Beta coefficient" , x = "Correlation coefficient") ;A1

png(paste0("output/plot/Fig3/","FigS9",".png"), width = 7000, height = 4000, res = 600)
print(A1)
dev.off() 


##############################################################
#Interannual responses  ; 
exp0<- c("AEp1", "PF");

Fig2ED <- Coef_pool %>% filter(exp %in% exp0, variable %in% c("area","prod", "export" ,"price", "import", "consume")) %>% 
  group_by(variable, exp, ccs) %>%
  summarise(b01 = quantile(estimate, probs = 0.1),
            b09 = quantile(estimate, probs = 0.9)) %>% ungroup() %>%
  group_by(variable,exp) %>% summarise(b01 = min(b01), b09 = max(b09))%>% left_join(
    Coef_pool %>% filter(exp %in% exp0, variable %in% c("area","prod", "export" ,"price", "import", "consume")) %>% 
      group_by(variable, exp) %>% summarise(b05 = mean(estimate))
  )
Fig2ED$variable <-  factor(Fig2ED$variable, levels =  c("area","prod", "export", "price", "import","consume"), 
                           labels = c("Harvested area","Production", "Export","Price", "Import","Consumption"))

Fig2ED$exp <-  factor(Fig2ED$exp, levels =  exp0, 
                      labels = c("Adaptive expectation","Perfect foresight"))

Fig2ED1 <- Fig2ED %>% mutate(Byield = 1) %>% 
  bind_rows(Fig2ED %>% mutate(Byield = -1, b01 = -b01, b05 = -b05, b09 = -b09)) %>%
  bind_rows(Fig2ED %>% mutate(Byield = -1/b05, b01 = -b01/b05, b09 = -b09/b05, b05 = -1)) %>%
  bind_rows(Fig2ED %>% mutate(Byield = 1/b05, b01 = b01/b05, b09 = b09/b05, b05 = 1)) %>%
  bind_rows(Fig2ED %>% mutate(Byield = -1/b01, b05 = -b05/b01, b09 = -b09/b01, b01 = -1)) %>%
  bind_rows(Fig2ED %>% mutate(Byield = 1/b01, b05 = b05/b01, b09 = b09/b01, b01 = 1)) %>%
  bind_rows(Fig2ED %>% mutate(Byield = -1/b09, b01 = -b01/b09, b05 = -b05/b09, b09 = -1)) %>%
  bind_rows(Fig2ED %>% mutate(Byield = 1/b09, b01 = b01/b09, b05 = b05/b09, b09 = 1))

xscale = scale_x_continuous(expand = c(0, 0), breaks = c(-0.5,0,0.5)) #
yscale = scale_y_continuous(expand = c(0, 0), breaks = c(-0.5,0,0.5))

A1 <- ggplot(Fig2ED1) + #facet_grid(cols = vars(ccs), rows = vars(exp)) +
  facet_grid(cols = vars(exp)) +
  geom_ribbon(aes(x = Byield, ymin = b01, ymax = b09, fill = variable),alpha = 0.2) +
  scale_fill_manual(values=c(col5))  +
  geom_line(aes(x= Byield, y =b05, group = variable, color =  variable), alpha = 1,  size = 1) + 
  scale_color_manual(values=c(col5))+
  geom_hline(yintercept=0, linetype=1, color = "gray", size = 0.5) + 
  geom_vline(xintercept=0, linetype=1, color = "gray", size =0.5) +
  coord_cartesian(ylim=c(-1, 1), xlim = c(-1,1)) +
  xscale+ yscale + guides(fill = guide_legend(nrow = 2)) +
  theme_bw() + theme0 + theme_leg +   theme(axis.ticks = element_blank(), legend.position ="top", #  c(.25,0.82), # #
                                            legend.key.size = unit(1, "cm"),
                                            legend.key.height=unit(1,"line"),
                                            panel.spacing = unit(2, "lines"),
                                            legend.text = element_text(margin = margin(l = -25,t=5, b=0), size = 12),
                                            axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 5)),
                                            axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 5), vjust= 0.5),
                                            plot.margin = margin(t = 10, r = 10, b = 10, l = 10) ) +
  labs(y ="Interannual economic responses (%)" , x = "Biophysical yield shocks (%)") ;A1

png(paste0("output/plot/Fig3/","Fig10_response_agg",".png"), width = 4900, height = 3000, res = 600)
print(A1)
dev.off() 

########### all ccs
Fig2ED <- Coef_pool %>% filter(exp %in% exp0, variable %in% c("area","prod", "export" ,"price", "import", "consume")) %>% 
  group_by(variable, exp, ccs) %>%
  summarise(b01 = quantile(estimate, probs = 0.1),
            b09 = quantile(estimate, probs = 0.9)) %>% left_join(
              Coef_pool %>% filter(exp %in% exp0, variable %in% c("area","prod", "export" ,"price", "import", "consume")) %>% 
                group_by(variable, exp, ccs) %>% summarise(b05 = mean(estimate))
            )

Fig2ED$variable <-  factor(Fig2ED$variable, levels =  c("area","prod", "export", "price", "import","consume"), 
                           labels = c("Harvested area","Production", "Export","Price", "Import","Consumption"))

Fig2ED$exp <-  factor(Fig2ED$exp, levels =  exp0, 
                      labels = c("Adaptive expectation","Perfect foresight"))

Fig2ED$ccs <-  factor(Fig2ED$ccs, levels =  scenlabel)

Fig2ED1 <- Fig2ED %>% mutate(Byield = 1) %>% 
  bind_rows(Fig2ED %>% mutate(Byield = -1, b01 = -b01, b05 = -b05, b09 = -b09)) %>%
  bind_rows(Fig2ED %>% mutate(Byield = -1/b05, b01 = -b01/b05, b09 = -b09/b05, b05 = -1)) %>%
  bind_rows(Fig2ED %>% mutate(Byield = 1/b05, b01 = b01/b05, b09 = b09/b05, b05 = 1)) %>%
  bind_rows(Fig2ED %>% mutate(Byield = -1/b01, b05 = -b05/b01, b09 = -b09/b01, b01 = -1)) %>%
  bind_rows(Fig2ED %>% mutate(Byield = 1/b01, b05 = b05/b01, b09 = b09/b01, b01 = 1)) %>%
  bind_rows(Fig2ED %>% mutate(Byield = -1/b09, b01 = -b01/b09, b05 = -b05/b09, b09 = -1)) %>%
  bind_rows(Fig2ED %>% mutate(Byield = 1/b09, b01 = b01/b09, b05 = b05/b09, b09 = 1))

xscale = scale_x_continuous(expand = c(0, 0), breaks = c(-0.5,0,0.5)) #
yscale = scale_y_continuous(expand = c(0, 0), breaks = c(-0.5,0,0.5))

A1 <- ggplot(Fig2ED1) + facet_grid(cols = vars(ccs), rows = vars(exp)) +
  #facet_grid(cols = vars(exp)) +
  geom_ribbon(aes(x = Byield, ymin = b01, ymax = b09, fill = variable),alpha = 0.2) +
  scale_fill_manual(values=c(col5))  +
  geom_line(aes(x= Byield, y =b05, group = variable, color =  variable), alpha = 1,  size = 1) + 
  scale_color_manual(values=c(col5))+
  geom_hline(yintercept=0, linetype=1, color = "gray", size = 0.5) + 
  geom_vline(xintercept=0, linetype=1, color = "gray", size =0.5) +
  coord_cartesian(ylim=c(-1, 1), xlim = c(-1,1)) +
  xscale+ yscale + guides(fill = guide_legend(nrow = 1)) +
  theme_bw() + theme0 + theme_leg +   theme(axis.ticks = element_blank(), legend.position ="top", #  c(.25,0.82), # #
                                            legend.key.size = unit(1, "cm"),
                                            legend.key.height=unit(1,"line"),
                                            legend.text = element_text(margin = margin(l = -25,t=5, b=0), size = 12),
                                            axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 5)),
                                            axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 5), vjust= 0.5),
                                            plot.margin = margin(t = 10, r = 10, b = 10, l = 10)) +
  labs(y ="Interannual economic responses (%)" , x = "Biophysical yield shocks (%)") ;A1

png(paste0("output/plot/Fig3/","FigS10_response",".png"), width = 7000, height = 4000, res = 600)
print(A1)
dev.off() 
######



Fig2_0 <- Coef_pool %>% filter(variable %in% c("area","prod", "export" ,"price", "import", "consume")) %>% 
  filter( interaction(region,crop) !="EU-15.palmfruit")
Fig2_0$variable <-  factor(Fig2_0$variable, levels =  c("area","prod", "export", "price", "import","consume"), 
                           labels = c("Harvested area","Production", "Export","Price", "Import","Consumption"))

Fig2_0$exp <-  factor(Fig2_0$exp, levels =  exp0, 
                      labels = c("Adaptive expectation","Perfect foresight"))
Fig2_0S$ccs <-  factor(Fig2_0S$ccs, levels = scenlabel)

A2 <- ggplot(Fig2_0 %>% group_by(exp, ccs, variable) %>% 
               summarise(r01 = quantile(estimate, probs = 0.1),
                         r25 = quantile(estimate, probs = 0.25),
                         r05 = quantile(estimate, probs = 0.5),
                         r75 = quantile(estimate, probs = 0.75),
                         r09 = quantile(estimate, probs = 0.9)) %>% ungroup(),
             aes(x =reorder(variable, desc(variable)))) + # facet_grid(cols = vars(ccs)) +
  facet_grid(cols = vars(ccs), rows = vars(exp)) +
  geom_boxplot(aes(ymin = r01, lower = r25, middle = r05, upper = r75, 
                   ymax = r09, fill =  variable), alpha=0.7, stat = "identity")+ 
  stat_summary(data = Fig2_0, fun.y=mean, aes(x =reorder(variable, desc(variable)), y =estimate), geom="point", shape=18, size=1) +
  theme_bw() + theme0 + theme_leg +   theme(axis.ticks = element_blank(), legend.position = "none") + 
  #scale_y_continuous(expand = c(0, 0),limits = c(-1,1)) + 
  scale_fill_manual(values=c(col5))  +   
  theme(axis.text.x = element_text(angle = 90, hjust= 1, size = 14),
        axis.title.x = element_blank(),
        panel.grid.minor.y = element_line(size = 0.1, linetype = 1,colour = "grey75"),panel.grid.major = element_line(size = 0.1, linetype = 1,colour = "grey75"),
        plot.margin = margin(t = 0, r = 10, b = 0, l = 0))+
  labs(y ="Interannual economic responses \nto biophysical shock" );A2

png(paste0("output/plot/Fig3/","FigS10_beta",".png"), width = 7000, height = 4000, res = 600)
print(A2)
dev.off() 

