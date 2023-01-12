
col5 <- c("#F8766D","#00BF7D","#B79F00", "#00B0F6", "gray", "#E76BF3" ) 
exp0<- c("AEp1", "PF"); explab = c("Adaptive expectation","Perfect foresight")
VariableLevel <- c("area","prod","consume","price","export","import","yield")
Variablelable <- c("Harvested area", "Production", "Consumption","Price","Export","Import","Realized yield")
scenlabel <- c("HE","GE","HL","GL")


FigS1_2A <- Fig1_2 %>% separate(scen0, c("exp","ccs"), remove = T) %>% filter(ccs != "REF") %>% left_join(
  Fig1_2 %>% separate(scen0, c("exp","ccs")) %>% filter(ccs == "REF") %>% within(rm(ccs)) %>% rename(REF = value)) %>%
  filter(year == 2050) %>% mutate(value = (value/REF)) %>% within(rm(REF)) %>% 
  filter(crop != "Aggregated", variable %in% VariableLevel) 
FigS1_2A$variable <-  factor(FigS1_2A$variable, levels = VariableLevel, labels = Variablelable)
FigS1_2A$exp <-  factor(FigS1_2A$exp, levels = exp0, labels = explab)
FigS1_2A$ccs <-  factor(FigS1_2A$ccs, levels = scenlabel)

FigS1_2B <- FigS1_2A %>% 
  group_by(exp, ccs, variable) %>%
  summarise(mean = mean(value),
            value01 = quantile(value, probs = 0.1),
            value25 = quantile(value, probs = 0.25),
            value05 = quantile(value, probs = 0.5),
            value75 = quantile(value, probs = 0.75),
            value09 = quantile(value, probs = 0.9)) 

A1 <- ggplot(FigS1_2B,  aes(x =ccs)) + facet_wrap(vars(variable), nrow = 2)+
  geom_boxplot(aes(ymin = value01, lower = value25, 
                   middle = value05, upper = value75, 
                   ymax = value09, fill = exp), stat = "identity") + 
  stat_summary(data = FigS1_2A, fun.y=mean, aes(x =ccs, y =value, group = exp, color = exp), geom="point", shape = 18, size = 2,
               color = "black", position = position_dodge(width = .9)) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") + 
  guides(fill = guide_legend(title="Expectation scheme:")) +
  theme_bw() + theme0 + theme_leg + theme(axis.ticks = element_blank(), legend.position = c(0.88,0.2),
                                          strip.text = element_text(size = 14),
                                          legend.title = element_text(), legend.title.align=0,
                                          panel.spacing = unit(0.5, "lines"),
                                          legend.key.size = unit(1.1, "cm"),
                                          legend.key.height=unit(1,"line")) + 
  scale_y_continuous(expand = c(0, 0),limits = c(0,3), breaks = seq(0.2, 2.8, 0.4)) +
  labs(y ="Cumulative change (2010 = 1)" , x = "Scenario") + 
  theme(axis.ticks.y = element_line(linetype = 1,size = 1)); A1

png(paste0("output/plot/OtherSIFigs/","FigS17.png"), width = 7000, height = 4300, res = 600)
print(A1)
dev.off() 

######################

VariableLevel <- c("area","prod","consume","price","export","import","yield", "Byield")
Variablelable <- c("Harvested area", "Production", "Consumption","Price","Export","Import","Realized yield", "Biophysical yield")

FigS1_2A <- Fig1_2 %>% separate(scen0, c("exp","ccs"), remove = T) %>% filter(ccs != "REF") %>% left_join(
  Fig1_2 %>% separate(scen0, c("exp","ccs")) %>% filter(ccs == "REF") %>% within(rm(ccs)) %>% rename(REF = value)) %>%
  filter(year == 2050) %>% mutate(value = (value/REF-1)*100) %>% within(rm(REF)) %>% 
  filter(crop != "Aggregated", variable %in% VariableLevel) 

FigS1_2A$variable <-  factor(FigS1_2A$variable, levels = VariableLevel, labels = Variablelable)
FigS1_2A$exp <-  factor(FigS1_2A$exp, levels = exp0, labels = explab)
FigS1_3A <- FigS1_2A %>% left_join(
  FigS1_2A %>% filter(variable == "Biophysical yield") %>% 
    within(rm(variable)) %>% rename(YEXO = value)) %>%
  filter(is.na(YEXO) == F)

beta_UPDATE = FigS1_3A %>% nest_by(exp, variable) %>%
  mutate(fit = list(lm(value ~ YEXO , data = data))) 

Coef_UPDATE <- beta_UPDATE %>% summarise(tidy(fit)) %>% 
  dplyr::select(exp, variable, term, estimate) %>% 
  spread(term, estimate) %>% rename(a = `(Intercept)`, b = YEXO) 

FigS1_4A <- Coef_UPDATE %>% mutate(x = 75, y = a + b * 75) %>% bind_rows(
  Coef_UPDATE %>% mutate(x= -75, y = a - b * 75) ) %>% filter(variable != Variablelable[8])

FigS1_4B <- FigS1_3A %>% 
  dplyr::select(exp, variable,value, YEXO) %>% filter(variable == "Harvested area")


col8 <- c("#F8766D", "#00BF7D", "#E76BF3", "#00B0F6", "#B79F00",  "gray", "black", "black") 
sty_reg <- c( scale_color_manual(values=c(col8)),
              scale_alpha_manual(values = c(rep(1,8))),
              scale_size_manual(values = c(rep(1,8))) ,
              scale_shape_manual(values=c(rep(1,8))),
              scale_linetype_manual(values = c(rep(1,7), 2)))
xscale = scale_x_continuous(expand = c(0, 0), breaks = c(-50,0,50)) #
yscale = scale_y_continuous(expand = c(0, 0), breaks = c(-50,0,50))
A1 <- ggplot(FigS1_4A) + facet_wrap(~exp, nrow = 1) +   #facet_grid(rows = vars(Study), cols = vars(Model)) +
  geom_point(data = FigS1_4B , 
             aes(x = YEXO, y = value, group = variable, color =  variable, shape = variable), color = col8[1], alpha = 0.5) +
  geom_line(aes(x= x, y =y, group = variable, color =  variable, linetype = variable), alpha = 1,  size = 1) + 
  sty_reg +
  geom_hline(yintercept=0, linetype=5, color = "gray", size = 0.5) + 
  geom_vline(xintercept=0, linetype=5, color = "gray", size =0.5) +
  coord_cartesian(ylim=c(-75, 75), xlim = c(-75,75)) +
  xscale+ yscale + guides(col = guide_legend(title="Regression lines"),
                          linetype = guide_legend(title="Regression lines"),
                          shape = guide_legend(title="Points")) +
  theme_bw() + theme0 + theme_leg +   theme(axis.ticks = element_blank(), legend.position ="right",  
                                            legend.title.align=0,
                                            strip.text = element_text(size = 14),
                                            legend.key.size = unit(1, "cm"),
                                            legend.key.height=unit(1,"line"),
                                            panel.spacing = unit(1, "lines"),
                                            legend.title = element_text(size = 12),
                                            legend.text = element_text(margin = margin(l = -25,t=5, b=0), size = 12),
                                            axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 5)),
                                            axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 5), vjust= 0.5),
                                            plot.margin = margin(t = 10, r = 5, b = 10, l = 10) ) +
  labs(y ="Cross-sectional economic responses \n(percent change)" , x = "Biophysical yield shocks (percent change)") ;A1 

png(paste0("output/plot/OtherSIFigs/","FigS18.png"), width = 6000, height = 2500, res = 600)
print(A1)
dev.off() 
###################################################################
