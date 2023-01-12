library(sf) #sf_0.9-6
dir.create("output/plot/Fig4", showWarnings = FALSE)

`%notin%` = Negate(`%in%`)
scenLevel <- c("AEp1_HE", "AEp1_GE","AEp1_HL","AEp1_GL")
scenlabel <- c("HE","GE","HL","GL")
VariableLevel <- c("Byield","area","prod","consume","price","export","import","yield")
Variablelable <- c("Biophysical yield","Harvested area", "Production", "Consumption","Price","Export","Import","Realized yield")
CropLevel <- c("corn", "wheat", "rice", "oilcrop", "othergrain", "fibercrop", "palmfruit",  "sugarcrop", "root_tuber", "misccrop")
Croplabel <- c("Corn", "Wheat", "Rice", "Oil crops", "Other grains", "Fiber crops", "Palm fruits", "Sugar crops", "Root & tuber", "Misc. crops")


Fig1_2A <- Fig1_2 %>% separate(scen0, c("exp","ccs"), remove = F) %>% filter(ccs != "REF") %>% left_join(
  Fig1_2 %>% separate(scen0, c("exp","ccs")) %>% filter(ccs == "REF") %>% within(rm(ccs)) %>% rename(REF = value)) %>%
  mutate(value1 = log(value / REF)*100, value2 = (value/REF-1)*100, value = (value/REF)) %>% within(rm(REF)) %>% 
  group_by(scen0, region, crop, variable) %>% 
  mutate(valueR = value1 - lag(value1)) %>% 
  ungroup() #%>% filter(exp == "AEp1")

unique(Fig1_2A$crop)
Fig1_2A$variable <-  factor(Fig1_2A$variable, levels = VariableLevel, labels = Variablelable)
Fig1_2A$ccs <-  factor(Fig1_2A$ccs, levels = scenlabel)
Fig1_2A$crop <-  factor(Fig1_2A$crop, levels = CropLevel, labels = Croplabel)

########################################################################

basin <- read.csv(file = "data/basin_to_country_mapping.csv", header = T, comment.char = "#")
country <- read.csv(file ="data/GCAM_region_names.csv", header = T, comment.char = "#")
map_424_sf <- st_read("data/map_424.shp")
summary(map_424_sf)
class(map_424_sf)

map_31_sf <- map_424_sf %>% filter(basn_ID !=1) %>%  #remove Greenland 1
  group_by(cntr_ID) %>% summarise(cntry_n = first(cntry_n)) 
#plot(map_31_sf)

exp0<- c("AEp1", "PF")

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

Fig3_1 <- ANOVA1 %>% filter(exp %in% exp0) %>%
  group_by(exp,ccs, region, crop,variable) %>% summarise(IV = sd(value)) %>% 
  ungroup() %>% mutate(RC = interaction(region, crop))
Fig3_1$variable <-  factor(Fig3_1$variable, levels = VariableLevel, labels = Variablelable)
Fig3_1$ccs <-  factor(Fig3_1$ccs, levels = scenlabel)
Fig3_1$crop <-  factor(Fig3_1$crop, levels = CropLevel, labels = Croplabel)

#Fill in emplty value with average!!! Canada rice is an important one!!
Fig3_2 <-
  Fig3_1 %>% filter(variable == "Consumption") %>% 
  dplyr::select(exp,ccs,region, crop, RC) %>% #consumption has all RC 310...
  left_join(Fig3_1 %>% filter(variable == "Price") ) %>% 
  mutate(variable ="Price") %>%  group_by(exp, ccs, crop, variable) %>% 
  mutate(mIV = mean(IV,na.rm=TRUE), IV = if_else(is.na(IV), mIV, IV )) %>% 
  within(rm(mIV)) %>% ungroup() %>% 
  bind_rows(
    Fig3_1 %>% filter(variable == "Consumption") %>% 
      dplyr::select(exp,ccs,region, crop, RC) %>% #consumption has all RC 310...
      left_join(Fig3_1 %>% filter(variable == "Biophysical yield") ) %>% 
      mutate(variable ="Biophysical yield")
  ) %>%  bind_rows(Fig3_1 %>% filter(variable == "Consumption")) 

########################################################################
#library(viridis)
#library(scales)
ccs0 ="GE"
for (ccs0 in  scenlabel) {  tryCatch({
  df1 <- Fig3_2 %>% filter(exp == "AEp1", ccs ==ccs0, crop %in% Croplabel[1:4], variable %in% Variablelable[c(1,4,5)]) %>% 
    dplyr::select(cntry_n = region, value = IV, exp, ccs, crop, variable) 
  breaks = seq(0,22,2)
  df0<-left_join(map_31_sf, df1) %>% mutate(value = cut(value,unique(breaks)))
  lab <- c(paste0("(",sort(c(unique(df0$value)))*2-2,", ",sort(c(unique(df0$value)))*2,"]"), "No prod.")
  
  assign(paste0("Fig4_",ccs0),
         ggplot(data = df0) +
           geom_sf(aes(fill = value), color = "black", size = 0.5) + 
           facet_grid(rows = vars(crop),  cols= vars(variable)) +
           #scale_fill_gradient(low = "#00B0F6", high = "#E76BF3") + 
           #scale_fill_distiller(palette = c("BrBG"), direction = -1) +  #YlOrRd
           #scale_fill_viridis(option = "D", alpha = 0.5) + 
           #scale_fill_manual(palette = c("BrBG")) + 
           scale_fill_brewer(palette = "YlGnBu",direction = 1 ,na.value="grey80", labels = lab) +
           scale_y_continuous(expand = c(0.04, 0.04)) + scale_x_continuous(expand = c(0.02, 0.02)) +
           theme_bw() +  theme0 + theme_leg +
           theme(panel.grid.major = element_line(colour = 'transparent'), #panel.border =element_blank(),
                 panel.border = element_rect(color = "black", size =1),
                 axis.text = element_blank(), axis.ticks = element_blank(), 
                 axis.text.x = element_blank(), axis.title = element_blank(), 
                 legend.position = "right",
                 legend.key.size = unit(1, "cm"),
                 #legend.key.width = unit(1.5,"line"),
                 legend.key.height=unit(1.5,"line"),
                 #legend.box.margin=margin(-10, 10,15,10),
                 legend.text = element_text(margin = margin(l = -15, t=0, b=0, r=0), size = 16),
                 strip.background =element_rect(fill="grey99"),
                 legend.spacing.x = unit(0.8, 'cm'),
                 legend.spacing.y = unit(0.8, 'cm'),
                 legend.margin = margin(t=5),
                 legend.direction = "horizontal", 
                 legend.box = "horizontal",
                 legend.title = element_text(color = "black", size = 16 #,face = "bold"
                 )) + 
           guides(fill=guide_legend(
             title = "Interannual \nvariability (%)",
             title.position = "top",
             title.hjust = 0, keyheight=2.5, keywidth = 2.5,
             ncol = 1, label.hjust = 0, label.vjust = 0.5, label.position = "right"))
  ) 
  
  png(paste0("output/plot/Fig4/Fig4_",ccs0,"4crop",".png"), width = 9000, height = 4500, res = 600)
  print(get(paste0("Fig4_",ccs0)))
  dev.off()
}, error = function(e){}) }
#Fig4 exported!! 
########################################################################

col5 <- c("#F8766D","#00BF7D","#B79F00", "#00B0F6", "gray", "#E76BF3" ) 
col3 <-  c("gray", "#E76BF3", "#00B0F6" ) 
xscale = scale_x_continuous(expand = c(0, 0),limits = c(0, 20),breaks = seq(5, 15, 5)) #
yscale = scale_y_continuous(expand = c(0, 0),limits = c(0, 1.8),breaks = seq(0.5, 1.5, 0.5))

df <- Fig1_2A%>% filter(crop != "Aggregated", year!=2010) %>%
  group_by(exp, ccs, region, crop, variable) %>% 
  summarise(sd =sd(valueR)) %>% ungroup() %>%
  group_by(exp, ccs, variable) #%>% spread(variable, sd)

yscale = scale_y_continuous(expand = c(0, 0),limits = c(0, 1.8),breaks = seq(0.5, 1.5, 0.5))
EDFig15 <- ggplot(df %>% filter(crop %in% Croplabel[1:4], 
                                 variable %in% Variablelable[c(1,4,5)], 
                                 exp =="AEp1")) +  
  facet_grid(cols = vars(ccs), rows = vars(crop)) + 
  scale_fill_manual(values=c(col3)) +
  geom_density(aes(x= sd, fill = variable), alpha = 0.5) +
  theme_bw() +  theme0 + theme_leg + xscale + yscale +
  theme(legend.position= "top",
        legend.key.size = unit(1, "cm")) +
  labs(x ="Interannual variability (%)", y = "Density");EDFig15

png(paste0("output/plot/Fig4/","FigS15_4crops.png"), width = 6000, height = 4000, res = 600)
print(EDFig15)
dev.off() 


yscale = scale_y_continuous(expand = c(0, 0),limits = c(0, 3),breaks = seq(0.5, 1.5, 0.5))
FigS15 <- ggplot(df %>% filter(crop %notin% Croplabel[1:4], 
                                  variable %in% Variablelable[c(1,4,5)], 
                                  exp =="AEp1")) +  
  facet_grid(cols = vars(ccs), rows = vars(crop)) + 
  scale_fill_manual(values=c(col3)) +
  geom_density(aes(x= sd, fill = variable), alpha = 0.5) +
  theme_bw() +  theme0 + theme_leg + xscale + yscale +
  theme(legend.position= "top",
        legend.key.size = unit(1, "cm")) +
  labs(x ="Interannual variability (%)", y = "Density");FigS15

png(paste0("output/plot/Fig4/","FigS15_others.png"), width = 6000, height = 6000, res = 600)
print(FigS15)
dev.off()

