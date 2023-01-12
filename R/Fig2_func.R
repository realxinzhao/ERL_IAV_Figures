#' Generating Fig2
#'
#' @return output Fig2
#' @export
#'
#' @examples
#' 
#' 
Fig2 <- function(leftmargin = 6, LP = "none", vv = "Price" ){
  
  plt1_1 <- line(data = Fig2_1 %>% filter(variable == vv, scen0 %in% c(scenlabel[s*2-1], scenlabel[s*2]), crop != "Aggregated"), 
                 x = "year", y ="value", group = "scen0", arearange = c(0.1,0.9), areaalpha = 0.2) +
    scale_x_continuous(expand = c(0, 0),limits = c(2010, 2050),breaks = seq(2015, 2045, 10)) + 
    scale_y_continuous(expand = c(0, 0),limits = limit0,breaks =BK)  +    
    geom_hline(yintercept=1, linetype="dashed", color = "black") + labs(title = paste(AA,vv)) +
    theme(plot.margin = margin(t = 10, r = 0, b = 0, l = leftmargin), legend.position = LP) +
    scale_fill_manual(values=c(color4[s],color4[5]))  +
    scale_color_manual(values= c(color4[s],color4[5])) 
  
  
  
  plt1_2 <- ggplot(data = Fig1_5) + 
    geom_line(data = Fig1_5 %>% filter(variable == vv, crop !="Aggregated"), 
              aes(x= year, y =value, group = RC, color = get(group)), alpha = 0.1, size = 1) +
    scale_color_manual(values=c(color4)) + 
    geom_line(data = Fig1_5 %>% filter(variable == vv, crop =="Aggregated", region != "Waverage"),
              aes(x= year, y =value, group = get(group)), alpha = 1, color=1, size =0.5, linetype = 1) +
    geom_line(data = Fig1_5 %>% filter(variable == vv, crop =="Aggregated", region == "Waverage"), 
              aes(x= year, y =value), alpha = 1, color=1, size = 0.5, linetype = 2) + #scenario average!
    scale_x_continuous(expand = c(0, 0))+scale_y_continuous(expand = c(0, 0),limits = limit0)+
    theme_bw()+theme( legend.position="none",
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.border = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(), 
                      axis.title.y = element_blank(), axis.title.x = element_blank(), axis.ticks = element_blank(),
                      plot.margin = margin(t = 10, r = 2, b = 0, l =0))
  
  plt1_3 <- line(data = data, 
                 x = "year", y ="valueR", group = group, arearange = c(0.1,0.9), areaalpha = 0.1) +
    scale_x_continuous(expand = c(0, 0),limits = c(2010, 2050),breaks = seq(2015, 2045, 10)) + 
    scale_y_continuous(expand = c(0, 0),limits = limit1,breaks =BK1)  +    
    geom_hline(yintercept=0, linetype="dashed", color = "black") +  labs(y ="Annual (%)" , x = "Year")+ 
    theme(plot.margin = margin(t = 2, r = 0, b = 5, l = leftmargin), legend.position = LP) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
  
  Fig1_3SD <- data %>% filter(crop != "Aggregated", year!=2010) %>%
    group_by(exp, ccs, region, crop, variable) %>% 
    summarise(sd =sd(valueR)) %>% ungroup()
  
  plt1_4 <- mar.boxplot(data = Fig1_3SD, x = group, y = "sd",  col = color4) +
    scale_y_continuous(expand = c(0, 0),limits = limit1, breaks = BK1)
  
  ggarrange(plt1_1,plt1_2,plt1_3,plt1_4, 
            nrow=2, ncol = 2, 
            widths = c(0.83,0.17), 
            heights = c(0.8,0.2))
  
}
