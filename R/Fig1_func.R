#' Fig1 line plot with shaded areas
#'
#' @param data Main data processed from Fig1_2
#' @param x 
#' @param y 
#' @param group 
#' @param arearange area percentile range
#'
#' @return
#' @export
#'
#' @examples
line <- function(data, x, y = "value", group, arearange = c(0.1,0.9), areaalpha = 0.1){
  ggplot(data  %>%  filter(is.na(get(y)) == F) %>%
           group_by_at(vars(group, x)) %>% 
           summarise(mean = mean(get(y))) %>%  # using unweighted average here
           left_join(
             data%>% filter(is.na(get(y)) == F) %>%
               group_by_at(vars(group,x))  %>% 
               mutate(value01 = quantile(get(y), probs = arearange[1]),
                      median = median(get(y)),
                      value09 = quantile(get(y), probs = arearange[2])) %>%
               filter(get(y) >= value01, get(y) <= value09) %>%
               summarise(max = max(get(y)), min = min(get(y)), median = min(median)) %>% ungroup() )
  ) +   
    geom_ribbon(aes(x = get(x), ymin = min, ymax = max,group = get(group), fill = get(group),alpha = get(group)),alpha = areaalpha) +
    scale_fill_manual(values=c(color4))  +
    geom_line(aes(x = get(x), y = mean, group = get(group), color = get(group), linetype = get(group),alpha = get(group), size = get(group))) +
    sty_reg + theme_bw() + theme0 + theme_leg  + 
    theme(plot.title = element_text(size=16, face="bold")) 
}


#' Marginal plots of Fig1 bottol panel
#'
#' @param data 
#' @param x 
#' @param y 
#' @param boxrange 
#' @param col 
#' @param yscale 
#'
#' @return
#' @export
#'
#' @examples
mar.boxplot <- function(data, x, y, boxrange = c(0.1,0.25,0.5,0.75,0.9),
                        col = hue_pal()(10)){
  ggplot(data %>% group_by_at(vars(x)) %>% 
           summarise(ymin = quantile(get(y), probs = boxrange[1]),
                     lower = quantile(get(y), probs = boxrange[2]),
                     middle = quantile(get(y), probs = boxrange[3]),
                     upper = quantile(get(y), probs = boxrange[4]),
                     ymax = quantile(get(y), probs = boxrange[5])) %>% ungroup() 
         , aes(x = get(x))) + 
    geom_boxplot(aes(ymin = ymin, lower = lower, middle = middle, 
                     upper = upper, max = ymax, fill =  get(x)), 
                 alpha=0.5, lwd=0.5, fatten = 1, stat = "identity") + 
    stat_summary(data = data, 
                 fun.y=mean, aes(x = get(x), y = get(y)), 
                 geom="point", shape=18, size= 1) +
    scale_fill_manual(values=c(col))  +
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(), 
      axis.title = element_blank(), 
      axis.ticks = element_blank(), 
      legend.position="none",
      plot.margin = margin(t = 2, r = 2, b = 5, l =0)) 
}




#' Generating Fig1
#'
#' @return output Fig1
#' @export
#'
#' @examples
#' 
#' 
Fig1 <- function(data = Fig1_2A %>% filter(variable == vv), 
                 leftmargin = 6, LP = "none", vv = "Price", group = "ccs"){
  
  plt1_1 <- line(data = data %>% filter(crop != "Aggregated"), 
                 x = "year", y ="value", group = group, arearange = c(0.1,0.9), areaalpha = 0.1) +
    scale_x_continuous(expand = c(0, 0),limits = c(2010, 2050),breaks = seq(2015, 2045, 10)) + 
    scale_y_continuous(expand = c(0, 0),limits = limit0,breaks =BK)  +    
    geom_hline(yintercept=1, linetype="dashed", color = "black") + labs(title = paste(AA,vv)) +
    theme(plot.margin = margin(t = 10, r = 0, b = 0, l = leftmargin), legend.position = LP) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(),axis.ticks.x = element_blank())

    
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
    theme(plot.margin = margin(t = 2, r = 0, b = 5, l = leftmargin), legend.position = "none") +
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















