### SCRIPT TO GRAPH PAPER RESULTS OF ABUAN STREAMFLOW STUDY

## Prelims

data_dir <- 'D:/AbuanSWAT'
results_dir <- 'D:/AbuanSWAT/figures'
require(pacman)
pacman::p_load(plyr,dplyr,ggplot2, ggpubr,ggtern)

## preprocessing

baseline <- read.csv(paste0(data_dir, '/baseline.csv'))
raw_cc <- read.csv(paste0(data_dir, '/results_cc.csv'))
raw_cc_lulc <- read.csv(paste0(data_dir, '/results_cc_lulc.csv'))

cc_2050_45 <- raw_cc[1:4]
cc_2050_85 <- raw_cc[5:8]
cc_2070_45 <- raw_cc[9:12]
cc_2070_85 <- raw_cc[13:16]
cc_lulc_2050_45 <- raw_cc_lulc[1:4]
cc_lulc_2050_85 <- raw_cc_lulc[5:8]
cc_lulc_2070_45 <- raw_cc_lulc[9:12]
cc_lulc_2070_85 <- raw_cc_lulc[13:16]

cc_2050_45$impact <- 'cc'
cc_2050_85$impact <- 'cc'
cc_2070_45$impact <- 'cc'
cc_2070_85$impact <- 'cc'
cc_lulc_2050_45$impact <- 'cc_lulc'
cc_lulc_2050_85$impact <-'cc_lulc'
cc_lulc_2070_45$impact <- 'cc_lulc'
cc_lulc_2070_85$impact <- 'cc_lulc'

cc_2050_45$scenario <- 'c50a'
cc_2050_85$scenario <- 'c50b'
cc_2070_45$scenario <- 'c70a'
cc_2070_85$scenario <- 'c70b'
cc_lulc_2050_45$scenario <- 'l50a'
cc_lulc_2050_85$scenario <- 'l50b'
cc_lulc_2070_45$scenario <- 'l70a'
cc_lulc_2070_85$scenario <- 'l70b'

scenarios <- list(cc_2050_45,cc_2050_85,cc_2070_45,cc_2070_85,cc_lulc_2050_45,cc_lulc_2050_85,
                  cc_lulc_2070_45,cc_lulc_2070_85)
new_name <-  c('lwr', 'upr', 'flow', 'pfr', 'impact', 'scenario')
scenarios <- lapply(scenarios, setNames, nm = new_name)
scenarios <- ldply(scenarios, data.frame)
scenarios$month <- rep(1:12, 8)
scenarios$impact <- as.factor(scenarios$impact)
head(scenarios)

## Baseline and scenario bar graphs per season and with error range
dry <- scenarios[scenarios$month %in% c(2:5), ]
wet <- scenarios[scenarios$month %in% c(6:8), ]

dry_avg = dry %>% group_by(scenario) %>% summarise (lwr=mean(lwr), upr=mean(upr), flow=mean(flow), impact=mean(impact))
wet_avg = wet %>% group_by(scenario) %>% summarise (lwr=mean(lwr), upr=mean(upr), flow=mean(flow))


dry_plot <- ggplot(dry_avg, aes(x=scenario, y=flow, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width=0.2)
dry_plot + labs(y="Simulated flows and 95% CI", x = "Scenario") + theme_classic()

wet_plot <- ggplot(wet_avg, aes(scenario, flow)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = lwr, ymax = upr), width=0.2)
wet_plot + labs(y="Simulated wet flows and 95% CI", x = "Scenario") + theme_classic()


mo_obs = baseline %>% group_by(Month) %>% summarise (observed=mean(observed), uncalibrated=mean(uncalibrated.simulated), 
                                                     calibrated=mean(calibrated.simulated))


plot_func <- function(df,mo, mo1){
  plots <- ggplot(df, aes(x=scenario, y=flow, fill=impact)) + 
    geom_bar(stat="identity") + scale_fill_manual("impact", values = c("cc" = rgb2hex(253,184,39), "cc_lulc" =  rgb2hex(84,37,131))) +
    theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1), 
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         panel.border = element_blank(),  
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(colour = "grey")) + 
    geom_hline(yintercept=mo1, linetype="dashed", color = "red") +
    coord_cartesian(ylim = c(0, 200)) + #geom_hline(yintercept=obs, linetype="dashed", color = "red") +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width=0.2)+ ggtitle(mo)
  return (plots) 
}
mo <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 
         'September', 'October', 'November', 'December')
mo_base <- baseline %>% group_by(Month) %>% summarise (abuan=mean(abuan_baseline))
mo_base <- mo_base[[2]]

## monthly plots
mo_plot <- function(df){
  mos <- lapply(1:12, function(x) df[df$month %in% x, ]) #monthly df
  mo_avg <- lapply(1:12, function(x)
    mos[[x]] %>% group_by(scenario) %>% 
      summarise (lwr=mean(lwr), upr=mean(upr), flow=mean(flow)))
  mo_avg <- lapply(mo_avg, function(x) {x$impact <- c(rep('cc',4), rep('cc_lulc', 4));return(x)})
  
  plot4 <- ggplot(mo_avg[[4]], aes(x=scenario, y=flow, fill=impact)) +     
    geom_bar(stat="identity") + scale_fill_manual("impact", values = c("cc" = rgb2hex(253,184,39), "cc_lulc" =  rgb2hex(84,37,131))) +
    theme(legend.position = c(0.8, 0.8), legend.text = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 90, hjust = 1), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          legend.background = element_rect(size=0.5, linetype="solid",  colour ="black"),
          panel.border = element_blank(),  
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "grey")) + 
    
    coord_cartesian(ylim = c(0, 200)) + #geom_hline(yintercept=obs, linetype="dashed", color = "red") +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width=0.2)+ ggtitle('April') 
  plot4 <- plot4 + geom_hline(yintercept=mo_base[[4]], linetype="dashed", color = "red")
   
  
  plots <- lapply(1:12, function(x) plot_func(mo_avg[[x]], mo[[x]], mo_base[[x]]))
  plots_arr <- ggarrange (plots[[1]],plots[[2]],plots[[3]],plot4, plots[[5]],plots[[6]],
                          plots[[7]],plots[[8]],plots[[9]],plots[[10]],plots[[11]],plots[[12]],ncol = 4, nrow = 3)
#  plots_arr <- annotate_figure(plots_arr,
   #                            left = text_grob("Mean monthly flows and 95% CI (m3/sec)",rot = 90,size = 24),
     #                          bottom = text_grob("Scenarios per month",size = 24))
  setwd(results_dir)
  ggsave(plot=plots_arr, filename=paste0('Figure_Scens.png'),
         device='png', dpi=300, width = 14, height = 10, units='in')
  return(mo_avg)
}


mo_vag <- mo_plot(scenarios)
mo_avg <- ldply(mo_vag, data.frame)
mo_avg$month <- c(rep(1, 8),rep(2, 8),rep(3, 8),rep(4, 8),rep(5, 8),rep(6, 8),rep(7, 8),rep(8, 8),
                  rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8))
write.csv(mo_avg, 'mo_avg_new.csv', row.names=F)
