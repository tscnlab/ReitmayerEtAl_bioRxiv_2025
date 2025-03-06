library(data.table)
library(ggplot2)
library(patchwork)
library(plotly)
library(dplyr)
library(ggpubr) 
library(Hmisc)
library(lme4)
library(lmerTest)
library(rstatix)

KSSExp2 = fread("data_thermal.csv")
KSSExp2 = KSSExp2[ID != 103]
KSSExp2 = KSSExp2[!is_outlier(KSSExp2$KSS, coef = 1.5) & !is.na(KSSExp2$KSS), ]

KSSExp2 = KSSExp2[, Scenario := fcase(Scenario == 1, 1.10,
                                      Scenario == 10, 10,
                                      Scenario == 100, 69.1,
                                      Scenario == 1000, 593)]

summary_dtKSS = KSSExp2[, .(mean_KSS = mean(KSS, na.rm = TRUE),
                            sd_KSS = sd(KSS, na.rm = TRUE),
                            median_KSS = median(KSS, na.rm = TRUE)),by = .(top_mean,rh_mean,Scenario)]

Temp = KSSExp2[,.(mean_top_mean = mean(top_mean)), by =.(Scenario)]

summary_dtKSS[, `:=` (
  Scenario = as.factor(Scenario)
)]


#without scenario

(KSSMedianKSSAll=ggplot(summary_dtKSS, aes(x = top_mean, y = median_KSS)) +
  geom_point(summary_dtKSS, mapping=aes(x=top_mean,y=mean_KSS, colour = Scenario))+
  geom_smooth(summary_dtKSS, mapping=aes(x=top_mean,y=mean_KSS), color="#999999", span=2)+
  stat_regline_equation(label.y = 1.8,size =3.5) +
  stat_cor(label.y = 1,size =3.5) +
  geom_vline(data=Temp, aes(xintercept = mean_top_mean, colour = as.factor(Scenario)),
               linetype="dashed")+
  labs(#title = "Karolinska Sleepiness Scale",
       x = "Operative Temperature (°C)",
       y = "KSS - Sleepiness",
       colour = "mEDI [lx]",
       fill = "mEDI [lx]")+
  scale_y_continuous(breaks = c(1, 3, 5, 7, 9), limits = c(1,9), labels = c("1\nExtremely\nalert","3","5","7","9\nExtremely\nsleepy"))+
  theme_minimal()+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                       labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                      labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none"))
print(KSSMedianKSSAll)




# with scenario



(KSSMeanKSSAll=ggplot(summary_dtKSS, aes(x = top_mean, y = mean_KSS)) + 
     geom_point(summary_dtKSS, mapping=aes(x = top_mean, y = mean_KSS, group = Scenario, colour = Scenario))+ 
    geom_smooth(summary_dtKSS, mapping=aes(ymin = mean_KSS - sd_KSS, ymax = mean_KSS + sd_KSS), color="#999999", span=2)+ 
    stat_regline_equation(label.y = 1.8,size =3.5) +
    stat_cor(label.y = 1,size =3.5) +    
    geom_vline(data=Temp, aes(xintercept = mean_top_mean, colour = as.factor(Scenario)),linetype="dashed")+
    facet_wrap(~Scenario, nrow = 1, labeller = as_labeller(c("1.1" = "1 lx", "10" = "10 lx", "69.1" = "70 lx", "593" = "595 lx")))+
    labs(#title = "Karolinska Sleepiness Scale",
      x = "Operative Temperature (°C)",
      y = "KSS - Sleepiness",
      colour = "mEDI [lx]",
      fill = "mEDI [lx]")+
    scale_y_continuous(breaks = c(1, 3, 5, 7, 9), limits = c(1,9), labels = c("1\nExtremely\nalert","3","5","7","9\nExtremely\nsleepy"))+
    scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                       labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
    scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                      labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
    theme_minimal()+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),  
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          legend.text=element_text(size=10),
          legend.title=element_text(size=10))+ 
    theme(legend.position = "none"))
print(KSSMeanKSSAll)


ggarrange(KSSMeanKSSAll, NULL, KSSMedianKSSAll, nrow =3, heights = c(2.1,0.05, 2.0), common.legend = TRUE, legend = "bottom",labels = c("A", "B"), font.label = list(size=10)) #hjust = -0.1, vjust = 1.2, 
ggsave(filename = "Supplementary_KSS_thermal.jpg",width = 20.5, height = 16.5, unit = "cm", dpi =350)


