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
       y = "KSS - Sleepiness")+
  scale_y_continuous(breaks = c(1, 3, 5, 7, 9), limits = c(1,9), labels = c("1\nExtremely\nalert","3","5","7","9\nExtremely\nsleepy"))+
  theme_minimal()+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none"))
print(KSSMedianKSSAll)




# with scenario


by(summary_dtKSS$median_KSS,summary_dtKSS$top_mean, shapiro.test)

model = aov(median_KSS ~ top_mean, data = summary_dtKSS)
summary(model)



(KSSMeanKSSAll=ggplot(summary_dtKSS, aes(x = top_mean, y = mean_KSS)) + 
     geom_point(summary_dtKSS, mapping=aes(x = top_mean, y = mean_KSS, group = Scenario, colour = Scenario))+ 
    geom_smooth(summary_dtKSS, mapping=aes(ymin = mean_KSS - sd_KSS, ymax = mean_KSS + sd_KSS), color="#999999", span=2)+ 
    stat_regline_equation(label.y = 1.8,size =3.5) +
    stat_cor(label.y = 1,size =3.5) +    
    geom_vline(data=Temp, aes(xintercept = mean_top_mean, colour = as.factor(Scenario)),linetype="dashed")+
    facet_wrap(~Scenario, nrow = 1)+
    labs(#title = "Karolinska Sleepiness Scale",
      x = "Operative Temperature (°C)",
      y = "KSS - Sleepiness")+
    scale_y_continuous(breaks = c(1, 3, 5, 7, 9), limits = c(1,9), labels = c("1\nExtremely\nalert","3","5","7","9\nExtremely\nsleepy"))+
    scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
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
ggsave(filename = "KSS.jpg",width = 10.5, height = 16.5, unit = "cm", dpi =350)
