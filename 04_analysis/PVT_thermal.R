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


PVTNBACKNASATCExp2 = fread("data_thermal.csv")
PVTNBACKNASATCExp2 = PVTNBACKNASATCExp2[ID != 103]
PVTNBACKNASATCExp2 = PVTNBACKNASATCExp2[!is_outlier(PVTNBACKNASATCExp2$Mean.RT, coef = 1.5) & !is.na(PVTNBACKNASATCExp2$Mean.RT), ]
PVTNBACKNASATCExp2 = PVTNBACKNASATCExp2[!is_outlier(PVTNBACKNASATCExp2$Median.RT, coef = 1.5) & !is.na(PVTNBACKNASATCExp2$Median.RT), ]

Temp = PVTNBACKNASATCExp2[,.(mean_top_mean = mean(top_mean)), by =.(Scenario)]

summary_dtPVT = PVTNBACKNASATCExp2[, .(mean_RT = mean(Mean.RT, na.rm = TRUE),
                          sd_RT = sd(Mean.RT, na.rm = TRUE),
                          median_RT = median(Median.RT, na.rm = TRUE),
                          mean_Error = mean(Total.Errors, na.rm = TRUE),
                          sd_Error = sd(Total.Errors, na.rm = TRUE),
                          median_Error = median(Total.Errors, na.rm = TRUE)),by = .(top_mean,rh_mean,Scenario)]



summary_dtPVT[, `:=` (
  Scenario = as.factor(Scenario)
)]



by(summary_dtPVT$median_RT,summary_dtPVT$top_mean, shapiro.test)
by(summary_dtPVT$median_RT,summary_dtPVT$top_mean, shapiro.test)

model = aov(median_Error ~ top_mean, data = summary_dtPVT)
summary(model)




#without scenario

(PVTmeansd = ggplot(summary_dtPVT, aes(x = top_mean, y = mean_RT))+
    geom_point(summary_dtPVT, mapping=aes(x=top_mean,y=mean_RT, colour = Scenario))+
    geom_smooth(summary_dtPVT, mapping=aes(x=top_mean,y=mean_RT), color="#999999", span=2)+
    stat_regline_equation(label.y = 100,size =3.5) +
    stat_cor(label.y = 55,size =3.5) +
    geom_vline(data=Temp, aes(xintercept = mean_top_mean, colour = as.factor(Scenario)),
               linetype="dashed")+
    scale_y_continuous(breaks = seq(100,750,250), limits = c(0,850), oob = scales::squish)+
    labs(#title = "PVT Test",
       x = "Operative Temperature (°C)",
       y = "PVT - Reaction time [ms]")+
    scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    theme_minimal()+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),  
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          strip.text = element_blank(),
          legend.text=element_text(size=10),
          legend.title=element_text(size=10))+ 
    theme(legend.position = "none"))
print(PVTmeansd)


# with scenario


by(summary_dtPVT$PVTmeansd,summary_dtPVT$top_mean, shapiro.test)

model = aov(PVTmeansd ~ top_mean, data = summary_dtPVT)
summary(model)



(PVTmeansdScen=ggplot(summary_dtPVT, aes(x = top_mean, y = mean_RT)) + 
    geom_point(summary_dtPVT, mapping=aes(x = top_mean, y = mean_RT, group = Scenario, colour = Scenario))+ 
    geom_smooth(summary_dtPVT, mapping=aes(ymin = mean_RT - sd_RT, ymax = mean_RT + sd_RT), color="#999999", span=2)+ 
    stat_regline_equation(label.y = 170,size =3.5) +
    stat_cor(label.y = 150,size =3.5) +
    geom_vline(data=Temp, aes(xintercept = mean_top_mean, colour = as.factor(Scenario)),linetype="dashed")+
    facet_wrap(~Scenario, nrow = 1)+
    labs(#title = "PVT Test",
      x = "Operative Temperature (°C)",
      y = "PVT - Reaction time [ms]")+
    scale_y_continuous(limits = c(150,450), breaks = seq(150,450,50), labels = seq(150,450,50))+ 
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
print(PVTmeansdScen)


ggarrange(PVTmeansdScen, NULL, PVTmeansd, nrow =3, heights = c(2.1,0.05, 2.0), common.legend = TRUE, legend = "bottom",labels = c("A", "B"), font.label = list(size=10)) #hjust = -0.1, vjust = 1.2, 
ggsave(filename = "PVT.jpg",width = 20.5, height = 16.5, unit = "cm", dpi =350)
