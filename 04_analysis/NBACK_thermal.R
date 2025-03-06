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
PVTNBACKNASATCExp2 = PVTNBACKNASATCExp2[!is_outlier(PVTNBACKNASATCExp2$Accuracy, coef = 1.5) & !is.na(PVTNBACKNASATCExp2$Accuracy), ]

PVTNBACKNASATCExp2 = PVTNBACKNASATCExp2[, Scenario := fcase(Scenario == 1, 1.10,
                                                            Scenario == 10, 10,
                                                            Scenario == 100, 69.1,
                                                            Scenario == 1000, 593)]

summary_dtNASAMASExp2 = PVTNBACKNASATCExp2[, .(mean_Accuracy = mean(Accuracy, na.rm = TRUE),
                                          sd_Accuracy = sd(Accuracy, na.rm = TRUE),
                                          median_Accuracy = median(Accuracy, na.rm = TRUE),
                                          mean_Correct.responses = mean(Means.correct.responses, na.rm = TRUE),
                                          sd_Correct.responses = sd(Means.correct.responses, na.rm = TRUE),
                                          median_Correct.responses = median(Medians.correct.responses, na.rm = TRUE)),
                                      by = .(top_mean,rh_mean,Scenario)]



summary_dtNASAMASExp2[, `:=` (
  Scenario = as.factor(Scenario)
)]

Temp = PVTNBACKNASATCExp2[,.(mean_top_mean = mean(top_mean)), by =.(Scenario)]


#without scenario

(Nbackmeansd = ggplot(summary_dtNASAMASExp2, aes(x = top_mean, y = mean_Accuracy))+
    geom_point(summary_dtNASAMASExp2, mapping=aes(x=top_mean,y=mean_Accuracy, colour = Scenario))+
    geom_smooth(summary_dtNASAMASExp2, mapping=aes(x=top_mean,y=mean_Accuracy), color="#999999", span=2)+
    stat_regline_equation(label.y = 100,size =3.5) +
    stat_cor(label.y = 55,size =3.5) +
    geom_vline(data=Temp, aes(xintercept = mean_top_mean, colour = as.factor(Scenario)),
               linetype="dashed")+
    scale_y_continuous(breaks = seq(0.7,1,0.1), limits = c(0.7,1.02),label = scales::percent)+
    labs(#title = "N-back Test",
      x = "Operative Temperature (°C)",
      y = "N-back - Accuracy",
      colour = "mEDI [lx]",
      fill = "mEDI [lx]")+
    scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                       labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
    scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                      labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
    theme_minimal()+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),  
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          strip.text = element_blank(),
          legend.text=element_text(size=10),
          legend.title=element_text(size=10))+ 
    theme(legend.position = "none"))
print(Nbackmeansd)




# with scenario



(NbackmeansdScen=ggplot(summary_dtNASAMASExp2, aes(x = top_mean, y = mean_Accuracy)) + 
    geom_point(summary_dtNASAMASExp2, mapping=aes(x = top_mean, y = mean_Accuracy, group = Scenario, colour = Scenario))+ 
    geom_smooth(summary_dtNASAMASExp2, mapping=aes(ymin = mean_Accuracy - sd_Accuracy, ymax = mean_Accuracy + sd_Accuracy), color="#999999", span=2)+ 
    stat_regline_equation(label.y = 0.72,size =3.5) +
    stat_cor(label.y = 0.7,size =3.5) +
    geom_vline(data=Temp, aes(xintercept = mean_top_mean, colour = as.factor(Scenario)),linetype="dashed")+
    facet_wrap(~Scenario, nrow = 1, labeller = as_labeller(c("1.1" = "1 lx", "10" = "10 lx", "69.1" = "70 lx", "593" = "595 lx")))+
    labs(#title = ""N-back Test",
      x = "Operative Temperature (°C)",
      y = "N-back - Accuracy",
      colour = "mEDI [lx]",
      fill = "mEDI [lx]")+
    scale_y_continuous(breaks = seq(0.7,1,0.1), limits = c(0.7,1.02),label = scales::percent)+
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
print(NbackmeansdScen)


ggarrange(NbackmeansdScen, NULL, Nbackmeansd, nrow =3, heights = c(2.1,0.05, 2.0), common.legend = TRUE, legend = "bottom",labels = c("A", "B"), font.label = list(size=10)) #hjust = -0.1, vjust = 1.2, 
ggsave(filename = "Supplementary_Nback_thermal.jpg",width = 20.5, height = 16.5, unit = "cm", dpi =350)



