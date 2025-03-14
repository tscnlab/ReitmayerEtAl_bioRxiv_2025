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


KSSExp2 = fread("data.csv")
KSSExp2 = KSSExp2[ID != 103]
KSSExp2 = KSSExp2[!is_outlier(KSSExp2$KSS, coef = 1.5) & !is.na(KSSExp2$KSS), ]

KSSExp2 = KSSExp2[, Scenario := fcase(Scenario == 1, 1.10,
                       Scenario == 10, 10,
                       Scenario == 100, 69.1,
                       Scenario == 1000, 593)]


summary_dtKSS = KSSExp2[, .(mean_KSS = mean(KSS, na.rm = TRUE),
                          sd_KSS = sd(KSS, na.rm = TRUE),
                          median_KSS = median(KSS, na.rm = TRUE)),by = .(Timepoint,Scenario)]

summary_dtKSS[, `:=` (
  Scenario = as.factor(Scenario),
  Timepoint = as.factor(Timepoint)
)]


by(summary_dtKSS$median_KSS,summary_dtKSS$Scenario, shapiro.test)

model = aov(median_KSS ~ Scenario, data = summary_dtKSS)
summary(model)


(KSSmeansd = ggplot(summary_dtKSS, aes(x = Timepoint, y = mean_KSS, group = Scenario, colour = Scenario))+
  geom_line()+
  geom_ribbon(aes(ymin = mean_KSS - sd_KSS, ymax = mean_KSS + sd_KSS, fill = Scenario),alpha = 0.6, linewidth = 0.25)+
  #scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,11), labels = c("1\nExtremely\nalert","5","9\nExtremely\nsleepy"))+
  scale_y_continuous(breaks = seq(1, 9, 1), limits = c(1,9.3), labels = c("1\nExtr.\nalert","","","","5","","","","9\nExtr.\nsleepy"))+
  scale_x_discrete(labels = c("1","","","", "5","","","","", "10"))+
  labs(#title = "Karolinska Sleepiness Scale",
       x = "Time point",
       y = "KSS Sleepiness",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+
  facet_wrap(~Scenario, nrow = 1, labeller = as_labeller(c("1.1" = "1 lx", "10" = "10 lx", "69.1" = "70 lx", "593" = "595 lx")))+
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
print(KSSmeansd)

my_comparisonsdtPVT = list(c("1 lx", "10 lx"),c("1 lx", "70 lx"),c("1 lx", "595 lx"),c("10 lx", "70 lx"),c("10 lx", "595 lx"), c("70 lx", "595 lx"))

set.seed(555)
(KSSMedianKSSAll=ggplot(summary_dtKSS, aes(x = Scenario, y = median_KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(#title = "Karolinska Sleepiness Scale",
       x = "Melanopic EDI [lx]",
       y = "KSS Sleepiness",
       color = "mEDI  [lx]",
       fill = "mEDI  [lx]")+
  stat_pwc(hide.ns = TRUE, label = "p.adj.signif", y.position = 9, group.by = "x.var", method = "tukey_hsd", step.increase = 0.15, p.adjust.method = "bonferroni", parse = FALSE)+
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(1, 9, 1), limits = c(1,9.3), labels = c("1\nExtr.\nalert","","","","5","","","","9\nExtr.\nsleepy"))+
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
print(KSSMedianKSSAll)


ggarrange(KSSMedianKSSAll, NULL, KSSmeansd, hjust = -0.1, vjust = 1.2, nrow =3, common.legend = TRUE, legend = "bottom",labels = c("A", "B"), heights = c(2.1,0.1, 1.0), font.label = list(size=10))
ggsave(filename = "Figure10-KSS.jpg",width = 10.5, height = 16.5, unit = "cm", dpi =350)



##############Linear Mixed Model Analysis
KSSExp2 = fread("data.csv")
KSSExp2 = KSSExp2[ID != 103]
KSSExp2 = KSSExp2[!is_outlier(KSSExp2$KSS, coef = 1.5) & !is.na(KSSExp2$KSS), ]

KSSExp2 = KSSExp2[, Scenario := fcase(Scenario == 1, 1.10,
                                      Scenario == 10, 10,
                                      Scenario == 100, 69.1,
                                      Scenario == 1000, 593)]
KSSExp2[, `:=` (               
  Timepoint = as.factor(Timepoint),
  ID = as.factor(ID),
  KSS = as.numeric(KSS)
)]


KSSExp2[, `:=` (
  Scenario = as.numeric(log10(Scenario)), 
  Timepoint = as.numeric(Timepoint)
)]

formula = "KSS ~ Scenario + I(Scenario^2) + Timepoint + Timepoint:Scenario + (1|ID)"
model = lmer(formula = formula, data = KSSExp2)
summary(model)
AIC(model)
BIC(model)

num_fixed_effects <- length(fixef(model))
p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
p_values_corrected <- p.adjust(p_values, method = "bonferroni", n = num_fixed_effects)
p_values_corrected

