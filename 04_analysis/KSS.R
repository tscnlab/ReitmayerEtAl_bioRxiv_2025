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


summary_dtKSS = KSSExp2[, .(mean_KSS = mean(KSS, na.rm = TRUE),
                          sd_KSS = sd(KSS, na.rm = TRUE),
                          median_KSS = median(KSS, na.rm = TRUE)),by = .(Timeframe,Scenario)]

summary_dtKSS[, `:=` (
  Scenario = as.factor(Scenario),
  Timeframe = as.factor(Timeframe)
)]


by(summary_dtKSS$median_KSS,summary_dtKSS$Scenario, shapiro.test)

model = aov(median_KSS ~ Scenario, data = summary_dtKSS)
summary(model)


##### Code for Figures

(KSSmeansd = ggplot(summary_dtKSS, aes(x = Timeframe, y = mean_KSS, group = Scenario, colour = Scenario))+
  geom_line()+
  geom_ribbon(aes(ymin = mean_KSS - sd_KSS, ymax = mean_KSS + sd_KSS, fill = Scenario),alpha = 0.6, linewidth = 0.25)+
  #scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,11), labels = c("1\nExtremely\nalert","5","9\nExtremely\nsleepy"))+
  scale_y_continuous(breaks = seq(1, 9, 1), limits = c(1,9.3), labels = c("1\nExtr.\nalert","","","","5","","","","9\nExtr.\nsleepy"))+
  scale_x_discrete(labels = c("1","","","", "5","","","","", "10"))+
  labs(#title = "Karolinska Sleepiness Scale",
       x = "Time point",
       y = "KSS Sleepiness",
       color = "Scenario [lx]",
       fill = "Scenario [lx]")+
  facet_wrap(~Scenario, nrow = 1, labeller = as_labeller(c("1" = "1 lx", "10" = "10 lx", "100" = "100 lx", "1000" = "1000 lx")))+
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
print(KSSmeansd)

my_comparisonsdtKSS = list(c("1 lux", "10 lux"),c("1 lux", "100 lux"),c("1 lux", "1000 lux"),c("10 lux", "100 lux"),c("10 lux", "1000 lux"), c("100 lux", "1000 lux"))

set.seed(555)
(KSSMedianKSSAll=ggplot(summary_dtKSS, aes(x = Scenario, y = median_KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(#title = "Karolinska Sleepiness Scale",
       x = "Photopic illuminance [lx]",
       y = "KSS Sleepiness",
       color = "Scenario [lx]",
       fill = "Scenario [lx]")+
  stat_pwc(hide.ns = TRUE, label = "p.adj.signif", y.position = 9, group.by = "x.var", method = "tukey_hsd", step.increase = 0.15, p.adjust.method = "bonferroni", parse = FALSE)+
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(1, 9, 1), limits = c(1,9.3), labels = c("1\nExtr.\nalert","","","","5","","","","9\nExtr.\nsleepy"))+
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
print(KSSMedianKSSAll)


ggarrange(KSSMedianKSSAll, NULL, KSSmeansd, hjust = -0.1, vjust = 1.2, nrow =3, common.legend = TRUE, legend = "bottom",labels = c("A", "B"), heights = c(2.1,0.1, 1.0), font.label = list(size=10))
ggsave(filename = "Figure10-KSS.jpg",width = 10.5, height = 16.5, unit = "cm", dpi =350)



##### Linear Mixed Model Analysis

KSSExp2[, `:=` (
  Scenario = as.factor(Scenario),               
  Timeframe = as.factor(Timeframe),
  ID = as.factor(ID),
  KSS = as.numeric(KSS)
)]


KSSExp2[, `:=` (
  Scenario = as.numeric(Scenario)
)]

KSSExp2[, `:=` (
  Scenario = as.numeric(log10(Scenario)), 
  Timeframe = as.numeric(Timeframe)
)]

formula = "KSS ~ Scenario + I(Scenario^2) + Timeframe + Timeframe:Scenario + (1|ID)"
model = lmer(formula = formula, data = KSSExp2)
summary(model)
AIC(model)
BIC(model)

num_fixed_effects <- length(fixef(model))
p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
p_values_corrected <- p.adjust(p_values, method = "bonferroni", n = num_fixed_effects)
p_values_corrected

