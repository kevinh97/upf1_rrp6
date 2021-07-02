#To Do
#Do t-tests on singles to u_r

#Make plots of the upf1 and rrp6 total RNA seq data

#Library stuff####
library(ggplot2)
library(plotly)
library(ggVennDiagram)

#import datasets and create new dataframes####
upf1_rrp6_ratios <- read.csv(file = "UPF1_rrp6_WT_intron_ratios.csv")
upf1_rrp6_ratios$WT_avg_ratio <- rowMeans(x = upf1_rrp6_ratios[,2:4])
upf1_rrp6_ratios$rrp6_avg_ratio <- rowMeans(x = upf1_rrp6_ratios[,5:7])
upf1_rrp6_ratios$upf1_avg_ratio <- rowMeans(x = upf1_rrp6_ratios[,8:10])
upf1_rrp6_ratios$u_r_avg_ratio <- rowMeans(x = upf1_rrp6_ratios[,11:13])
upf1_rrp6_ratios$predicted_rrp6_upf1 <- ((upf1_rrp6_ratios$rrp6_avg_ratio)/(upf1_rrp6_ratios$WT_avg_ratio))*((upf1_rrp6_ratios$upf1_avg_ratio)/(upf1_rrp6_ratios$WT_avg_ratio))
upf1_rrp6_ratios$actual_rrp6_upf1_vs_wt <- ((upf1_rrp6_ratios$u_r_avg_ratio)/(upf1_rrp6_ratios$WT_avg_ratio))

#subset data based on p-val and biological sig####
#Subset to significant p-vals
rrp6_WT_sig <- subset(upf1_rrp6_ratios, subset = upf1_rrp6_ratios$WT_vs_rrp6_pval<0.05)
#calculate diffrence from samples
rrp6_WT_sig$log_ratio_wt_rrp6 <- (log10(rrp6_WT_sig$rrp6_avg_ratio/rrp6_WT_sig$WT_avg_ratio) >= .25)

#Subset to significant p-vals
upf1_WT_sig <- subset(upf1_rrp6_ratios, subset = upf1_rrp6_ratios$WT_vs_upf1_pval<0.05)
#calculate diffrence from samples
upf1_WT_sig$log_ratio_wt_upf1 <- (log10(upf1_WT_sig$upf1_avg_ratio/upf1_WT_sig$WT_avg_ratio) >= .25)

#Subset to significant p-vals
u_r_sig <- subset(upf1_rrp6_ratios, subset = upf1_rrp6_ratios$WT_vs_u_r_pval<0.05)
#calculate diffrence from samples
u_r_sig$log_ratio_wt_u_r <- (log10(u_r_sig$u_r_avg_ratio/u_r_sig$WT_avg_ratio) >= .25)

#Deprciated sorta
upf1_rrp6_ratios$upf1_rrp6_ratios_sig <- upf1_rrp6_ratios$WT_vs_upf1_pval<0.05

#Make plots####
WT_vs_upf1 <-ggplot(data = upf1_rrp6_ratios, aes(x = WT_avg_ratio, y = upf1_avg_ratio, color = upf1_rrp6_ratios_sig,title=X1)) +
  geom_point() +
  ggtitle("WT vs upf1 unspliced transcripts, p-val < 0.05") +
  xlab("WT intronic reads ratio") +
  ylab("ufp1 intronic reads ratio")+
  geom_abline(slope = 1, intercept = 0) +
  geom_abline(slope = 1, intercept = 0.10)
  

WT_vs_rrp6 <-ggplot(data = rrp6_WT_sig, aes(x = WT_avg_ratio , y = rrp6_avg_ratio, title = X1)) +
  geom_point() + 
  ggtitle("WT vs rrp6 unspliced transcripts") +
  xlab("WT intronic reads ratio") +
  ylab("rrp6 intronic reads ratio") +
  geom_abline(slope = 1, intercept = 0)+
  geom_abline(aes(color="red"), slope = 1.25, intercept = 0.0)

WT_vs_u_r <- ggplot(data = upf1_rrp6_ratios, aes(x = WT_avg_ratio, y = u_r_avg_ratio)) +
  geom_point() + 
  ggtitle("WT vs upf1rrp6 unspliced transcripts") +
  xlab("WT intronic reads ratio") +
  ylab("upf1rrp6 intronic reads ratio") +
  geom_abline(slope = 1, intercept = 0)+
  geom_abline(aes(color="red"), slope = 1, intercept = 0.10)

WT_vs_rrp6_log <-ggplot(data = rrp6_WT_sig, aes(x = WT_avg_ratio , y = rrp6_avg_ratio, color = log_ratio_wt_rrp6, title = X1)) +
  geom_point() + 
  ggtitle("WT vs rrp6 unspliced transcripts") +
  xlab("WT intronic reads ratio") +
  ylab("rrp6 intronic reads ratio") +
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope = 1, intercept = 0)+
  geom_abline(aes(color="red"), slope = 1, intercept = 0.25)

WT_vs_upf1_log <-ggplot(data = upf1_WT_sig, aes(x = WT_avg_ratio , y = upf1_avg_ratio, color = log_ratio_wt_upf1, title = X1)) +
  geom_point() + 
  ggtitle("WT vs upf1 unspliced transcripts") +
  xlab("WT intronic reads ratio") +
  ylab("upf1 intronic reads ratio") +
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope = 1, intercept = 0)+
  geom_abline(aes(color="red"), slope = 1, intercept = 0.25)

WT_vs_u_r_log <-ggplot(data = u_r_sig, aes(x = WT_avg_ratio , y = u_r_avg_ratio, color = log_ratio_wt_u_r, title = X1)) +
  geom_point() + 
  ggtitle("WT vs upf1_rrp6 unspliced transcripts") +
  xlab("WT intronic reads ratio") +
  ylab("upf1_rrp6 intronic reads ratio") +
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope = 1, intercept = 0)+
  geom_abline(aes(color="red"), slope = 1, intercept = 0.25)

predicted_vs_actual <- ggplot(data = u_r_sig, aes(x = predicted_rrp6_upf1, y = actual_rrp6_upf1_vs_wt)) +
  geom_point() + 
  ggtitle("Predicted effect vs actual effect") +
  xlab("Predicted effect of double mutant") +
  ylab("Observed effect of double mutant") +
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(method='lm')

#ggplotly(predicted_vs_actual)
#ggplotly(WT_vs_upf1)
#ggplotly(WT_vs_rrp6)
#ggplotly(WT_vs_u_r)
#ggplotly(WT_vs_rrp6_log)
#ggplotly(WT_vs_upf1_log)
#ggplotly(WT_vs_u_r_log)

ggsave(filename = "~/Desktop/upf1_rrp6/upf1_rrp6/plots/predicted_vs_actual.png", plot = predicted_vs_actual)

#VennDiagrams####
rrp6_WT_ratio_true <- subset(rrp6_WT_sig , subset = rrp6_WT_sig$log_ratio_wt_rrp6)
upf1_WT_ratio_true <- subset(upf1_WT_sig , subset = upf1_WT_sig$log_ratio_wt_upf1)
ggVennDiagram(x = list(as.character(rrp6_WT_ratio_true$X1), as.character(upf1_WT_ratio_true$X1)), 
              show_intersect = TRUE)
plotly(rrp6_upf1_venn)
ggVennDiagram(x = list(as.character(rrp6_WT_sig$X1), as.character(u_r_sig$X1)), 
              show_intersect = TRUE)
ggVennDiagram(x = list(as.character(rrp6_WT_sig$X1), as.character(upf1_WT_sig$X1)), 
              show_intersect = TRUE)
