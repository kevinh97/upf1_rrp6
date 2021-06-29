upf1_rrp6_ratios <- read.csv(file = "UPF1_rrp6_WT_intron_ratios.csv")
upf1_rrp6_ratios$WT_avg_ratio <- rowMeans(x = upf1_rrp6_ratios[,2:4])
upf1_rrp6_ratios$rrp6_avg_ratio <- rowMeans(x = upf1_rrp6_ratios[,5:7])
upf1_rrp6_ratios$upf1_avg_ratio <- rowMeans(x = upf1_rrp6_ratios[,8:10])
upf1_rrp6_ratios$u_r_avg_ratio <- rowMeans(x = upf1_rrp6_ratios[,11:13])
library(ggplot2)
upf1_rrp6_sig <- subset(upf1_rrp6_ratios, subset = upf1_rrp6_ratios$WT_vs_rrp6_pval<0.05)


upf1_WT_sig <- subset(upf1_rrp6_ratios, subset = upf1_rrp6_ratios$WT_vs_upf1_pval<0.05)
upf1_WT_notsig <- subset(upf1_rrp6_ratios, subset = upf1_rrp6_ratios$WT_vs_upf1_pval>0.05)

upf1_rrp6_ratios$upf1_rrp6_ratios_sig <- upf1_rrp6_ratios$WT_vs_upf1_pval<0.05

WT_vs_upf1 <-ggplot(data = upf1_rrp6_ratios, aes(x = WT_avg_ratio, y = upf1_avg_ratio, color = upf1_rrp6_ratios_sig,title=X1)) +
  geom_point() +
  ggtitle("WT vs upf1 unspliced transcripts, p-val < 0.05") +
  xlab("WT intronic reads ratio") +
  ylab("ufp1 intronic reads ratio")+
  geom_abline(slope = 1, intercept = 0) +
  geom_abline(slope = 1, intercept = 0.10)
  

WT_vs_rrp6 <-ggplot(data = upf1_rrp6_ratios, aes(x = WT_avg_ratio , y = rrp6_avg_ratio)) +
  geom_point() + 
  ggtitle("WT vs rrp6 unspliced transcripts") +
  xlab("WT intronic reads ratio") +
  ylab("rrp6 intronic reads ratio") +
  geom_abline(slope = 1, intercept = 0)+
  geom_abline(aes(color="red"), slope = 1, intercept = 0.10)

WT_vs_u_r <- ggplot(data = upf1_rrp6_ratios, aes(x = WT_avg_ratio, y = u_r_avg_ratio)) +
  geom_point() + 
  ggtitle("WT vs upf1rrp6 unspliced transcripts") +
  xlab("WT intronic reads ratio") +
  ylab("upf1rrp6 intronic reads ratio") +
  geom_abline(slope = 1, intercept = 0)+
  geom_abline(aes(color="red"), slope = 1, intercept = 0.10)

print(WT_vs_upf1)

ggplotly(WT_vs_upf1)
