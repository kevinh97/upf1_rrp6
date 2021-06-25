upf1_rrp6_ratios <- read.csv(file = "UPF1_rrp6_WT_intron_ratios.csv")
upf1_rrp6_ratios$WT_avg_ratio <- rowMeans(x = upf1_rrp6_ratios[,2:4])
upf1_rrp6_ratios$rrp6_avg_ratio <- rowMeans(x = upf1_rrp6_ratios[,5:7])
upf1_rrp6_ratios$upf1_avg_ratio <- rowMeans(x = upf1_rrp6_ratios[,8:10])
upf1_rrp6_ratios$u_r_avg_ratio <- rowMeans(x = upf1_rrp6_ratios[,11:13])
library(ggplot2)
upf1_rrp6_sig <- subset(upf1_rrp6_ratios, subset = upf1_rrp6_ratios$WT_vs_rrp6_pval<0.05)
upf1_rrp6_sig <- subset(upf1_rrp6_ratios, subset = upf1_rrp6_ratios$WT_vs_upf1_pval<0.05)
WT_vs_upf1 <-ggplot(data = upf1_rrp6_ratios, aes(x = upf1_avg_ratio, y = WT_avg_ratio)) +
  geom_point()
WT_vs_rrp6 <-ggplot(data = upf1_rrp6_ratios, aes(x = rrp6_avg_ratio, y = WT_avg_ratio)) +
  geom_point() + 
  ggtitle("WT vs rrp6 unspliced transcripts") +
  xlab("rrp6 intronic reads ratio") +
  ylab("WT intronic reads ratio")
