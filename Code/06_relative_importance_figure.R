### brt figure

library(tidyverse)

out.dir <- "output_data/Manuscript/Figures/"
### CSCI 
gbm_fin_RI_csci <- read.csv("models/05_rel_imp_csci_labels_n1.csv")
gbm_fin_RI_asci <- read.csv("models/05_rel_imp_asci_labels_n1.csv")

gbm_fin_RI_csci <- gbm_fin_RI_csci %>%
  mutate(Index = "CSCI")

gbm_fin_RI_asci <- gbm_fin_RI_asci %>%
  mutate(Index = "ASCI")

gbm_fin_RI <- rbind(gbm_fin_RI_csci, gbm_fin_RI_asci)

gbm_fin_RI


c1 <- ggplot(data=gbm_fin_RI, aes(x=reorder(var,-rel.inf), y=rel.inf, fill = Flow.Component)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))+
  # scale_x_continuous(limits = c(0, 35)) +
  facet_wrap(~Index) +
  labs(title = "",
       x = "",
       y = "Relative Importance (%)") #+ theme_bw(base_size = 15)
c1

out.filename <- paste0(out.dir,"05_rel_imp_csci_asci_bar_plot_n1.jpg")
ggsave(c1, file = out.filename, dpi=300, height=4, width=6)

