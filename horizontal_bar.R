
library(ggplot2)
library(dplyr)
library(scales)
library(readr)

total_complaints <- read_csv("total_complaints.csv")

df_2023 <- total_complaints %>%
  filter(Year == 2023) %>%
  arrange(desc(complaint_rate_passengers)) %>%
  mutate(highlight = ifelse(row_number() == 1, "highlight", "normal"),
         complaint_label = paste0(format(Complaints, big.mark=","), " complaints"))

ggplot(df_2023, aes(x = reorder(Carrier, complaint_rate_passengers), y = complaint_rate_passengers)) +
  geom_bar(stat = "identity", aes(fill = highlight), width = 0.7) +
  
  geom_text(aes(label = paste0(round(complaint_rate_passengers, 1), " per million")), 
            hjust = 1, size = 3, color = "black") +
  
  coord_flip() +
  
  labs(
    title = "Airline Complaint Rates (2023)",
    subtitle = "Complaints per million passengers",
    x = NULL,  # Remove x-axis label as it's the carrier names
    y = "Complaints per million passengers"
  ) +
  
 
  scale_fill_manual(values = c("highlight" = "#F7E400", "normal" = "#D3D3D3"), guide = "none") +
  
 
  theme_minimal() +
  theme(
   
    text = element_text(size = 10),
    
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "#666666", hjust = 0.5),
    
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    
   
    axis.text = element_text(color = "#333333"),
    axis.text.y = element_text(face = "bold"),
    
   
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    
   
    plot.margin = margin(15, 50, 10, 10)
  ) +
  
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, 100))


ggsave("airline_complaints.svg", width = 375/72, height = 467/72, units = "in", dpi = 72)