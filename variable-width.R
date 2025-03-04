library(ggplot2)
library(dplyr)
library(scales)

total_complaints <- read.csv("data/total_complaints.csv")
# Filter for just 2023 data, select top 5 by complaint rate, and prepare it
df_2023_top5 <- total_complaints %>%
  filter(Year == 2023) %>%
  arrange(desc(complaint_rate_passengers)) %>%
  slice(1:5) %>%  
  mutate(
    
    xmin = lag(cumsum(enplaned_passengers_per_million), default = 0),
    xmax = cumsum(enplaned_passengers_per_million),
    xcenter = (xmin + xmax) / 2,
    
    passenger_label = paste0(format(round(enplaned_passengers_per_million), big.mark=","), "M"),
   
    highlight = ifelse(row_number() == 1, "highlight", "normal")
  )


ggplot(df_2023_top5) +
  
  geom_rect(
    aes(
      xmin = xmin, 
      xmax = xmax, 
      ymin = 0, 
      ymax = complaint_rate_passengers,
      fill = highlight
    ),
    color = "white",
    linewidth = 0.5
  ) +
 
  geom_text(
    aes(
      x = xcenter,
      y = -5, 
      label = Carrier
    ),
    size = 3.5,
    fontface = "bold"
  ) +
 
  geom_text(
    aes(
      x = xcenter, 
      y = complaint_rate_passengers,
      label = paste0(round(complaint_rate_passengers, 1), "\nper million\nper year")
    ),
    vjust = -0.5,
    size = 3
  ) +

  geom_text(
    aes(
      x = xcenter, 
      y = 0,
      label = passenger_label
    ),
    vjust = 1.5,
    size = 3
  ) +
  
  geom_text(
    aes(
      x = xcenter, 
      y = complaint_rate_passengers/2,
      label = paste0(format(Complaints, big.mark=","), "\ncomplaints"),
      color = highlight
    ),
    size = 3
  ) +
  
  scale_fill_manual(values = c("highlight" = "#F7E400", "normal" = "#D3D3D3"), guide = "none") +
  scale_color_manual(values = c("highlight" = "white", "normal" = "black"), guide = "none") +
 
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.1))) +
  labs(
    title = "Top 5 Airlines with Highest Disability Complaint Rates (2023)",
    subtitle = "Rates per million passengers by carrier",
    x = "U.S. airline passengers (2023)",
    y = "Complaint rate per million passengers"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),

)