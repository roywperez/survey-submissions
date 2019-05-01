library(tidyverse)
library(openxlsx)
library(here)

survey_data <- read.xlsx("data/survey_data1.xlsx", sheet = 1, colNames = TRUE, na.strings = TRUE)

names(survey_data)[1] = "CEName"
names(survey_data)[2] = "CEID"
names(survey_data)[3] = "ESC_Region"
names(survey_data)[10] = "professional_tracking_software"

###Create a new table for Chart###
survey_data1 <- survey_data%>%
  group_by(ESC_Region) %>% 
  count(ESC_Region, professional_tracking_software) %>%
  mutate(pct = n/sum(n))

###Stacked Bar Chart
p <- ggplot(survey_data1, aes(ESC_Region, n, fill = professional_tracking_software)) +
  geom_col(position = position_stack(reverse = TRUE)) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(aes(label = paste0(sprintf("%1.1f", pct*100),"%")),
            position = position_stack(reverse = TRUE, vjust = 0.75), size = 2.25, angle = 45) +
  geom_text(aes(label = n),
            position = position_stack(reverse = TRUE, vjust = 0), size = 2.25) +
  scale_x_continuous(expand = c(0,0), limits = c(0.5,20.5), breaks = 1:20) +
  scale_y_continuous(expand = c(0,0.7), limits = c(0,70)) +
  theme(plot.background = element_rect(fill = "grey87"),
        panel.background = element_rect(fill = "#BFD5E3", color = "#6D9EC1",
                                        size = 2, linetype = "solid"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) + 
  scale_fill_discrete(name = "Utilization of \nelectronic professional\nstandards tracking\nsoftware") +
  labs(title = "Survey for Utilization of Electronic Professional Standards Tracking Software",
       subtitle = "640 CE responses",
       x = "ESC Region",
       y = "CE Count",
       caption = "survey data from JotForm submission")

p

####
