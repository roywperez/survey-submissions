library(tidyverse)
library(openxlsx)
library(here)

survey_data <- read.xlsx("survey_data2.xlsx", sheet = 1, colNames = TRUE, na.strings = TRUE)


names(survey_data)[1] = "CEName"
names(survey_data)[2] = "CEID"
names(survey_data)[3] = "ESC_Region"
names(survey_data)[4] = "Do_you_use_menu_planning_software"

####adding case when column

survey_data <- survey_data %>% 
  mutate(enrollment_size = case_when(Enrollment < 501 ~ "<500",
                                     Enrollment > 500 & Enrollment < 1001 ~ "501 to 1,000",
                                     Enrollment > 1000 & Enrollment < 2501 ~ "1,001 to 2,500",
                                     Enrollment > 2500 & Enrollment < 5001 ~ "2,501 to 5,000",
                                     Enrollment > 5000 & Enrollment < 10000 ~ "5,001 to 10,000",
                                     Enrollment > 10000 ~ "> 10,000"))
summary <- survey_data %>%
  filter(enrollment_size == "501 to 1,000",
         ESC_Region == 20,
         Do_you_use_menu_planning_software == "No")



  ###Create a new table for Chart###
survey_data1 <- survey_data%>%
  group_by(ESC_Region) %>% 
  count(ESC_Region, Do_you_use_menu_planning_software) %>%
  mutate(pct = n/sum(n))

### 1227 Total CEs



###Stacked Bar Chart
p <- ggplot(survey_data1, aes(ESC_Region, n, fill = Do_you_use_menu_planning_software)) +
  geom_col(position = position_stack(reverse = TRUE)) + 
  guides(fill = guide_legend(reverse = TRUE)) +

  geom_text(aes(label = n),
            position = position_stack(reverse = TRUE, vjust = .5), size = 4, fontface = "bold") +
  scale_x_continuous(expand = c(0,0), limits = c(0.5,20.5), breaks = 1:20) +
  scale_y_continuous(expand = c(0,0.7), limits = c(0,70), breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70)) +
  theme(plot.background = element_rect(fill = "grey87"),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) + 
  scale_fill_discrete(name = "Utilization of \nany meal\nplanning software") +
  labs(title = "Survey for utilization of menu planning software",
       subtitle = "640 CE responses, 352 Yes responses, 282 No responses, 6 did not answer",
       x = "ESC Region",
       y = "Respondent count by CE",
       caption = "Survey data from JotForm submission")

p

####
