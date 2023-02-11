library(googlesheets4)
library(dplyr)
library(ggplot2)

data_l<- read_sheet("https://docs.google.com/spreadsheets/d/1RpZAFLFT84gitnUg9s0mB0KEcYTuIWr9jKrpdj4vSo8/edit#gid=1061786364")
data_r <- read_sheet("https://docs.google.com/spreadsheets/d/1IVheNIn_Ns1w4kUd1-IMI3aVEUCui5N94JLQXBnDtCQ/edit#gid=908840308")

data_merge <- data_l %>% left_join( data_r, 
                                    by=c('Email Address'='Email'))%>%
  select(c(2:9), c(11:15)) %>%
  filter(Name != "NA")

responses <- dim(data_merge)[1]
responses

prof_tool <- data_merge %>% 
  mutate(proficiency=case_when(
    `Which of these tools do you have high proficiency in?` == "SQL" ~ "SQL",
    `Which of these tools do you have high proficiency in?` == "R" ~ "R",
    `Which of these tools do you have high proficiency in?` == "Python" ~ "Python",
    `Which of these tools do you have high proficiency in?` == "PowerBI" ~ "PowerBI",
    `Which of these tools do you have high proficiency in?` == "Excel" ~ "Excel",
    `Which of these tools do you have high proficiency in?` == "Microsoft Excel" ~ "Excel",
    `Which of these tools do you have high proficiency in?` == "None" ~ "None",
    `Which of these tools do you have high proficiency in?` == "None, Just Excel" ~ "Excel")) %>%
  count(proficiency) %>%
  arrange(n) %>%
  mutate(p = n / sum(n) )

prof_tab <- table(data_merge$`Which of these tools do you have high proficiency in?`)


pct = function(x, digits=1) {
  sprintf(paste0("%1.", digits, "f%%"), x*100)
}



proficiency <- ggplot(prof_tool, aes(x=reorder(proficiency, -n), y = n)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  labs(x="", y="") + 
  geom_text(aes(label=pct(p, 2), y = n + 0.1 )) + 
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())

specialization <- 
ggplot(level_pro, aes(x= `rate your level of proficiency`,y=n, fill=`Please state your interest of specialization`)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(legend.position = "top", legend.direction = "vertical")

specialization


ggplot(specialization, aes(x="", fill= `Please state your interest of specialization`, y=n)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  theme_void() +
  geom_text(aes(label=pct(p, 2), y = n + 0.1 )) 


mem_proj <- data_merge %>%
  select(`Have you worked on a project or you are currently working on one?`) %>%
  rename(project = `Have you worked on a project or you are currently working on one?`) %>%
  mutate(project = case_when(
    project == "No" ~ "No",
    TRUE ~ "Yes"
  ))
