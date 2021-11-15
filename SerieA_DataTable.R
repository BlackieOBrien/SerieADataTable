#Filename:Serie A 
#Author: Paul Ankers   
#Date Started: 25/10/2021
#Description:  Data Table Competition

Report_Name <- 'Taylor Swift' 

library(readxl)
library(data.table)
library(dplyr)
library(kableExtra)
library(formattable)

SerieA_Table <- read_excel("data/raw/SerieA.xlsx", 
                     sheet = "Table")
SerieA_SPI <- read_excel("data/raw/SerieA.xlsx", 
                     sheet = "SPI", range = "A1:F21")
SerieA_Websites <- read_excel("data/raw/SerieA.xlsx", 
                         sheet = "Websites")
SerieA_Champs <- read_excel("data/raw/SerieA.xlsx", 
                              sheet = "Champions")
#SerieA_Champs[Club == "Internazionale"] <- "Inter"


setDT(SerieA_Table)
setDT(SerieA_SPI)
setDT(SerieA_Websites)
setDT(SerieA_Champs)
setkey(SerieA_Table, "Team" )
setkey(SerieA_SPI, "Premier League 2020/21")
setkey(SerieA_Websites, "Team" )
setkey(SerieA_Champs, "Club" )

SerieA_Champs[, Club := 
                case_when(
                  Club == "Internazionale" ~ "Inter",
                  Club != "Internazionale" ~ SerieA_Champs$Club)]

comb <- SerieA_Table[SerieA_SPI ]
comb <- comb[SerieA_Websites, on = "Team"]
comb <- SerieA_Champs[comb, on = c("Club" = "Team")]

comb <- comb[order(Rank)]

comb$Won = cell_spec(comb$Won, color = "white", bold = T, background = ifelse(comb$Won > 20, "green", "red"))
comb$Drawn = cell_spec(comb$Drawn, color = "white", bold = T, background = ifelse(comb$Drawn > 12, "red", "green"))
comb$Lost = cell_spec(comb$Lost, color = "white", bold = T, background =ifelse(comb$Lost > 20, "red", "green"))
# comb$MP <- cell_spec(comb$MP, spec_popover(
#   content = "Test",
#   title = "Trying Stuff",
#   trigger = "hover",
#   position = "right"
# )) 
comb$GF <- color_tile("green", "orange")(comb$GF)
comb$GA <- color_tile("green", "orange")(comb$GA)
comb$GD <- color_tile("green", "orange")(comb$GD)

comb[, Stars := case_when(
   Champions >= 30 ~ "⭐⭐⭐",
   Champions >= 20 ~ "⭐⭐",
   Champions >= 10 ~ "⭐",
  TRUE ~ "")]  

FinOutput <- comb[, c("Rank", "Club",  "MP", "Won", "Drawn", "Lost", "GF", "GA", "GD", "Points", "Offense SPI", "Defense SPI",  "Champions", "Stars" )]

kbl(FinOutput , escape = F) %>%
  kable_paper("hover", full_width = F) %>%
  kable_material() %>%
  #FinOutput$Drawn = cell_spec(comb$Drawn, color = "white", bold = T, background = ifelse(FinOutput$Drawn > 12, "red", "green")) %>%
  add_header_above(header = c( "Serie A 2020/21" = 14), 
                   color = "white",
                   background = "blue",
                   font_size = 28) %>%
  row_spec(0, angle = 0, color = "white", background = "black", font_size = 12) %>%
  column_spec(1:14, bold = T) %>%
  column_spec(2, tooltip = "Team Name", link = comb$Website, new_tab = T) %>%
  column_spec(9, bold = T, border_right = T) %>%
  column_spec(10, tooltip = "Aggregation of 3 points for a win and 1 point for a draw", bold = T, border_right = T) %>%
  column_spec(3, bold = T, border_right = T) %>%
 # column_spec(11, color = "white", 
  #            background = spec_color(comb$`Offense SPI`[1:20], begin = 0.5, end = 0.1, option = "turbo"),
   #           popover = paste("am:", comb$`Offense SPI`[1:20])) %>%
  column_spec(11, tooltip = "Click for Offensive Soccer Power Index Explanation", link = 'https://www.espn.com/soccer/news/story/_/id/1873765') %>% 
  column_spec(12, tooltip = "Click for Defensive Soccer Power Index Explanation", link = 'https://www.espn.com/soccer/news/story/_/id/1873765') %>% 
  column_spec(13, tooltip = comb$`Winning seasons`) %>%
  column_spec(14, tooltip = "Stars are denoted for every 10 championships won") %>%
  column_spec(2, 1:4, bold = T, color = "white", background = "peru") %>%
  row_spec(18:20, bold = T, color = "white", background = "black") %>%
  footnote(general = "This is the final table of the Italian Serie A Football season 2020/21 ") %>%
  save_kable(file = "SerieA.html", self_contained = T)





  
