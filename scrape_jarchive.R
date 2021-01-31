# Setup

rm(list=ls())

library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

base_url <- "https://j-archive.com"


# get list of seasons
seasons_url <- file.path(base_url, "listseasons.php")
season_links <- read_html(seasons_url) %>% 
  html_node("table") %>% 
  html_nodes("a")

# loop through each season
games <- list(); k <- 1
for(sl in season_links){
  print(sl)
  season_url <- file.path(base_url, html_attr(sl, "href"))
  table_rows <- read_html(season_url) %>%
    html_node("table") %>%
    html_nodes("tr")
  for(i in 1:length(table_rows)){
    # read html table data
    cells <- table_rows[[i]] %>% html_nodes("td")
    link <- cells[[1]] %>% html_node("a") %>% html_attr("href")
    episode <- (cells[[1]] %>% html_text() %>% str_split(","))[[1]]
    ep_number <- as.integer(str_replace(episode[1], "\n\\s+#", ""))
    lubridate::as_date(str_replace(str_replace(episode[2], "\\s+aired\\s+", ""), "\n\\s+", ""))
    date <- lubridate::as_date(str_replace(str_replace(episode[2], "\\s+aired\\s+", ""), "\n\\s+", ""))
    contestants <- cells[[2]] %>% html_text() 
    description <- cells[[3]] %>% html_text()
    #games[[i]] = list("link" = link, "ep_number" = ep_number, "date" = date,
    #                "contestants" = contestants, "description" = description)
    print(ep_number)
    
    # get game info
    rounds <- read_html(link) %>% 
      html_nodes("[class='round']")
    if(length(rounds) == 0 ) {next}
    jclues <- rounds[[1]] %>% html_nodes("td") %>% html_nodes("[class='clue_header']")
    jorders <- c(); jvalues <- c()
    for(jclue in jclues){
      order_number <- jclue %>% html_nodes("[class='clue_order_number']") %>% html_text() %>% as.integer()
      clue_value <- jclue %>% html_nodes("[class='clue_value']") %>% html_text() %>% str_replace("\\$", "") %>% as.integer()
      if(length(clue_value) == 0){next}
      jorders <- c(jorders, order_number)
      jvalues <- c(jvalues, clue_value)
    }
    if(length(rounds) == 1 ) {next}
    djclues <- rounds[[2]] %>% html_nodes("td") %>% html_nodes("[class='clue_header']")
    djorders <- c(); djvalues <- c()
    for(djclue in djclues){
      order_number <- djclue %>% html_nodes("[class='clue_order_number']") %>% html_text() %>% as.integer()
      clue_value <- djclue %>% html_nodes("[class='clue_value']") %>% html_text() %>% str_replace("\\$", "") %>% as.integer()
      if(length(clue_value) == 0){next}
      djorders <- c(djorders, order_number)
      djvalues <- c(djvalues, clue_value)
    }
    
    # create data frames
    dfj <- data.frame("round"=rep("jeopardy", length(jorders)), "sequence"=jorders, "value"=jvalues)
    dfdj <- data.frame("round"=rep("double jeopardy", length(djorders)), "sequence"=djorders, "value"=djvalues)
    df_game <- rbind(dfj, dfdj) %>% mutate("game"=ep_number,
                                           "airdate"=date,
                                           "link"=link,
                                           "contestants"=contestants,
                                           "description"=description)
    games[[k]] <- df_game
    k <- k + 1
  }
}
df <- do.call("rbind", games)

saveRDS(df, "scraped_data.RDS")
df <- readRDS("scraped_data.RDS")


df$contestants <- df$contestants %>% 
  str_remove_all("\\n") %>%
  str_remove_all("\\s+")
df <- df %>% separate(contestants, sep="vs.", c("c1", "c2", "c3"), remove=F)
df$has_jh <- str_detect(df$contestants, "JamesHolzhauer")

df <- df %>% mutate(sequence2 = ifelse(round=="jeopardy", sequence, sequence + 30))
df %>% 
  filter(year(airdate) == 2019) %>%
  mutate(game = as.factor(game)) %>%
  group_by(airdate, game, round, has_jh) %>%
  arrange(sequence) %>%
  mutate(cumval = cumsum(value)) %>%
  summarize(ausv = sum(cumval)) %>%
  ggplot(aes(airdate, ausv, color=has_jh)) +
  geom_point(size=0.8) + 
    facet_grid(round~.) + 
  scale_color_manual(values=c("blue", "green"))

df2 <- df %>% 
  filter(year(airdate) == 2019) %>%
  mutate(game = as.factor(game),
         has_jh = factor(has_jh, levels=c(F,T))) %>%
  group_by(airdate, game, has_jh) %>%
  arrange(sequence2) %>%
  mutate(cumval = cumsum(value))

df2T <- df2[df2$has_jh=="TRUE",]
df2F <- df2[df2$has_jh=="FALSE",]

ggplot() +
  geom_rect(aes(xmin=0,  xmax=30, ymin=0, ymax=60000), fill="grey90", alpha=0.5) +
  geom_rect(aes(xmin=30, xmax=60, ymin=0, ymax=60000), fill="grey80", alpha=0.5) +
  geom_line(data=df2F, aes(sequence2, cumval, group=game, color=has_jh), alpha=0.05, size=1.0, color="black") + 
  geom_line(data=df2T, aes(sequence2, cumval, group=game, color=has_jh), alpha=0.4,  size=1.0, color="blue") + 
  annotate("text", x=15, y=55000, label="Jeopardy!",        size=8) + 
  annotate("text", x=46, y=55000, label="Double Jeopardy!", size=8) + 
  annotate("text", x=10, y=17000, label="Games with\nJames Holzhauer", color="blue", size=3) + 
  annotate("text", x=22, y=5000, label="Other\nGames", color="black", size=3) + 
  labs(title="2019 Jeopardy Season", 
       x="Clue Sequence", y="Cumulative Value", color="JH Game?")
ggsave("plot.png", height=8, width=13)

