library(lattice)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyhelper)
library(dplyr)
library(tm)
library(wordcloud)
library(purrr)
library(readr)
library(stringr)
library(magrittr)
library(reactable)
library(NLP)
library(RColorBrewer)
library(twitteR)
library(gcookbook) 
library(viridis)  
library(forcats)
library(gridExtra)
library(grid)
library(scales)
library(caret)
library(MLeval)
library(ggrepel)
library(rtweet)
library(reactable)
library(glue)
library(stringr)
library(httpuv)
#library loads
library(readxl)
library(dplyr)
library(shiny)
library(ggplot2)
library(psych)
library(data.table)
library(rsconnect)
library(broom)
library(lpSolve)
library(lpSolveAPI)
library(shinythemes)
library(cluster)
library(randomForest)
library(xgboost)
library(caret)
library(h2o)
library(pdp)
library(corrplot)
library(pROC)
library(magick)
library(stringr)
library(circlize)
library(fmsb)
library(ggradar)
library(remotes)
library(tidyverse)
library(scales)
library(highcharter)
#library(png)
#remotes::install_github("ricardo-bion/ggradar")

options(warn=-1)

seed_val=2401

#Working directory
getwd()
setwd("C:/Users/utkar/Documents/Purdue Academics/Module 2/Fall_2021_MGMT_59000_Using R/Shiny app video")
`%nin%` = Negate(`%in%`)
rsq <- function (x, y) cor(x, y) ^ 2

#data load
atp <- read.csv("df_atp.csv",header=TRUE)
summary(atp)
str(atp)

#atp_city
atp_city <- read.csv("world_atp_latlong.csv",header=TRUE)
summary(atp_city)
#114
atp_city <- atp_city %>% mutate(Location = str_trim(Location))

##atp_rank#####
atp_rank <- read.csv("atp_rank.csv",header=TRUE)
summary(atp_rank)
str(atp_rank)
atp_rank <- atp_rank %>% 
  filter(Rank!="-") %>%
  mutate(Rank=as.numeric(Rank), Points = as.numeric(Points)) %>%
  filter(Rank <=50)

#######QC

#replace na with 0
atp[is.na(atp)] <- 0

#Adding year column
atp$Date=as.Date(atp$Date)
atp$Year <- as.numeric(format(atp$Date,"%Y"))


#1 records with Lsets= `1
table(atp$Lsets)
atp <- atp %>% mutate(Lsets = ifelse(Lsets=="`1",1,as.numeric(Lsets)))
table(atp$Lsets)

#18 records with Best.of= 3 and Wsets=3
table(atp$Wsets,atp$Best.of)
atp <- atp %>% mutate(Best.of = ifelse(Best.of==3 & Wsets==3,5,Best.of))
table(atp$Wsets,atp$Best.of)

#name updates
table(nchar(atp$Winner))
table(nchar(atp$Loser))
atp <- atp %>% mutate(
  Winner = str_trim(Winner),
  Loser  = str_trim(Loser),
  Location = str_trim(Location))

table(nchar(atp$Winner))
table(nchar(atp$Loser))

table(atp$Court,nchar(atp$Court))
table(atp$Surface,nchar(atp$Surface))

#merge city with location
atp <- merge(atp,atp_city,by.x="Location",by.y="Location",all.x=TRUE)
summary(atp)

rm(atp_city,city)
################################################################################
#Player_Information Data
#winner summary
win <- atp %>%
  mutate(player=Winner) %>%
  group_by(player) %>% 
  summarize(
    type = "Wins",
    n = n(),
    #Indoor wins
    indoor = sum(ifelse(Court=="Indoor",1,0)),
    #Outdoor wins
    outdoor = sum(ifelse(Court=="Outdoor",1,0)),
    #Carpet wins
    carpet = sum(ifelse(Surface=="Carpet",1,0)),
    #Clay wins
    clay = sum(ifelse(Surface=="Clay",1,0)),
    #Grass wins
    grass = sum(ifelse(Surface=="Grass",1,0)),
    #Hard wins
    hard = sum(ifelse(Surface=="Hard",1,0)),
    avg.bet= mean(AvgW),
    avg.sets = mean(Wsets),
    #Best3 -> y = -0.5x + 2.5
    avg.sets.Best3 = mean(Wsets*(-0.5*Best.of + 2.5)),
    #Best5 -> y =  0.5x - 1.5
    avg.sets.Best5 = mean(Wsets*( 0.5*Best.of - 1.5))
  ) 
#938

los <- atp %>% 
  mutate(player=Loser) %>%
  group_by(player) %>% 
  summarize(
    type="Losses",
    n = n(),
    #Indoor loss
    indoor = sum(ifelse(Court=="Indoor",1,0)),
    #Outdoor loss
    outdoor = sum(ifelse(Court=="Outdoor",1,0)),
    #Carpet loss
    carpet = sum(ifelse(Surface=="Carpet",1,0)),
    #Clay loss
    clay = sum(ifelse(Surface=="Clay",1,0)),
    #Grass loss
    grass = sum(ifelse(Surface=="Grass",1,0)),
    #Hard loss
    hard = sum(ifelse(Surface=="Hard",1,0)),
    avg.bet= mean(AvgL),
    avg.sets = mean(Lsets),
    #Best3 -> y = -0.5x + 2.5
    avg.sets.Best3 = mean(Lsets*(-0.5*Best.of + 2.5)),
    #Best5 -> y =  0.5x - 1.5
    avg.sets.Best5 = mean(Lsets*( 0.5*Best.of - 1.5))
  ) 
#1450

#players data
players <- rbind(win,los)
#2388
#cleanup
rm(win,los)

#merge with rank and total game
player_tot <- players %>% 
  group_by(player) %>%
  summarize(tot=sum(n))
#1450
atp_rank <- merge(atp_rank,player_tot,by="player",
                  all.x=TRUE)

rm(player_tot)

#merge with rank and points
player50 <- merge(atp_rank,players,by="player",
                  all.x=TRUE)

player50[is.na(player50)] <- 0.00

colnames(player50)
player50 <- player50 %>%
  mutate(overall.perc = round(n*100/tot,2),
         indoor.perc = round(indoor*100/n,2),
         outdoor.perc = round(outdoor*100/n,2),
         carpet.perc = round(carpet*100/n,2),
         clay.perc = round(clay*100/n,2),
         grass.perc = round(grass*100/n,2),
         hard.perc = round(hard*100/n,2))

#replace na with 0
player50[is.na(player50)] <- 0

#####radar plot: https://www.r-graph-gallery.com/web-radar-chart-with-R.html
#colnames(player50)[1]="player"
filter_player="Djokovic N."

atp_r <- player50 %>% filter(player==filter_player)
atp_r <- subset(atp_r,select= c("type",
                    "indoor.perc","outdoor.perc",
                    "carpet.perc","clay.perc","grass.perc","hard.perc"))
#atp_r <- atp_r %>% filter(player==filter_player)

rad1 <- atp_r %>%
  ggradar(
    grid.max = 100,
    grid.label.size = 0.1,  # Affects the grid annotations (0%, 50%, etc.)
    axis.label.size = 3, # Affects the names of the variables
    group.point.size = 3, # Simply the size of the point 
    plot.title = paste(filter_player),
    legend.text.size=8,
    legend.position = "bottom")
rad1

san_win <- atp %>% 
  filter(Tournament=="Wimbledon" &
           Round %in% c("The Final") &
           Winner == filter_player ) %>%
  group_by(Series,Year) %>% 
  summarize(Results= "Champion")
san_los <- atp %>% 
  filter(Tournament=="Wimbledon" &
           Round %in% c("The Final","Semifinals") &
           Loser == filter_player ) %>%
  group_by(Series,Year) %>% 
  summarize(Results= ifelse(Round=="The Final","Runner-Up","Knocked-Out"))

san <- rbind(san_win,san_los)  
t1 <- sample(x = c(paste(filter_player,"Grand Slam Wimbledon")) , size = 1, replace=TRUE)
t2 <- sample(san$Year, replace=TRUE)
t3 <- sample(san$Results , replace=TRUE)

rm(san_win,san_los)
san <- data.frame(cbind(t1,t2,t3))
#d <- merge(d,san,)

rad2 <- hchart(data_to_sankey(san), "sankey", 
               name = paste(filter_player,": Years, Finals in Wimbledon and Results"))
rad2

################################################################################
#Tournament_Information Data
#tournament profile
tour_map <- atp %>% 
  mutate(round_robin = ifelse(Round=="Round Robin",1,0)) %>%
  filter(Comment =="Completed") 
tour_map <- tour_map %>%
  group_by(Series,Tournament,Location,lat,lng,Surface,Court,Best.of) %>% 
  summarize(Round.Robin = max(Round),
            Tour.Type   = ifelse(Round.Robin==1,"Round Robin","Knock Out"))
tour_map <- unique(tour_map)

###tournament map
world <- map_data("world")
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "gray", fill = "lightgray", size = 0.1 )+
  geom_point(
    data = tour_map,
    aes(lng, lat, color = Series),
    alpha = 0.7 )+
  theme_void() +
  theme(legend.position = "none") +
  labs(title="Tennis Tournament Locations")


tour_view <- subset(atp,select=c('Series','Tournament',
                                 'Year','Winner','Loser','Round'))


str(tour_view)                                 

table(tour_view$Round)
#Tournament Winner
tour_1 <- tour_view %>% 
  filter(Round %in% c("Quarterfinals","Semifinals","The Final")) %>%
  group_by(Series,Tournament,Year,Round) %>% 
  summarize(Player = Winner, Result = "Winner")
tour_1 <- unique(tour_1)

tour_2 <- tour_view %>% 
  filter(Round %in% c("Quarterfinals","Semifinals","The Final")) %>%
  group_by(Series,Tournament,Year,Round) %>% 
  summarize(Player = Loser, Result = "Loser")
tour_2 <- unique(tour_2)

tour <- rbind(tour_1,tour_2)
rm(tour_1,tour_2)

table(tour$Round)
tour <- unique(tour)
#replace na with 0
tour[is.na(tour)] <- "0"


################################################################################
#Twitter data
tweet_df <- search_tweets("#tennis",n=100,include_rts = FALSE)
names(tweet_df)

tweets <- tweet_df %>% select(user_id,status_id,created_at,screen_name,text,favorite_count,retweet_count,urls_expanded_url)



tweet_table_data <- tweet_df %>%
  select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
  mutate(
    Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>") 
  )%>%
  select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs = urls_expanded_url)

#Function to make url
make_url_html <- function(url) {
  if(length(url) < 2) {
    if(!is.na(url)) {
      as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
    } else {
      ""
    }
  } else {
    paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
  }
}

tweet_table_data$URLs <- purrr::map_chr(tweet_table_data$URLs, make_url_html)



reactable::reactable(tweet_table_data)

reactable(tweet_table_data, 
          filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
          showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
          columns = list(
            DateTime = colDef(defaultSortOrder = "asc"),
            User = colDef(defaultSortOrder = "asc"),
            Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
            Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
            RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
            URLs = colDef(html = TRUE)
          )
)



##############Prediction
atp_lm_win <- subset(atp,select = c("Winner","Year","AvgW","Wsets"))
atp_lm_win <- atp_lm_win %>% 
  mutate(Results = 1, player = Winner, type="Wins", avg.bet=AvgW, avg.sets=Wsets) %>% 
  filter(Year>2005)
atp_lm_win$Winner <- NULL
atp_lm_win$AvgW <- NULL
atp_lm_win$Wsets <- NULL

atp_lm_los <- subset(atp,select = c("Loser","Year","AvgL","Lsets"))
atp_lm_los <- atp_lm_los %>% 
  mutate(Results = -1, player = Loser,type="Losses", avg.bet=AvgL, avg.sets=Lsets) %>% 
  filter(Year>2005)
atp_lm_los$Loser <- NULL
atp_lm_los$AvgL <- NULL
atp_lm_los$Lsets <- NULL

atp_lm <- rbind(atp_lm_los,atp_lm_win)
atp_lm$Year <- NULL
#74960
rm(atp_lm_los,atp_lm_win)

atp_lm <- merge(atp_lm,atp_rank,by="player",all.y=TRUE)
#10783
colnames(atp_lm)
atp_lm$Results <- atp_lm$Results*atp_lm$tot

atp_lm <- subset(atp_lm,
                 select = c("Results",
                            "tot",
                            "Points",
                            "Rank",
                            "avg.bet","avg.sets"))
str(atp_lm)

y <- "Results"
x <- setdiff(colnames(atp_lm),y)
X <- paste(x, collapse = '+')
lm.fit <- lm(formula = as.formula(paste(y,"~",X)),
             data = atp_lm)
summary(lm.fit)

p1 <- "Bautista Agut R."
p2 <- "Basilashvili N."
predict_p1 <- player50 %>% filter(player==p1 & type=="Wins")
p1_win <- predict(lm.fit,predict_p1)
predict_p2 <- player50 %>% filter(player==p2 & type=="Wins")
p2_win <- predict(lm.fit,predict_p2)

pred.results <- ifelse(p1_win>p2_win,p1,p2)
pred.results <- unique(pred.results)
pred.probability <- (p1_win-p2_win)/p2_win +0.5



########################App UI Starts########################################

title <- tags$p(tags$img(src="atp.png", height="50", width="50"),"ATP Tour")

dashHeader <- dashboardHeader(title = title,
                              titleWidth= 350
)

dashbar <- dashboardSidebar(
  width=350,
  sidebarMenu(id="my_menu",
              menuItem('Home', tabName = "HomeTab", 
                       icon=icon('home')
              ),
              menuItem('Trending',tabName = 'Trending',
                       icon=icon('twitter-square')
              ),
              menuItem('Players',tabName = 'PlayerProfile',
                       icon=icon('running')
              ),
              menuItem('Tournaments',tabName = 'Tournament',
                       icon=icon('users')
              ),
              menuItem('Predict Winner',tabName = 'Prediction',
                       icon=icon('medal')
              ),
              menuItem('Acknowledgement',tabName = 'Acknowledgement',
                       icon=icon('battle-net')
              )
              
  )
)

dashBody <- dashboardBody(
  
  tabItems(
    tabItem(tabName = 'HomeTab',
            h1('ATP Tour Dashboard', style="text-align:center"),
            p("As a global governing body of men's professional tennis, the ATP's mission is to serve tennis. They entertain a billion global fans, showcase the world's greatest players at the most prestigious tournaments, and inspire the next generation of fans and players. From the ATP Cup in Australia, to Europe, the Americas and Asia, the stars of the game battle for titles and FedEx ATP Rankings points at ATP Masters 1000, 500 and 250 events, and Grand Slams. All roads lead towards the Nitto ATP Finals, the prestigious season finale held in Turin, Italy. Featuring only the world's top 8 qualified singles players and doubles teams, the tournament also sees the official crowning of the year-end ATP World No. 1, the ultimate achievement in tennis."),
            p('Source: atptour.com',style="text-align:right;font-style:italic;"),
            fluidRow(
              column(width=12,tags$iframe(width="560" ,height="315" ,src="https://www.youtube.com/embed/7LaSrjkcn0A?controls=0",title="YouTube video player" ,frameborder="0", allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" ,allowfullscreen=NA))
            ),
    ),
    tabItem(tabName = 'Trending',
            fluidRow(column(width=7,box(title="Trending Words in the Tennis World",width=12, solidHeader=T, status="info",
                                        plotOutput('tweetplot', width="100%", height="600px"))),
                     column( width = 5,
                             tags$script(src = "https://platform.twitter.com/widgets.js", charset = "utf-8"),
                             tags$link(href = "https://fonts.googleapis.com/css?family=Roboto+Mono", rel = "stylesheet"),
                             tags$style(HTML('*#sidebar {background-color: #fff;border: 0px;}
                             .rt-th {display: none;}
                             .rt-noData {display: none;}
                             .rt-pagination-nav {float: left;width: 100%;}')),
                             box(title="#Tennis Tweets",status="info",width=12, solidHeader=T,reactableOutput("embedded_user_tweets_table", width = "400px"))
                     )
            )
    ),
    tabItem(tabName = 'PlayerProfile',
            fluidRow(column(width=3,selectInput(inputId = "choose_player", label = "Select a Player:", choices = atp_rank$player)),
                     column(width=5, h2(textOutput("player_name"), style="font-weight:bold;text-align:center;"))), 
            br(),
            h2("Ranking"),
            br(),
            fluidRow(column(width=7,box(title="ATP Singles Ranking",height="300",width="300",
                                        solidHeader=T,tableOutput("ranking"), status="danger")),
                    column(width=3, align="center",box(title="Player Picture",
                                     solidHeader=T, height="440",width="100%",status="danger",
                                                            br(), br(),imageOutput("playerimage", height="200px"))),
                                          br()),
            h2("Player Statistics"), 
            br(),
            fluidRow(column(width=7,box(title="Individual Player Statistics", solidHeader=T,status="warning",
                                        dataTableOutput("top50_player_stats"), width=12)),
                     column(width=3,align="center",box(title="Player Picture",solidHeader=T, height="440",
                                                       width="100%",status="warning",br(), br(),imageOutput("playerimage", height="200px"))),
                     
                     br()),
            fluidRow(box(title="Winning Distribution", solidHeader=T,status="warning",
                         plotOutput("win_graph"), width=12)),
                        ),
    tabItem(tabName = 'Tournament',
            fluidRow(column(width=3,selectInput(inputId = "tid",
                                                label = "Select a Tournament",
                                                "tour$Tournament")),
                     column(width=5, h2(textOutput("tournament_name"), style="font-weight:bold;text-align:center;"))),
            fluidRow(width=5,box(title="Tournament World Map",solidHeader=T,status="success", 
                                 plotOutput("tournament_map"),width=15)),
            
            br(),
            tabsetPanel(type = "tabs",
                        tabPanel("Overall",
                                 fluidRow(box(title="Tournament Record", status="info",solidHeader=T,
                                              tableOutput("table1"), width=12)),
                                 
                        ),
                        tabPanel("Year",
                                 fluidRow(column(width=3,selectInput(inputId = "yid",label= "Select Year","tour$Year")),
                                          column(width=5, h2(textOutput("tournament_name_Year"), 
                                                             style="font-weight:bold;text-align:center;"))),
                                 
                                 fluidRow(box(title="Tournament-Year", status="info", solidHeader=T,width=12,
                                              plotOutput("tour_year")))
                        )
                        
            ),       
    ),
    tabItem(tabName = 'Prediction',
            fluidRow(
              column(width=3,selectInput(inputId = "player1", label = "Select Player 1", 
                                         choices =player50$player, selected="1610612738")),
              column(width=2),
              column(width=3,selectInput(inputId = "player2", label = "Select Player 2", 
                                         choices =player50$player, selected="1610612751")),
              
            ),
            br(),
            fluidRow(column(width=3,imageOutput("player1", height="280px")),
                     column(width=2,br(),br(),br(),br(),img(src="versus.png",width="100",height="100"), align="center"),
                     column(width=3,imageOutput("player2", height="280px"))),
            br(),
            fluidRow(
              column(width=4, box(title = "The Winner is:",solidHeader = T,span(uiOutput('table2'), 
                                                                            style ='color:blue'), width="100")),
              column(width=4, box(title = 'Probability of winning',solidHeader = T,span(uiOutput('table1'), 
                                                                            style ='color:red'), width="100"))
            )
    ),
    tabItem(tabName = 'Acknowledgement',
            fluidRow(h3('  Professor Matthew Lanham', style="text-align:left"),
                     imageOutput("lahnam",height="280px"),
                     h5('   His favourite tennis player is Roger Federer', style="text-align:left")),
            fluidRow(h3('  Aman Bhargava', style="text-align:left"),
                     imageOutput("bhargava",height="280px"),
                     h5('   His favourite tennis player is Rafael Nadal', style="text-align:left")),
            fluidRow(h3('  Utkarsh Bajaj', style="text-align:left"),
                     imageOutput("bajaj",height="280px"),
                     h5('   His favourite tennis player is Andy Murray ', style="text-align:left")),
            fluidRow(h3('  Padma Dwivedi', style="text-align:left"),
                     imageOutput("dwivedi",height="280px"),
                     h5('   Her favourite tennis player is Roger Federer', style="text-align:left")),
            fluidRow(h3('  Rubin Mao', style="text-align:left"),
                     imageOutput("mao",height="280px"),
                     h5('   His favourite tennis player is Novak Djokovic ', style="text-align:left"))
    )
    
    
  )
)

ui <- dashboardPage (skin="purple",
                     header=dashHeader,
                     sidebar = dashbar,
                     body= dashBody,
                     title = 'ATP Tour'
)

server<-function(input,output,session){
  
  ###############################
  ##### Twitter Timeline #####
  user_tweets <- reactive(
    tweets %>%
      select(status_url, created_at, favorite_count, retweet_count) %>%
      arrange(desc(created_at))
  )
  
  output$user_links <- renderUI(
    tagList(make_links(paste0("https://twitter.com/", input$user_name), "Twitter", "\U0001F4AC"))  # speech bubble
  )
  
  output$embedded_user_tweets_table <- renderReactable(
    user_tweets() %>%
      select(status_url) %>%
      make_embedded_tweets_table()
  )
  ############################
  #### Word Cloud ######  
  output$tweetplot <- renderPlot({
    wordcloud <- wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  
                           random.color= TRUE, random.order = FALSE,max.words=300)
  })
  ############################
  
  ############################
  #### Player Profile ######
  output$player_name <- renderText({player_name <- as.character(atp_rank %>% 
                    filter(player==input$choose_player) %>%select(player))})
  
  output$ranking <- renderTable({ranking <- atp_rank %>% filter(player==input$choose_player)})
  
  #output$playerimage <- renderImage({})
  
  colnames(player50)[6]="Result"
  
  output$top50_player_stats <- renderDataTable({player50 %>% filter(player==input$choose_player) %>% 
      select(-indoor,-outdoor,-carpet,-clay,-grass,-hard)})
  
  output$win_graph <- renderPlot({rad1}) 
  
  ############################
  
  ############################
  #### Tournament ######
  output$tournament_name <-renderText({tournament_name <- as.character(tour%>%filter(Tournament==input$tid) %>% 
                                                                         select(Tournament))})
  
  output$tournament_map <- renderPlot({world})
  
  output$table1 <- renderDataTable({table1<-tour%>%filter(Tournament==input$tid)})
  
  output$tournament_name_Year <- renderText({tour%>%filter(Tournament==input$tid & Year==input$yid) %>% 
      print(paste(Tournament,": ",Year))})
  
  output$tour_year <- renderPlot({rad2})
  
  ############################
  
  ############################
  #### Prediction ######
  output$table1 <- renderInfoBox({
    predict_p1 <- player50 %>% filter(player==input$player1 & type=="Wins")
    p1_win <- predict(lm.fit,predict_p1)
    predict_p2 <- player50 %>% filter(player==input$player2 & type=="Wins")
    p2_win <- predict(lm.fit,predict_p2)
    pred.results <- ifelse(p1_win>p2_win,p1,p2)
    pred.results <- unique(pred.results)
    pred.probability <- (p1_win-p2_win)/p2_win +0.5
    #pred.results
    infoBox('#Probability',round(pred.probability*100,2),
            icon = icon('trophy'), color = 'olive', fill = T)
  })
  
  output$table2 <- renderInfoBox({
    predict_p1 <- player50 %>% filter(player==input$player1 & type=="Wins")
    p1_win <- predict(lm.fit,predict_p1)
    predict_p2 <- player50 %>% filter(player==input$player2 & type=="Wins")
    p2_win <- predict(lm.fit,predict_p2)
    pred.results <- ifelse(p1_win>p2_win,p1,p2)
    pred.results <- unique(pred.results)
    #pred.probability <- (p1_win-p2_win)/p2_win +0.5
    pred.results
    infoBox('#Winner',pred.results,
            icon = icon('racquet'), color = 'olive', fill = T)
  })
  
  ############################
  
  ############################
  #### Acknowledgement ######
  output$mao <- renderImage({
    readPNG("mao.png",
         width=250,
         height=250)
  })
  
  
}

shinyApp(ui=ui,server=server)













