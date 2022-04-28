#Shiny App

#load packages
library(shiny)
library(tidyverse)
library(broom)
library(ggplot2)
library(dplyr)
library(knitr)
library(scales)
library(ggthemes)
library(ggrepel)


# read data
survey <- readRDS("data/2020.rds")

#plots options
choices <-  c("Donald Trump supporters' basic profiles",
              "Donald Trump supporters' profiles by states",
              "Donald Trump supporting rate by States in 2016 and 2020",
              "Donald Trump supporting rate vs. usage social media",
              "Donald Trump supporting rate vs. worried ban of twitter",
              "Donald Trump supporting rate grouped by income, race and social media",
              "Donald Trump supporters' age distribution grouped by usage of social media",
              "Donald Trump supporters' age distribution grouped by worried of twitter ban",
              "Donald Trump supporting rate grouped by vote group in 2016 and social media",
              "Logistic model estimates"
)

#UI of shiny
ui <- navbarPage(
  h5("Shiny App"),
  
  tabPanel(
    "Data Visualisation Shiny app for US election",
    sidebarLayout(
      
      sidebarPanel(
        
        radioButtons("x",  "What would you like to explore?", 
                    choices =choices,
                    selected = "Donald Trump supporting rate vs. usage social media")),
      mainPanel(
        
        tabsetPanel(
          
          tabPanel("Data Visualisation", plotOutput("plot"))
          
        )
      )
      
    )
  )
  
)
#server side
server <- function(input, output) {
  
  xvar <- reactive({
    input$x
  })
   
  
  output$plot <- renderPlot({
    
    #options of plots
  if ( xvar() == choices[1]) {
    
    d1 <- survey %>% group_by(agegroup,gender, race ) %>%
      summarise(rate = mean(Trump2020))
    d1$group <- paste0(d1$agegroup," ", d1$gender, " ", d1$race)
    d1 <- d1 %>% arrange(desc(-rate))
    d1$group <- factor(d1$group, levels = unique(d1$group))
    ggplot(d1, aes(group,rate)) +
      geom_col(fill = "indianred") + coord_flip() +
      theme_economist_white() +
      labs(x = "", title = "Donald Trump supporters' profiles",
           y = "Voting rate") +
      scale_y_continuous(label = percent)
    
  } else if(xvar() == choices[2]) {
d1 <- survey %>% group_by(stateicp) %>%
  summarise(rate = mean(Trump2020))  %>% arrange(desc(-rate))
d1$stateicp <- factor(d1$stateicp, levels =d1$stateicp)
ggplot(d1, aes(x=stateicp, y=rate,
              label=round(rate*100,1))) + 
  geom_point(stat='identity',  size= 9, color = "red")  +
  geom_segment(aes(y = 0, 
                   x = stateicp, 
                   yend =rate, 
                   xend = stateicp)) +
  geom_text(color="white", size=3) +
  labs(title="Donald Trump supporters' profiles by States", 
       x= "",
       y = "Voting rate") +
  coord_flip() +
    theme_economist()+
  scale_y_continuous(label = percent)
} else if(xvar() == choices[3] ) {
d1 <- survey %>% group_by(stateicp) %>%
  summarise(rate = mean(Trump2020),
            rate2016 = mean(Trump2016),
            news_sources_facebook = mean(news_sources_facebook == "Yes"),
            twitter_ban = mean(twitter_ban == "worried")
            )
a1 <- d1
ggplot(d1, aes(rate2016,rate)) + geom_point(color = "darkorange", size = 2) +
  geom_label_repel(aes(label = stateicp), size = 3, max.overlaps  = 60) +
  geom_smooth(method = "lm", fill = NA, color = "blue", alpha = 0.1, linetype = 2) +
  labs(title="Donald Trump supporting rate by States in 2016 and 2020", 
       x= "Voting rate in 2016",
       y = "Voting rate in 2020") +
    theme_economist()+
  scale_y_continuous(label = percent) +
    scale_x_continuous(label = percent)


} else if(xvar() == choices[4]) {
  d1 <- survey %>% group_by(stateicp) %>%
    summarise(rate = mean(Trump2020),
              rate2016 = mean(Trump2016),
              news_sources_facebook = mean(news_sources_facebook == "Yes"),
              twitter_ban = mean(twitter_ban == "worried")
    )
ggplot(d1, aes(news_sources_facebook,rate)) + geom_point(color = "darkorange", size = 2) +
  geom_label_repel(aes(label = stateicp), size = 3, max.overlaps  = 60) +
  geom_smooth(method = "lm", fill = NA, color = "blue", alpha = 0.1, linetype = 2) +
  labs(title="Donald Trump supporting rate vs. usage of twitter, facebook", 
       x= "Percentage of usage social media like twitter, facebook",
       y = "Voting rate in 2020") +
    theme_economist()+
  scale_y_continuous(label = percent) +
    scale_x_continuous(label = percent)

} else  if(xvar() == choices[5]) {
  d1 <- survey %>% group_by(stateicp) %>%
    summarise(rate = mean(Trump2020),
              rate2016 = mean(Trump2016),
              news_sources_facebook = mean(news_sources_facebook == "Yes"),
              twitter_ban = mean(twitter_ban == "worried")
    )
ggplot(d1, aes(twitter_ban,rate)) + geom_point(color = "darkorange", size = 2) +
  geom_label_repel(aes(label = stateicp), size = 3, max.overlaps  = 60) +
  geom_smooth(method = "lm", fill = NA, color = "blue", alpha = 0.1, linetype = 2) +
  labs(title="Donald Trump supporting rate vs. worried ban of twitter", 
       x= "Percentage of worried ban of twitter",
       y = "Voting rate in 2020") +
    theme_economist()+
  scale_y_continuous(label = percent) +
    scale_x_continuous(label = percent)
} else if(xvar() == choices[6]) {
    
    
d1 <- survey %>% 
  group_by(incomegroup, race, news_sources_facebook) %>% 
  summarise(rate = mean(Trump2020))

d1 %>% ggplot(aes(race, rate, fill =  news_sources_facebook)) +
  geom_col(position = "dodge") +
  facet_wrap(~incomegroup ) +
    labs(title="Donald Trump supporting rate", 
         subtitle = "grouped by income, race and social media",
       x= "Race",
       y = "Voting rate in 2020") +
    theme_economist()+
  scale_y_continuous(label = percent) 
} else if(xvar() == choices[7]) {
  survey2  = survey
    survey2$trumpvote = ifelse(survey2$Trump2020 ==1,"Vote Trump","Not Vote Trump")
    survey2 %>% ggplot(aes(age, color =  trumpvote)) +
      geom_density() +
      facet_wrap(~news_sources_facebook) +
      labs(title="Donald Trump supporters' age distribution", 
           subtitle = "grouped by usage of social media twitter, facebook",
           x= "Age", color  = "") +
      theme_economist() 
    
} else if (xvar() == choices[8]) {
  survey2  = survey
  survey2$trumpvote = ifelse(survey2$Trump2020 ==1,"Vote Trump","Not Vote Trump")
survey2 %>% ggplot(aes(age, color =  trumpvote)) +
  geom_density() +
  facet_wrap(~twitter_ban) +
    labs(title="Donald Trump supporters' age distribution", 
         subtitle = "grouped by worried of twitter ban",
       x= "Age", color = "") +
    theme_economist()
} else if(xvar() == choices[9]) {
  
  d1 <- survey %>% 
    group_by(Trump2016,  news_sources_facebook) %>% 
    summarise(rate = mean(Trump2020))
  d1 %>% ggplot(aes(factor(Trump2016), fill = news_sources_facebook, rate)) +
    geom_col(position = "dodge")+
    labs(title="Donald Trump supporting rate", 
         subtitle = "grouped by vote group in 2016 and social media",
         x= "vote group in 2016",
         y = "Voting rate in 2020",
         fill = "Usage twitter,facebook") +
    theme_economist()+
    scale_y_continuous(label = percent) 
}    else {
  survey2 = survey
  survey2$age = NULL
  survey2$stateicp = NULL
  fit <- glm(Trump2020 ~ ., 
             data = survey2, 
             family = binomial(link = "logit"))
  coefs <- broom::tidy(fit, conf.int = T)
  coefs2 <- coefs %>%arrange(desc(-estimate))
  coefs2$term <- factor(coefs2$term , levels = coefs2$term )
  coefs2$color = factor(ifelse(coefs2$estimate > 0, "positive","negative"))
  ggplot(coefs2, aes(estimate, term)) +
    geom_point(aes(color = color)) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = color)) +
    labs(title = "Logistic model estimates in log odds",
         x = "Coefficient",
         y = "",
         color = "") + 
    theme_light() +
    scale_color_manual(values = c("red", "blue"))
}

    
  })
  
}

#connect UI and Server

shinyApp(ui, server)