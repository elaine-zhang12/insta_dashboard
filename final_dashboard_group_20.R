library(foreign)
library(ggplot2)
library(shiny)
library(tidyverse)
library(skimr)

# Data Cleaning
# September data
# Import data
insta_sept <- read.csv("influencers_sept2022_instagram.csv")

# Select variables of interest and rename them as necessary
insta_sept_clean <- insta_sept %>%
  select(Category_1, Subscribers, Authentic.engagement.) 

insta_sept_clean <- insta_sept_clean %>%
  dplyr::rename(c(category=Category_1, followers=Subscribers, authentic_engagement=Authentic.engagement.))

#Move unit to header for followers and authentic engagement
insta_sept_clean <- insta_sept_clean %>%
  mutate(followers = str_replace_all(followers, c("M" = " M", "K" = " K"))) %>%
  separate(followers, c('followers', 'unit_followers'), sep=' ') %>%
  mutate(authentic_engagement = str_replace_all(authentic_engagement, c("M" = " M", "K" = " K"))) %>%
  separate(authentic_engagement, c('authentic_engagement', 'unit_auth'), sep=' ')

# Make followers and authentic_engagement numeric
insta_sept_clean$followers <- as.numeric(insta_sept_clean$followers)
insta_sept_clean$authentic_engagement <- as.numeric(insta_sept_clean$authentic_engagement)

# Change units to all be in millions
insta_sept_clean <- insta_sept_clean %>%
  mutate(followers = ifelse(unit_followers == 'K', round(followers/1000, 3), followers)) %>%
  mutate(unit_followers = ifelse(unit_followers == 'K', 'M', unit_followers)) %>%
  mutate(authentic_engagement = ifelse(unit_auth == 'K', round(authentic_engagement/1000, 3), authentic_engagement)) %>%
  mutate(unit_auth = ifelse(unit_auth == 'K', 'M', unit_auth))

# Rename variable columns to indicate in millions, and drop unit columns
insta_sept_clean <- insta_sept_clean %>%
  select(-("unit_followers"), -("unit_auth")) %>%
  rename(followers_mil = followers, authentic_engagement_mil = authentic_engagement)

# Organize the categories
insta_sept_clean <- insta_sept_clean %>%
  mutate(category = ifelse(category == 'Racing Sports', 'Sports', category)) %>%
  mutate(category = ifelse(category == 'Sports with a ball', 'Sports', category)) %>%
  mutate(category = ifelse(category == 'Water sports', 'Sports', category)) %>%
  mutate(category = ifelse(category == 'Winter sports', 'Sports', category)) %>%
  mutate(category = ifelse(category == 'Adult content', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Gaming', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Cinema & Actors/actresses', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Humor & Fun & Happiness', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Modeling', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Music', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Shows', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Kids & Toys', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Beauty', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Clothing & Outfits', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Accessories & Jewellery', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Family', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Fashion', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Fitness & Gym', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Food & Cooking', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Lifestyle', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Luxury', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Travel', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Art/Artists', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Business & Careers', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Finance & Economics', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Literature & Journalism', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Education', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Science', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Photography', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Cars & Motorbikes', 'Technology', category)) %>%
  mutate(category = ifelse(category == 'Computers & Gadgets', 'Technology', category)) %>%
  mutate(category = ifelse(category == 'Machinery & Technologies', 'Technology', category)) %>%
  mutate(category = ifelse(category == 'Nature & landscapes', 'Other', category)) %>%
  mutate(category = ifelse(category == 'Animals', 'Other', category)) %>%
  mutate(category = ifelse(category == '', 'Other', category))

# Drop NA values
insta_sept_clean <- insta_sept_clean %>%
  drop_na()

# December dataset
# Import data 
insta_dec <- read.csv("influencers_dec2022_instagram.csv")

# Select variables of interest and rename them as necessary
insta_dec_clean <- insta_dec %>%
  select(Category_1, followers, Eng...Auth..) %>%
  rename(category = Category_1, authentic_engagement = Eng...Auth..)

#Move unit to header for followers and authentic engagement
insta_dec_clean <- insta_dec_clean %>%
  mutate(followers = str_replace_all(followers, c("M" = " M", "K" = " K"))) %>%
  separate(followers, c('followers', 'unit_followers'), sep=' ') %>%
  mutate(authentic_engagement = str_replace_all(authentic_engagement, c("M" = " M", "K" = " K"))) %>%
  separate(authentic_engagement, c('authentic_engagement', 'unit_auth'), sep=' ')

# Make followers and authentic_engagement numeric
insta_dec_clean$followers <- as.numeric(insta_dec_clean$followers)
insta_dec_clean$authentic_engagement <- as.numeric(insta_dec_clean$authentic_engagement)

# Change units to all be in millions
insta_dec_clean <- insta_dec_clean %>%
  mutate(followers = ifelse(unit_followers == 'K', round(followers/1000, 3), followers)) %>%
  mutate(unit_followers = ifelse(unit_followers == 'K', 'M', unit_followers)) %>%
  mutate(authentic_engagement = ifelse(unit_auth == 'K', round(authentic_engagement/1000, 3), authentic_engagement)) %>%
  mutate(unit_auth = ifelse(unit_auth == 'K', 'M', unit_auth))

# Rename variable columns to indicate in millions, and drop unit columns
insta_dec_clean <- insta_dec_clean %>%
  select(-("unit_followers"), -("unit_auth")) %>%
  rename(followers_mil = followers, authentic_engagement_mil = authentic_engagement)

# Organize the categories
insta_dec_clean <- insta_dec_clean %>%
  mutate(category = ifelse(category == 'Racing Sports', 'Sports', category)) %>%
  mutate(category = ifelse(category == 'Sports with a ball', 'Sports', category)) %>%
  mutate(category = ifelse(category == 'Water sports', 'Sports', category)) %>%
  mutate(category = ifelse(category == 'Winter sports', 'Sports', category)) %>%
  mutate(category = ifelse(category == 'Adult content', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Gaming', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Cinema & Actors/actresses', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Humor & Fun & Happiness', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Modeling', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Music', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Shows', 'Entertainment', category)) %>%
  mutate(category = ifelse(category == 'Beauty', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Clothing & Outfits', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Family', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Fashion', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Fitness & Gym', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Food & Cooking', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Lifestyle', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Luxury', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Travel', 'Lifestyle/Health', category)) %>%
  mutate(category = ifelse(category == 'Art/Artists', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Business & Careers', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Finance & Economics', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Literature & Journalism', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Education', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Science', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Photography', 'Professionals', category)) %>%
  mutate(category = ifelse(category == 'Cars & Motorbikes', 'Technology', category)) %>%
  mutate(category = ifelse(category == 'Computers & Gadgets', 'Technology', category)) %>%
  mutate(category = ifelse(category == 'Machinery & Technologies', 'Technology', category)) %>%
  mutate(category = ifelse(category == 'Nature & landscapes', 'Other', category)) %>%
  mutate(category = ifelse(category == 'Animals', 'Other', category)) %>%
  mutate(category = ifelse(category == '', 'Other', category))

# Merge the two data sets together
insta_sept_clean <- insta_sept_clean %>%
  add_column(month = "September")

insta_dec_clean <- insta_dec_clean %>%
  add_column(month = "December")

insta_clean <- rbind(insta_sept_clean, insta_dec_clean)
insta_clean$month = as.factor(insta_clean$month)


#Overall Shiny dashboard
ui <- navbarPage(title = "Instagram Influencer Analysis",
                 theme = bslib::bs_theme(bootswatch = "flatly"),
                 tabPanel(title = "Background",
                          fluidPage(
                            h1("Instagram Influencer Analysis", align="center"),
                            h5("Cheryl Bai, Janice Guo, Elaine Zhang", align="center"),
                            br(),
                            h2("Executive Summary"),
                            p("Recently, we have noticed that there has been a decrease in engagement on Instagram among our peers. This includes a decrease in the number of likes or comments on our posts. After seeing this trend occur to our accounts as average social media users, we were curious to know if top influencers were experiencing the same decrease in engagement and what this event could mean for the future of social media. As a result, we examined samples of influencer data for user engagement measurements based on account categories over a recent period of three months to see what we could discover."),
                            p("This report highlights the relationship between different categories of content on social media as well as the amount of followers and the rate of engagement each category produces. Specifically, the data presented in this report was extracted from Instagram with information about different influencers across a variety of topics. The findings produced in this report are highly beneficial for social media influencer managers and for individuals who are looking to expand their online influence. Overall, there is a trend that entertainment-centered social media accounts tend to have more followers. On the other hand, accounts based on other types of content outside of the five categories we formed generate a higher engagement rate from their followers. Thus, stakeholders should choose wisely what type of content they want to release depending on whether they value follower growth or engagement rates more."),
                            br(),
                            h2("Data Set"),
                            p("The data used in this report comes from ",
                              a("Social Media Influencers in 2022",
                                href = "https://www.kaggle.com/datasets/ramjasmaurya/top-1000-social-media-channels"),
                              ", a data set found online on Kaggle. For this project we are specifically using the Instagram December 2022 and Instagram September 2022 files. We accessed this data on March 15, 2023."),
                            p("We further organized the 32 categories given in raw data sets into 6 overarching categories (Sports, Entertainment, Lifestyle/Health, Professionals, Technology, and Other). The new categories are organized as follows:"),
                            tags$ul(
                              tags$li("Sports: Racing Sports, Sports with a ball, Water sports, Winter sports"),
                              tags$li("Entertainment: Adult content, Cinema & Actors/actresses, Gaming, Humor & Fun & Happiness, Modeling, Music, Shows, Kids & Toys"),
                              tags$li("Lifestyle/Health: Beauty, Clothing & Outfits, Family, Fashion, Fitness & Gym, Food & Cooking, Lifestyle, Luxury, Travel, Accessories & Jewellery"),
                              tags$li("Professionals: Art/Artists, Business & Careers, Finance & Economics, Literature & Journalism, Education, Photography"),
                              tags$li("Technology: Cars & Motorbikes, Computers & Gadgets, Machinery & Technologies, Science"),
                              tags$li("Other: (no label), Animals, Nature & Landscapes")
                            ),
                            p("Some placements to note are Modeling, Education, Animals, and Nature & Landscapes. We considered Modeling as both entertainment and a professional job, but saw modeling on Instagram as a greater means for user entertainment. Education was placed under Professionals because we saw learning and being a student content creator as a “job.” Both Animal and Nature & Landscapes didn’t fit into any of the mentioned categories, so they were grouped under Other. Finally, a few of the Instagram accounts had no value for the Category variable, so they were also placed under the Other category."),
                            p("Additionally in the Instagram September 2022 data set, there were four authentic engagement values that were NA in the table after cleaning due to their original values of 0. Of these four records with no engagement, two had no label, one was categorized as Sports, and one was categorized as Lifestyle/Health. As a result, we concluded it was fairly safe to remove these four records."),
                            br(),
                            h2("Background and Questions"),
                            p("We were inspired to dive deeper into the topic of social media engagement because of our current interactions on social media. We all use social media less than before and have noticed fewer interactions with our peers over social media. As such, we wanted to examine general trends in user engagement over a three month period. With this in mind, we are curious to see the topics that people tend to follow the most and have the greatest popularity on Instagram. We wanted to find general trends in what could affect user engagement, which may be useful for anyone looking into starting an Instagram account. Based on the given variables of the data set we chose, we decided that we can look at user engagement based on the category or topic of the account. For this project, there were two types of user engagement that we analyzed. The number of followers an Instagram account has is an indicator of user engagement because users follow accounts that they wish to continuously engage with in the future. The second variable we chose was authentic engagement, the engagement in the Instagram account by those following it, because that tells us how many of their followers are actively engaging in their content."),
                            p("We also see time as an important factor. Trends come and go and social media is fast paced. We want to see if over a period of time, there are any changes in user engagement in regards to the category. To do this, we gathered data from two different data sets for September and December in the year 2022 and compared the differences between the two in order to see if any changes could be seen over a quarter of a year. Below are the two questions we will be using to guide our analysis."),
                            tags$ol(
                              tags$li("What effect does account category have on follower count and engagement rates on Instagram at the start and end of a quarter?"),
                              tags$li("What is the relationship between follower count and engagement rates by account category?")
                            )
                          )
                 ),
                 tabPanel(title = "Data Summary", 
                          fluidPage(titlePanel("Data Descriptives"), 
                                    fluidRow(
                                      column(6,
                                             
                                             h4("Major variables:"),
                                             tags$ul(
                                               tags$li("Followers (Subscribers)"),
                                               tags$ul(
                                                 tags$li("This variable measures the number of people following an Instagram account"),
                                                 tags$li("This is a continuous variable and values are in millions")
                                               ),
                                               tags$li("Category (category)"),
                                               tags$ul(
                                                 tags$li("The data set organized the Instagram accounts under 32 different categories which defines the topic of the account"),
                                                 tags$li("This is a discrete variable")
                                               ),
                                               tags$li("Authentic engagement (Eng…Auth..)"),
                                               tags$ul(
                                                 tags$li("This is measured by the number of followers that interact with the Instagram account"),
                                                 tags$li("This is a continuous variable and values are in millions")
                                               )
                                             )),
                                      column(6, verbatimTextOutput("data"))
                                      
                                    ), br(),
                                    h2("Comparison of the Major Variables"), br(),
                                    fluidRow(sidebarLayout(
                                      sidebarPanel(
                                        radioButtons("measure_t", label = "Measurement:",
                                                     choices = c("Followers" = "followers_mil", 
                                                                 "Engagement" = "authentic_engagement_mil"),
                                                     selected = "followers_mil"),
                                        selectInput("month_eda", label = "Month: ",
                                                    choices = c("September", "December"),
                                                    selected = "September")),
                                      mainPanel(
                                        plotOutput("insta_eda_plot")
                                      )
                                    )
                                    )
                          )
                 ),
                 tabPanel(title = "Question 1", 
                          fluidPage(
                            titlePanel("User Engagement"),
                            h5("What effect does account category have on follower count and engagement rates on Instagram at the start and end of a quarter?"), br(),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("measure_type", label = "Measurement:",
                                            choices = c("followers_mil", "authentic_engagement_mil"),
                                            selected = "followers_mil"),
                                radioButtons("month", label = "Month: ",
                                             choices = c("September" = "September",
                                                         "December" = "December",
                                                         "Net Difference" = "Net Difference"),
                                             selected = "September")),
                              mainPanel(
                                plotOutput("insta_plot_1"),
                                
                              )
                              
                            ), br(),
                            p("We see that Entertainment type accounts are the most popular and gain the most amount of followers as time passes. This information could be important for social media influencers or managers that are looking to increase follower count for their Instagram accounts or start new Instagram accounts with the goal of having a high follower count. Entertainment is a great type of account to work with in the long run, as it is the most positive in terms of growth over time. Sports, Professionals, and Technology type accounts are relatively stable and are still considered a safe topic to focus on. Lifestyle/Health and Other type of accounts should be avoided if the main purpose is to grow and reach a wider audience of followers over time. Understanding the trends in follower count among different categories is a helpful tool to utilize when deciding what type of social media influencer to become."),
                            p("We also see that Other type accounts retain the highest percentage of user engagement over time, followed by Lifestyle/Health type accounts. This information could be important for Instagram social media influencers or managers that are looking to increase their user engagement or start a new Instagram account with the goal of having a high engagement to follower ratio. Furthermore, knowing which topics generally result in greater user engagement could help with creating new social media content to achieve that. Other and Lifestyle/Health type of accounts are great to work with for keeping a dedicated following base because these accounts post content on topics that are long-term interests of users and can be applicable to the life of an average person. Despite all this, it should be noted that a conclusion about specifically what topics Other type accounts encompasses cannot be drawn and thus would be difficult to apply to the real world."),
                            p("What is most notable is that while Entertainment saw the biggest increase in follower count, it also saw the least engagement increase compared to the start and end of the quarter. Overall, it is clear that follower increase does not mean engagement increase.")
                          )),
                 tabPanel(title = "Question 2", 
                          fluidPage(
                            titlePanel("Follower Count Versus Engagement"), 
                            h5("What is the relationship between follower count and engagement rates?"), br(),
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput("categories", label = "Categories :",
                                                   choices = c("Entertainment" = "Entertainment",
                                                               "Lifestyle/Health" = "Lifestyle/Health",
                                                               "Other" = "Other",
                                                               "Professionals" = "Professionals",
                                                               "Sports" = "Sports", 
                                                               "Technology" = "Technology"),
                                                   selected = "Entertainment"),
                                p("Select a month to begin: "),
                                actionButton("sept", "September"),
                                actionButton("dec", "December")),
                              mainPanel(
                                plotOutput("insta_plot_2")
                              )
                            ), br(),
                            p("We see that across the months and across all categories, the trend in follower count vs engagement rates does not change. Both graphs look like a reciprocal relationship, meaning that these two variables are actually inversely related to each other. Despite this negative correlation, we cannot conclude that a smaller number of followers causes a higher enagement rate. However, it is possible that this pattern exists because more genuine Instagram followers are present in accounts with lower follower counts. Stakeholders should note that as account popularity increases, it is inevitable that the number of truly engaged followers will not increase equally with unengaged followers. This means that the percentage of followers that are consistently interacting with their content will decrease as popularity increases. Such information should be taken into consideration when developing a marketing strategy for an influencer to maximize their influence."),
                            p("One category to pay special attention to is the Technology category. This category held the weakest relationship between engagement rates and follower count across both months, as sometimes accounts with a medium-sized following had a higher engagement rate than accounts with a smaller or larger following. This may indicate a different trend in Technology type accounts, but could also be due to inherent randomness in a small sample size. Thus, we observe an interesting phenomena in this category, but cannot draw any conclusions with the limited data we have.")
                          )
                 ),
                 tabPanel(title = "Citations and Summary",
                          fluidPage(h2("Summary"),
                                    p("As stakeholders in social media, we had originally thought that the popularity of Instagram was decreasing and that user engagement all around was going down. However, based on the comparison trends between the September and December data sets, user engagement actually increased in all categories. One reason for this, though, is that December is the holiday season, where people go on break and celebrate the holidays. During this period of time, it is possible that more people are using social media and are creating content, which could lead to higher user engagement values."),
                                    p("The key takeaway for influencers is that Instagram is still heavily used and there is still an increase in usage in recent months, making it a worthwhile endeavor to start and build a social media following right now. The categories with the largest increase in user engagement were those in Other (Animals, Nature & Landscape, and accounts with no labels), Sports, and Technology. Those with accounts under these categories could see more user engagement compared to that of other categories. What is surprising, though, is that the Other category had the largest decrease in followers. This could be an indicator that a greater amount of followers does not lead to a greater amount of engagement. Our graph depicting the percent engagement of followers by Instagram account also showcases the narrative that higher follower count does not equal higher user engagement. Perhaps, there are other factors, which we can’t analyze that might be more important when garnering user engagement, such as post time, how often content is released, the type of content, and etc."),
                                    br(),
                                    h2("Citations"),
                                    p("Below are a list of websites used as reference when creating and coding graphs for this shiny website:"),
                                    tags$ul(
                                      tags$li(
                                        a("ggplot2 boxplot",
                                          href = "http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization")),
                                      tags$li(
                                        a("Reordering ggplot bar chart axis",
                                          href = "https://www.rpubs.com/dvdunne/reorder_ggplot_barchart_axis")
                                      ),
                                      tags$li(
                                        a("Boxplot in ggplot2 without x-axis labels",
                                          href = "https://www.tutorialspoint.com/how-to-create-a-boxplot-using-ggplot2-for-single-variable-without-x-axis-labels-in-r")
                                      ),
                                      tags$li(
                                        a("Tabs",
                                          href = "https://bookdown.org/yihui/rmarkdown-cookbook/html-tabs.html")
                                      ),
                                      tags$li(
                                        a("Shiny HTML tags",
                                          href = "https://shiny.rstudio.com/articles/tag-glossary.html")
                                      )
                                    ),
                                    br(),
                                    h2("Acknowledgements"),
                                    p("We would like to acknowledge Professor Natalie Kupperman and Aubrey Winger for helping guide us throughout the creation of this dashboard. Thank you to all of our peers that provided us with constructive feedback which helped us refine our final product.")
                                    
                          ))
)

server <- function(input, output){
  
  output$insta_plot_1 <- renderPlot({
    if(input$measure_type == "followers_mil") {
      if(input$month == "September") {
        insta_sept_clean %>% 
          group_by(category) %>%
          summarize(followers = sum(followers_mil)) %>%
          ggplot(aes(x = reorder(category, -followers), y = followers)) + 
          geom_col(fill = c("#FACBCD", "#D3B8BA", "#E29190", "#E2DE84", "#AED9AB", "#7AD3D1")) +
          ggtitle("Followers in Millions By Category in September 2022") +
          xlab("Category") +
          ylab("Number of Followers") +
          theme(plot.title=element_text(hjust=0.5))
      } else if(input$month == "December") {
        insta_dec_clean %>% 
          group_by(category) %>%
          summarize(followers = sum(followers_mil)) %>%
          ggplot(aes(x = reorder(category, -followers), y = followers)) + 
          geom_col(fill = c("#FACBCD", "#D3B8BA", "#E29190", "#E2DE84", "#AED9AB", "#7AD3D1")) +
          ggtitle("Followers in Millions By Category in December 2022") +
          xlab("Category") +
          ylab("Number of Followers") +
          theme(plot.title=element_text(hjust=0.5))
      } else {
        insta_dec_follow <- insta_dec_clean %>%
          group_by(category) %>%
          summarize(followers = sum(followers_mil))
        insta_sept_follow <- insta_sept_clean %>%
          group_by(category) %>%
          summarize(followers = sum(followers_mil))
        insta_difference <- insta_dec_follow %>%
          select(category)
        insta_difference$follow_difference = insta_dec_follow$followers - insta_sept_follow$followers
        ggplot(insta_difference, aes(x = reorder(category, -follow_difference), y = follow_difference)) +
          geom_col(fill = c("#FACBCD", "#D3B8BA", "#E29190", "#E2DE84", "#AED9AB", "#7AD3D1")) +
          ggtitle("Follower Difference in Millions Between December and September") +
          xlab("Category") +
          ylab("Follower Difference (M)") +
          theme(plot.title=element_text(hjust=0.5))
      }
    } else {
      if(input$month == "September") {
        insta_sept_clean %>%
          group_by(category) %>%
          summarize(followers = sum(followers_mil), engagement = sum(authentic_engagement_mil)) %>%
          mutate(engagement = engagement * 100 / followers) %>%
          ggplot(aes(x = reorder(category, -engagement), y = engagement)) + 
          geom_col(fill = c("#FACBCD", "#D3B8BA", "#E29190", "#E2DE84", "#AED9AB", "#7AD3D1")) +
          ggtitle("Authentic Engagement Percentages By Category in September 2022") +
          xlab("Category") +
          ylab("Authentic Engagement Percentage") +
          theme(plot.title=element_text(hjust=0.5))
      } else if(input$month == "December") {
        insta_dec_clean %>%
          group_by(category) %>%
          summarize(followers = sum(followers_mil), engagement = sum(authentic_engagement_mil)) %>%
          mutate(engagement = engagement * 100 / followers) %>%
          ggplot(aes(x = reorder(category, -engagement), y = engagement)) + 
          geom_col(fill = c("#FACBCD", "#D3B8BA", "#E29190", "#E2DE84", "#AED9AB", "#7AD3D1")) +
          ggtitle("Authentic Engagement Percentages By Category in December 2022") +
          xlab("Category") +
          ylab("Authentic Engagement Percentage") +
          theme(plot.title=element_text(hjust=0.5))
      } else {
        insta_dec_engagement <- insta_dec_clean %>%
          group_by(category) %>%
          summarize(followers = sum(followers_mil), engagement = sum(authentic_engagement_mil)) %>%
          mutate(engagement = engagement * 100 / followers)
        
        insta_sept_engagement <- insta_sept_clean %>%
          group_by(category) %>%
          summarize(followers = sum(followers_mil), engagement = sum(authentic_engagement_mil)) %>%
          mutate(engagement = engagement * 100 / followers)
        
        insta_difference <- insta_dec_engagement %>%
          select(category)
        
        insta_difference$engage_difference= insta_dec_engagement$engagement - insta_sept_engagement$engagement
        
        ggplot(insta_difference, aes(x = reorder(category, -engage_difference), y = engage_difference)) +
          geom_col(fill = c("#FACBCD", "#D3B8BA", "#E29190", "#E2DE84", "#AED9AB", "#7AD3D1")) +
          ggtitle("Engagement Percentage Difference Between December and September") +
          xlab("Category") +
          ylab("Engagement Percentage Difference") +
          theme(plot.title=element_text(hjust=0.5))
      }
    }
  })
  
  observeEvent(input$sept, output$insta_plot_2 <- renderPlot({
    insta_sept_clean %>%
      filter(category %in% input$categories) %>%
      mutate(authentic_engagement_mil = authentic_engagement_mil * 100 / followers_mil) %>%
      ggplot(aes(x = followers_mil, y = authentic_engagement_mil, color = category)) +
      geom_point(alpha = 0.5, size = 5) +
      scale_color_manual(values = c("#FACBCD", "#D3B8BA", "#E29190", "#E2DE84", "#AED9AB", "#7AD3D1")) +
      ggtitle("September 2022 Engagement Percentage by Followers in Millions") +
      xlab("Number of Followers (M)") +
      ylab("Engagement Percentage") +
      theme(plot.title=element_text(hjust=0.5))
  }))
  observeEvent(input$dec, output$insta_plot_2 <- renderPlot({
    insta_dec_clean %>%
      filter(category %in% input$categories) %>%
      mutate(authentic_engagement_mil = authentic_engagement_mil * 100 / followers_mil) %>%
      ggplot(aes(x = followers_mil, y = authentic_engagement_mil, color = category)) +
      geom_point(alpha = 0.5, size = 5) +
      scale_color_manual(values = c("#FACBCD", "#D3B8BA", "#E29190", "#E2DE84", "#AED9AB", "#7AD3D1")) +
      ggtitle("December 2022 Engagement Percentage by Followers in Millions") +
      xlab("Number of Followers (M)") +
      ylab("Engagement Percentage") +
      theme(plot.title=element_text(hjust=0.5))
  }))
  
  output$insta_eda_plot <- renderPlot({
    if(input$measure_t == "followers_mil") {
      if(input$month_eda == "September") {
        ggplot(insta_sept_clean, aes(x = followers_mil, y = category)) +
          geom_boxplot(fill = c("#FACBCD", "#D3B8BA", "#E29190", "#E2DE84", "#AED9AB", "#7AD3D1")) + 
          ggtitle('Boxplot of September 2022 Instagram Followers in Millions by Category') +
          xlab("Number of Followers (M)") +
          ylab("Category") +
          theme(plot.title=element_text(hjust=0.5), legend.position = "none")
      } else {
        ggplot(insta_dec_clean, aes(x = followers_mil, y = category)) +
          geom_boxplot(fill = c("#FACBCD", "#D3B8BA", "#E29190", "#E2DE84", "#AED9AB", "#7AD3D1")) + 
          ggtitle('Boxplot of December 2022 Instagram Followers in Millions by Category') +
          xlab("Number of Followers (M)") +
          ylab("Category") +
          theme(plot.title=element_text(hjust=0.5), legend.position = "none")
      }
    } else {
      if(input$month_eda == "September") {
        ggplot(insta_sept_clean, aes(x = authentic_engagement_mil, y = category)) +
          geom_boxplot(fill = c("#FACBCD", "#D3B8BA", "#E29190", "#E2DE84", "#AED9AB", "#7AD3D1")) + 
          ggtitle('Boxplot of September 2022 Instagram Engagement in Millions by Category') +
          xlab("Number of Followers Engaging (M)") +
          ylab("Category") +
          theme(plot.title=element_text(hjust=0.5), legend.position = "none")
      } else {
        ggplot(insta_dec_clean, aes(x = authentic_engagement_mil, y = category)) +
          geom_boxplot(fill = c("#FACBCD", "#D3B8BA", "#E29190", "#E2DE84", "#AED9AB", "#7AD3D1")) + 
          ggtitle('Boxplot of December 2022 Instagram Engagement in Millions by Category') +
          xlab("Number of Followers Engaging (M)") +
          ylab("Category") +
          theme(plot.title=element_text(hjust=0.5), legend.position = "none")
      }
    }
  })
  
  output$data <- renderPrint({
    skim(insta_clean)
  })
}

shinyApp(ui = ui, server = server, options = list(height = 600))