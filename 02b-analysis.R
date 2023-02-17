##########################################################
#
#   Sycophants in 280 Characters - Replication Code
#   
#   02b - Analysis
#   
#   Includes Replication Code for Figure 2, Figure 3, and Figure 4.


#Load Libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(readit)
library(ggpubr)
library(r2symbols)


#Set Working Directory (only works in R Studio - otherwise replace manually)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


###
#Loading Tweets scored by model 
#Note: Prepared dataset includes information about political elites (party, position in party)
load("tweets_scored.RData")

#Pulling out political information for each individual - to use later
info <- tweets_scored[!duplicated(tweets_scored$author_id), ]
info <- info[,c("author_id", "party", "position", "confirmed_advisor")]



#Calculating individual averages for their tweets on the democratic-authoritarian dimension
ind_means <- dplyr::group_by(tweets_scored, author_id) %>% summarise(dem_auth_mean = mean(dem_auth_press))

#make a count of number of tweets per user
ind_count <- dplyr::group_by(tweets_scored, author_id) %>% summarise(tweet_count = n())


#Table in Appendix B.3 - Summary stats of tweets per user
summary(ind_count$tweet_count)

#merging this two directly - will check if any outliers with very few tweets
ind_count_compare <- dplyr::left_join(ind_means, ind_count, by = "author_id")

#Merging back in political information about individuals
ind_scores <- dplyr::left_join(ind_count_compare, info, by = "author_id")


##############
#Appendix B.3. Figure - People who have less than 10 tweets
lessthan10 <- dplyr::filter(ind_count_compare, tweet_count < 10)


appendixB3 <- ggplot(data = lessthan10, aes(dem_auth_mean)) +
  geom_histogram(bins = 50) +
  geom_density() +
  scale_x_continuous(breaks =seq(0.1,0.9 , by = 0.1)) + 
  labs(x = "Democratic-Authoritarian Sentiment",
       y = "Count",
       title = "Distribution of Democratic-Authoritarian Scores",
       subtitle = "Individuals with Few Tweets (10 or less)") +
  theme(plot.title = element_text(hjust = 0.4), plot.subtitle = element_text(hjust = 0.4))


appendixB3



#Saving
ggsave("appendixB3.png", plot = appendixB3, width = 6, height = 4, dpi = "retina")








####### Figure 2

##Load V-DEM Party Dataset for Turkey
load("./data/vdem_parties.RData")




#####
### Getting Chapel Hill Data
chapel <- readit("./data/CHES2019V3.dta")

#just Turkey
chapel_turkey <- dplyr::filter(chapel, country == 34)

#fewer columns
chapel_turkparties <- chapel_turkey[ , c("party", "galtan", "galtan_sd", "galtan_salience", "galtan_dissent", "galtan_blur")]

#Putting on same scale as Vdem
chapel_turkparties$galtan <- chapel_turkparties$galtan / 10

#Using Turkish Letters for IYI party
chapel_turkparties$party[chapel_turkparties$party == "IYI"] <- "İYİ"



#getting party means
party_means <- dplyr::group_by(tweets_scored, party) %>% summarise(dem_auth_mean = mean(dem_auth_press))

#only those parties that are in V-Dem (also these are the main parties)
party_means_only <- dplyr::filter(party_means, party %in% c("AKP", "İYİ", "CHP", "MHP", "HDP"))



#putting together
comparing_scores <- dplyr::left_join(party_means_only, vdem_parties, by="party")
comparing_scores <- dplyr::left_join(comparing_scores, chapel_turkparties, by="party")




###Figure 2 - Direct correlation of mine and expert measures

#using chapel hill
colScale3 <- c("gray30","orange3","gold2","green4","light blue","dodgerblue4","red3","darkorchid4")
party_order <- c("HDP", "CHP", "DEVA", "İYİ", "GELECEK", "AKP", "old AKP", "MHP")
po_r <- rev(party_order)


plot1a <- ggplot(comparing_scores, aes(x = galtan, y = dem_auth_mean, fill=party, label = party)) +
  geom_label(show.legend = FALSE) +
  geom_smooth(inherit.aes = FALSE, data=comparing_scores, aes(x = galtan, y = dem_auth_mean), 
              method = 'lm', formula = y~x, show.legend = FALSE, se=FALSE, color = "black") +
  scale_fill_manual(values = colScale3, limits=c(po_r)) +
  labs(y = "Democratic-Authoritarian Score on Twitter",
       x = "Chapel Hill - Postmateralist vs Authoritarian",
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  stat_cor(inherit.aes = FALSE, data=comparing_scores, aes(x = galtan, y = dem_auth_mean),
           method = "pearson" , label.x = 0.125, label.y = 0.55)  #


plot1a




#using V-Dem
plot1b <- ggplot(comparing_scores, aes(x = illiberalism, y = dem_auth_mean, fill = party, label = party)) +
  geom_label(show.legend = FALSE) +
  geom_smooth(inherit.aes = FALSE, data=comparing_scores, aes(x = illiberalism, y = dem_auth_mean), 
              method = 'lm', formula = y~x, show.legend = FALSE, se=FALSE, color = "black") +
  scale_fill_manual(values = colScale3, limits=c(po_r)) +
  labs(y = "",
       x = "V-Dem - Party Illiberalism",
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_cor(inherit.aes = FALSE, data=comparing_scores, aes(x = illiberalism, y = dem_auth_mean),
           method = "pearson" , label.x = 0.125, label.y = 0.55)  #



plot1b





#Combining the two
figure2 <- ggarrange(plot1a, plot1b)

figure2 <- annotate_figure(figure2,
                           top = text_grob("Comparing Sentiment Scored by Model with Expert Survey",
                                           just = "centre", face = "bold", size = 12))


figure2




#saving
ggsave("figure2.png", plot = figure2, width = 8, height = 6, dpi = "retina")









####### Figure 3
######################

#Getting all the main parties
parties <- dplyr::filter(ind_scores, party %in% c("HDP", "CHP", "DEVA", "İYİ", "GELECEK", "AKP", "old AKP", "MHP"))

#storing regression information
R2 <- summary(lm(dem_auth_mean ~ party, data = parties))

#extracting R score
rscore <- format(R2$r.squared, digits = 3)

#The regression formula with R score
dem_form <- paste("Dem-Auth Sentiment = α + β*Party, R² =" , rscore)

#Getting separate variable for Erdogan so can label
erdogan <- dplyr::filter(ind_scores, author_id == 68034431)


#For color and ordering the variables by party from most authoritarian to least
colScale2 <- c("black","orange3","gold2","green4","light blue","dodgerblue4","red3","darkorchid4")
party_order <- c("HDP", "CHP", "DEVA", "İYİ", "GELECEK", "AKP", "old AKP", "MHP")
po_r <- rev(party_order)




#Plotting Figure 3
figure3 <- ggplot(parties, aes(x = party, y = dem_auth_mean, fill = party)) +
  geom_dotplot(binaxis = "y", binwidth = 1/250, stackdir = "center", binpositions = "bygroup") +
  geom_boxplot(alpha = 0.4) +
  geom_text_repel(data = erdogan, aes(label = "Erdogan"), color = "black",
                  segment.colour="black", min.segment.length = 0, nudge_x = -0.4, nudge_y = 0.05,
                  box.padding = 1) +
  geom_point(data = erdogan, colour = "red", size = 2, show.legend = FALSE) +
  annotate(geom = "text", x = 2, y = 0.75, label = dem_form, fontface="italic") +
  scale_x_discrete(limits=c(party_order), NULL, labels = NULL) +
  scale_fill_manual(values = colScale2, limits=c(po_r)) + 
  coord_flip() +
  labs(y = "More Democratic <<<<    >>>> More Authoritarian                                                  ",
       x = "Party",
       title = "Individual Averages on Democracy-Authoritarian Dimension, 2019-2021",
       subtitle = "                                            Attitudes Towards the Press by Party",
       fill = "Party") +
  theme_bw() +
  theme(legend.text=element_text(size=13), legend.title = element_text(size=15),
        axis.ticks = element_blank())



figure3



#saving
ggsave("figure3.png", plot = plot1z, width = 8.5, height = 5, dpi = "retina")








####### Figure 4
################################
# Differentiate within the AKP


#Cabinet Ministers
cabinet <- dplyr::filter(ind_scores, position == "Cabinet Minister")

#AKP Parliamentarians
MP <- dplyr::filter(ind_scores, position == "Parliamentarian" & party == "AKP")

#Presidential Advisors
advisor <- dplyr::filter(ind_scores, position == "Presidential Advisor" & confirmed_advisor == "yes")

#Combining
within <- dplyr::bind_rows(cabinet, MP, advisor)
withinP <- dplyr::bind_rows(cabinet, erdogan, MP, advisor)



#####################
##Adding Notation
R2_exec <- summary(lm(dem_auth_mean ~ position, data = within))

rscore_exec <- format(R2_exec$r.squared, digits = 3)

dem_form_exec <- paste("Dem-Auth Sentiment = α + β*Position")
r2_exec <- paste("R² =" , rscore_exec)


#Color and order
colscale3c <- c("darkgoldenrod4", "goldenrod3", "cornsilk3", "darkslategrey")
ppca_pos_order <- c("Cabinet Minister", "Parliamentarian", "President", "Presidential Advisor")
ppca_pos_order_r <- c("Presidential Advisor", "President", "Parliamentarian", "Cabinet Minister")



figure4 <- ggplot(withinP, aes(x = position, y = dem_auth_mean, fill = position)) +
  geom_dotplot(binaxis = "y", binwidth = 1/145, stackdir = "center", binpositions = "bygroup") +
  geom_boxplot(data = within, alpha = 0.4) +
  scale_x_discrete(limits=c(ppca_pos_order), NULL) +
  annotate(geom = "text", x = 1.6, y = 0.8, label = dem_form_exec, fontface="italic") +
  annotate(geom = "text", x = 1.3, y = 0.903, label = r2_exec, fontface="italic") +
  scale_fill_manual(values = colscale3c, limits=c(ppca_pos_order_r)) + 
  coord_flip() +
  labs(y = "More Democratic <<<<    >>>> More Authoritarian                                                     ",
       x = "Position in Party",
       title = "Individual Averages on Democracy-Authoritarian Dimension, 2019-2021",
       subtitle = "                                   Attitudes Towards the Press Within The AKP",
       fill = "by Position in Party") +
  theme_bw() +
  theme(legend.text=element_text(size=13), axis.ticks = element_blank(), axis.text.y = element_blank()) 


figure4
#




#Saving
ggsave("figure4.png", plot = figure4, width = 8.5, height = 5, dpi = "retina")








#################################################
######### REGRESSIONs
# Replicates tables in Appendix C

#Libraries
library(sjPlot)
library(stargazer)

# Tables for regression referenced in Figure 3 - between party differences
parties$party.f <- factor(parties$party) #Making sure a factor variable
reg1 <- summary(lm(dem_auth_mean ~ party.f, data = parties)) #storing regression

#using sjPlot
tab_model(reg1, file = "reg_table1bAKP_parties", p.style = "numeric_stars",
          collapse.ci = TRUE, show.reflvl = TRUE, show.intercept = FALSE, show.fstat = TRUE,
          title = "Democratic-Authoritarian Sentiment \n vis-a-vis the Press, Between Parties")


#Making the reference category the HDP
parties$party.fbHDP <- relevel(parties$party.f, ref = "HDP")
reg_refHDP <- summary(lm(dem_auth_mean ~ party.fbHDP, data = parties))

#using sjPlot
tab_model(reg_refHDP, file = "reg_table2_1bHDP_parties", p.style = "numeric_stars",
          collapse.ci = TRUE, show.reflvl = TRUE, show.intercept = FALSE, show.fstat = TRUE,
          title = "Democratic-Authoritarian Sentiment \n vis-a-vis the Press, Between Parties")




##########
### SAME THING, but this time using ALL TWEETS
# Reproduces Appendix C.1.2

#filtering to keep only the main parties
tweets_parties <- dplyr::filter(tweets_scored, party %in% c("HDP", "CHP", "DEVA", "İYİ", "GELECEK", "AKP", "old AKP", "MHP"))

#making into factor variable
tweets_parties$party.f <- factor(tweets_parties$party)

#First with AKP as reference category
reg_allparties <- summary(lm(dem_auth_press ~ party.f, data = tweets_parties))

#saving
tab_model(reg_allparties, file = "reg_table_allpartiesrefAKP", p.style = "numeric_stars",
          collapse.ci = TRUE, show.reflvl = TRUE, show.intercept = FALSE, show.fstat = TRUE,
          title = "Democratic-Authoritarian Sentiment \n vis-a-vis the Press, Between Parties")



#Now HDP as reference category
tweets_parties$party.fbHDP <- relevel(tweets_parties$party.f, ref = "HDP")

reg_allpartiesRefHDP <- summary(lm(dem_auth_press ~ party.fbHDP, data = tweets_parties))

#saving
tab_model(reg_allpartiesRefHDP, file = "reg_table_allpartiesrefHDP", p.style = "numeric_stars",
          collapse.ci = TRUE, show.reflvl = TRUE, show.intercept = FALSE, show.fstat = TRUE,
          title = "Democratic-Authoritarian Sentiment \n vis-a-vis the Press, Between Parties")



#################################################  
#Appendix C.2 - Within the AKP Government

##############################
#Parliament, Cabinet, Advisors

#debugging - saving table gives error when df starts with "within"
df.within <- as.data.frame(within)

#Making position into factor variable
df.within$position.f <- factor(df.within$position)

#reference category - Parliament
df.within$position.fBparl <- relevel(df.within$position.f, ref = "Parliamentarian")
reg_bPARLpos <- summary(lm(dem_auth_mean ~ position.fBparl, data = df.within))

#saving
tab_model(reg_bPARLpos, file = "reg_tablebPARL_pos", p.style = "numeric_stars",
          collapse.ci = TRUE, show.reflvl = TRUE, show.intercept = FALSE, show.fstat = TRUE,
          title = "Democratic-Authoritarian Sentiment \n vis-a-vis the Press, Within the AKP")


#reference category - Advisors
df.within$position.fBadv <- relevel(df.within$position.f, ref = "Presidential Advisor")
reg_bADV_pos <- summary(lm(dem_auth_mean ~ position.fBadv, data = df.within))

#saving
tab_model(reg_bADV_pos, file = "reg_table4bADV_pos", p.style = "numeric_stars",
          collapse.ci = TRUE, show.reflvl = TRUE, show.intercept = FALSE, show.fstat = TRUE,
          title = "Democratic-Authoritarian Sentiment \n vis-a-vis the Press, Within the AKP")



##########
###Same thing but ALL TWEETS
#Replicates C.2.2

### Pulling out Tweets for...
#Cabinet Ministers
tweets_cabinet <- dplyr::filter(tweets_scored, position == "Cabinet Minister")

#AKP Parliamentarians
tweets_MP <- dplyr::filter(tweets_scored, position == "Parliamentarian" & party == "AKP")

#Presidential Advisors
tweets_advisor <- dplyr::filter(tweets_scored, position == "Presidential Advisor" & confirmed_advisor == "yes")

#Combining
tweets_within <- dplyr::bind_rows(tweets_cabinet, tweets_MP, tweets_advisor)



#Parliamentarians are reference category
reg_position_alltweets <- summary(lm(dem_auth_press ~ relevel(as.factor(position), ref = "Parliamentarian"), data = tweets_within))

#saving table
tab_model(reg_position_alltweets, file = "reg_table_pos_allREFparl", p.style = "numeric_stars",
          collapse.ci = TRUE, show.reflvl = TRUE, show.intercept = FALSE, show.fstat = TRUE,
          title = "Democratic-Authoritarian Sentiment \n vis-a-vis the Press, Within the AKP")


#Advisors are reference category
reg_pos_allADV <- summary(lm(dem_auth_press ~ relevel(as.factor(position), ref = "Presidential Advisor"), data = tweets_within))

#saving
tab_model(reg_pos_allADV, file = "reg_table_pos_allREFadv", p.style = "numeric_stars",
          collapse.ci = TRUE, show.reflvl = TRUE, show.intercept = FALSE, show.fstat = TRUE,
          title = "Democratic-Authoritarian Sentiment \n vis-a-vis the Press, Within the AKP")


