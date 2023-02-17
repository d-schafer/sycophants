################################################################
#
#   Sycophants in 280 Characters - by Dean Schafer
#
#   
#   Replication Code
#   01c - Examining the Model
#
#   Includes Replication Code for Figure 1



################################################################
#### FIGURE 1. 100 Most Important Words in the Model
####

#libraries
library(ggplot2)
library(ggrepel)

#Set Working Directory (only works in R Studio - otherwise replace manually)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Loading the final LASSO Model - provided
load("cvfit.lasso_finalmodel.RData")




#gets a list of variables (i.e. "words")
vars <- coef(cvfit.lasso, s = "lambda.min")

#get the names/words and coefficient values
coefList <- data.frame(vars@Dimnames[[1]][vars@i+1],vars@x)
names(coefList) <- c('var','val')


#creates an order index of the coef list
coefList$descending <- order(-coefList$val)
coefList$ascending <- order(coefList$val)



#Most Authoritarian Words 
descend <- data.frame(head(coefList$var[coefList$descending], n = 100))
descend$val <- head(coefList$val[coefList$descending], n = 100)

colnames(descend) <- c("word", "score")

#translations for authoritarian words
eng_auth <- brio::readLines("./dict/authoritarian_eng_words.txt")

descend$eng_word <- eng_auth
descend$dem_auth <- "Authoritarian"

#Just top 50 authoritarian words
descend50 <- head(descend, n = 50) 


###Most democratic words
ascend <- data.frame(head(coefList$var[coefList$ascending], n = 100))
ascend$val <- head(coefList$val[coefList$ascending], n = 100)

colnames(ascend) <- c("word", "score")


#translations for democratic words
eng_dem <- brio::readLines("./dict/democratic_eng_words.txt")

ascend$eng_word <- eng_dem
ascend$dem_auth <- "Democratic"


ascend50 <- head(ascend, n = 50)


#Combining
words_cont <- rbind(descend50, ascend50)



#setting some colors
nocolor <- rgb(0, 0, 0, alpha = 0)

###creating better coordinates so words are spaced more evenly and legible
set.seed(1999)
plot_map <- ggplot(words_cont, aes(x = score, color = score)) +
  geom_dotplot(stackdir = "center", binwidth = 1/50, fill = nocolor, 
               color = nocolor,
               binpositions = "all", stackratio = 5, show.legend = FALSE) # +


point.pos2 <- ggplot_build(plot_map)$data[[1]] #getting point positions
scale_auth <- rev(point.pos2$stackpos[51:100]) #... for authoritarian words
scale_dem <- point.pos2$stackpos[1:50] #... for democratic words
scales_all <- c(scale_auth, scale_dem)#... combining


#adding coordinates for words
words_cont$xtext <- scales_all


#Now the plot - Figure 1
set.seed(1999)
figure1 <- ggplot(words_cont, aes(x = score, color = score)) +
  geom_dotplot(stackdir = "center", binwidth = 1/50, fill = nocolor, 
               color = nocolor, binpositions = "all", stackratio = 5, 
               show.legend = FALSE) +
  scale_colour_viridis_c(option = "A", end = 0.7, direction = 1, guide = NULL) +
  geom_text_repel(aes(x = score, y = xtext, label = eng_word), max.overlaps = 100, size = 3, 
                  segment.colour = NA, force = 3, force_pull = 0.62) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_flip() +
  labs(x = "             More Authoritarian <<<<    >>>> More Democratic",
       title = "Coefficient Scores for 100 Most Important Words in Model",
       caption = "Note: words in model are used to address or speak about journalists and reflect either democratic or authoritarian sentiment.",
  ) +
  theme_bw() +  
  theme(plot.caption = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  scale_x_reverse()


#looking at plot  
figure1




ggsave("figure1.png", plot = figure1, width = 8, height = 6, units = "in", dpi = "retina")



