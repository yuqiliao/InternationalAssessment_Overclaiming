### Explore EdSurvey
### 6/13/18
### Yuqi Liao

# install.packages("EdSurvey")
# install the latest edsurvey on github (to get the latest feature/bug fixes)
# devtools::load_all("U:/ESSIN Task 14/NAEP R Program/Yuqi/edsurvey")

### install gganimate and its dependents
# install.packages("installr")
# library(installr)
# install.ImageMagick()
devtools::install_github("dgrtwo/gganimate")
Sys.setenv(PATH = paste("C:/PROGRA~1/ImageMagick-7.0.8-Q16",
                        Sys.getenv("PATH"), sep = ";"))
install.packages("animation")
library(animation)
ani.options(convert = 'C:/PROGRA~1/ImageMagick-7.0.8-Q16/convert.exe')



#library(EdSurvey)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(directlabels)
library(data.table)

# downloadPISA(years = 2012,
#              root = "G:/Conference/2019/Data/PISA", cache=FALSE)
PISA2012 <- readPISA(path = "G:/Conference/2019/Data/PISA/2012",
                     database = "INT",
                     countries = "*", verbose=TRUE)

T95_G8_USA <- readTIMSS(path = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Data/TIMSS/T1995/TIMSS/Grade 08/Y1995/Data/SPSS/",
                        countries = "usa",
                        gradeLvl = "8")
T99_G8_USA <- readTIMSS(path = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Data/TIMSS/T1999/TIMSS/Grade 08/Y1999/Data/SPSS/",
                        countries = "usa",
                        gradeLvl = "8")
T03_G8_USA <- readTIMSS(path = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Data/TIMSS/T2003/TIMSS/Grade 08/Y2003/Data/SPSS/",
                        countries = "usa",
                        gradeLvl = "8")
T07_G8_USA <- readTIMSS(path = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Data/TIMSS/T2007/TIMSS/Grade 08/Y2007/Data/SPSS/",
                        countries = "usa",
                        gradeLvl = "8")
T11_G8_USA <- readTIMSS(path = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Data/TIMSS/T2011/TIMSS/Grade 08/Y2011/Data/SPSS/",
                        countries = "usa",
                        gradeLvl = "8")
T15_G8_USA <- readTIMSS(path = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Data/TIMSS/T2015/TIMSS/Grade 08/Y2015/Data/SPSS/",
                        countries = "usa",
                        gradeLvl = "8")




### Save objects -----
# save.image(file = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/RObjects.Rdata")
# load("G:/Data Science/Data_Viz/Github/data-viz/International Assessment/RObjects.Rdata")





### Idea 1 - likert chart -----

searchSDF(string = "Proper Number", data = PISA2012, levels = TRUE)

# Proper Number
Math_st62q04 <- edsurveyTable(formula = math ~ st62q04, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife")

## Ask Trang at some point: I got the following error when running the line above. What is the fastest way to find out which countries is the #44
# Error on dataset 44: Error in calcEdsurveyTable(formula, sdf, weightVar, jrrIMax, pctAggregationLevel, : The requested data has 0 row so crosstab analysis cannot be done.
# 
#                                                 Warning message:
#                                                   In getData(data, reqvar, includeNaLabel = includeNaLabel, returnJKreplicates = (varMethod ==  :
#                                                                                                                                     The requested data set has 0 rows.

# code to see which country is which
PISA2012$covs


Math_st62q04$data %>% 
  View()

filtered_data <- Math_st62q04$data %>% 
  filter(country %in% c("Albania", "Argentina", "Australia", "Belgium", "Brazil", "Canada", "Chile", "Iceland", "United States"))
# define likert() using the script in the yuqi-branch of the edsurvey repo #need to swtich to yuqi-branch before running below
#source("U:/ESSIN Task 14/NAEP R Program/Yuqi/edsurvey/R/likert.R")

plot <- likert(sdf = PISA2012,
               #data = Math_st62q04$data,
               data = filtered_data,
               LikertVar = "st62q04",
               byVar = "country",
               LegendTitle = "Proper Number",
               pal = c("#DF4949", "#E27A3F", "#BEBEBE","#45B29D", "#334D5C"),
               ascending = FALSE)

new_theme <- theme(panel.background = element_blank(),
                    #legend
                    legend.position = "right",
                    legend.justification = "top",
                    legend.text = element_text(color = "black", hjust = 0),
                    legend.title = element_text(color = "black", hjust = 0, face = "bold"),
                    legend.key = element_blank(),
                    #plot title
                    plot.title = element_text(size = 15, color = "black", hjust = 0, face = "bold"),
                    plot.subtitle = element_text(size = 8, color = "black", hjust = 0, face = "italic"),
                    legend.background = element_blank(),
                    strip.background = element_blank(),
                    plot.background = element_blank(),
                    #panel.grid = element_blank(),
                    #axis.line = element_line(color = "red"),
                    #axis.ticks = element_line(color = "red"),
                    #axis.title.y = element_text(color = "black", hjust = 0, face = "bold"),
                    axis.title.x = element_text(size = 8, color = "black", hjust = 0, face = "plain"),
                    axis.text = element_text(color = "black")
                    )  
plot + 
  #over-write the chart title defined in the likert chart function
  labs(title="Percentage distribution of students' response",
       subtitle = "Thinking about mathematical concepts: how familiar are you with:",
       #caption = "optional caption",
       ylab="Percent", xlab="") +
  new_theme



### Idea 2 - animation on trends -----
#for T95_G8_USA, keep only the students who were grade 8
T95_G8_USA_filtered <- subset(T95_G8_USA,idgrade %in% 8)
TIMSSAllYears_G8_USA <- edsurvey.data.frame.list(list(T95_G8_USA_filtered, T99_G8_USA, T03_G8_USA, T07_G8_USA, T11_G8_USA, T15_G8_USA),
                                                 labels= c("T95_G8_USA", "T99_G8_USA", "T03_G8_USA", "T07_G8_USA", "T11_G8_USA", "T15_G8_USA"))
#check the variable of itsex to make sure it is consistent across years
searchSDF(string="itsex", data=TIMSSAllYears_G8_USA, levels = TRUE) 
#it turns out, i need to change "female/male" in 2016 into "girl/boy", need to redo the above two codes accordingly.
T15_G8_USA <- recode.sdf(T15_G8_USA,
                      recode = list(itsex = list(from = "FEMALE",
                                                  to = "GIRL"),
                                    itsex = list(from = "MALE",
                                                 to = "BOY")))



#gap anaysis
gapResult <- gap(variable = 'mmat', data = TIMSSAllYears_G8_USA)

mathGap_itsex <- gap(variable = 'mmat', data = TIMSSAllYears_G8_USA,
                  groupA = itsex %in% "GIRL", groupB = itsex %in% "BOY")

ssciGap_itsex <- gap(variable = 'ssci', data = TIMSSAllYears_G8_USA,
                     groupA = itsex %in% "GIRL", groupB = itsex %in% "BOY")


#contruct data frame for visualization
ssci_itsex <- ssciGap_itsex$results %>% 
  select(labels, estimateA, estimateB) %>% 
  mutate(year = c("1995", "1999", "2003", "2007", "2011", "2015")) %>% 
  rename(Girl = estimateA,
         Boy = estimateB) %>% 
  select(-labels) %>% 
  gather(key = "Gender",
         value = "ScienceScore",
         -year)

#visualize
new_theme <- theme(#legend
                   legend.position = "right",
                   legend.justification = "top",
                   legend.text = element_text(color = "black", hjust = 0),
                   legend.title = element_text(color = "black", hjust = 0, face = "bold"),
                   legend.key = element_blank(),
                   legend.background = element_blank(),
                   #plot title
                   plot.title = element_text(size = 15, color = "black", hjust = 0, face = "bold"),
                   plot.subtitle = element_text(size = 8, color = "black", hjust = 0, face = "italic"),
                   strip.background = element_blank(),
                   #panel
                   panel.background = element_blank(),
                   panel.grid.major.y = element_line(size = 0.5, linetype = 'dashed',
                                                   colour = "grey"), 
                   #axis.line = element_line(color = "red"),
                   #axis.ticks = element_line(color = "red"),
                   #axis.title.y = element_text(color = "black", hjust = 0, face = "bold"),
                   axis.title.x = element_text(size = 8, color = "black", hjust = 0, face = "plain"),
                   axis.text = element_text(color = "black")
)  

ssci_itsex_plot <- ggplot(ssci_itsex,
       aes(x = year, y = ScienceScore, 
           group = Gender, label = round(ScienceScore,0), 
           color = Gender,
           frame = year, cumulative = TRUE)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(position = position_dodge(1), show.legend = FALSE) +
  scale_y_continuous(limits = c(450, 550)) +
  #scale_y_continuous(breaks=c(475, 550), labels=c("Intermediate benchmark", "High benchmark"), position = "right") # this won't work, need to look for other options later
  # add reference lines for benchmarks
  
  #theme_classic() + 
  labs(title="TIMSS Science Performance of 8th-Grade Students in the US, by Gender",
       subtitle = "The gender gap in science is shriking over the years",
       #caption = "optional caption",
       y="Science Scores", x="TIMSS Assessment Year") + new_theme

ssci_itsex_plot2 <- ssci_itsex_plot %>% 
  direct.label(method = "last.qp", debug = FALSE)
ssci_itsex_plot2 <- direct.label(ssci_itsex_plot, method = "last.polygons", debug = FALSE)

ssci_itsex_plot_gif <- gganimate(ssci_itsex_plot, interval = 1.0) 






warming_chart <- ggplot(warming, aes(x = year, y = annual, frame = year, cumulative = TRUE)) +
  geom_line(colour="black") +
  geom_point(shape = 21, colour="black", aes(fill=annual), size=5, stroke=1) +
  scale_x_continuous(limits=c(1880,2015)) +
  scale_y_continuous(limits=c(-0.5,1)) +
  theme_minimal() +
  scale_fill_gradientn(colors = pal, values = vals, rescaler = function(x, ...) x, oob = identity, guide=FALSE) +
  xlab("") +
  ylab("Difference from 1951-1980 (ºC)") +
  theme(text=element_text(size=16, family="Georgia"))

# run in the viewer
gg_animate(warming_chart, interval = 0.1)
#output is here "C:/Users/yliao/AppData/Local/Temp/RtmpCMDaGC/file57842ae93290.gif"  
  



  
  
  select(year, Girl, BOy, Score)


gapResult3$results[,c("labels", "estimateA", "diffAA", "diffAAse",
                      "dofAA", "diffAApValue")]




library(gapminder)
library(ggplot2)

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()


library(gganimate)

gganimate(p)



### Idea 3 - send Claire my data viz portfolios -----