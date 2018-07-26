### Overclaiming
### 6/22/18
### Yuqi Liao

# install.packages("EdSurvey")
# install the latest edsurvey on github (to get the latest feature/bug fixes)
# devtools::load_all("U:/ESSIN Task 14/NAEP R Program/Yuqi/edsurvey")

### install gganimate and its dependents
# install.packages("installr")
# library(installr)
# install.ImageMagick()
# devtools::install_github("dgrtwo/gganimate")
# Sys.setenv(PATH = paste("C:/PROGRA~1/ImageMagick-7.0.8-Q16",
#                         Sys.getenv("PATH"), sep = ";"))
# install.packages("animation")
# library(animation)
# ani.options(convert = 'C:/PROGRA~1/ImageMagick-7.0.8-Q16/convert.exe')


#library(EdSurvey)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(directlabels)
library(data.table)
library(readr)
library(readxl)

# downloadPISA(years = 2012,
#              root = "G:/Conference/2019/Data/PISA", cache=FALSE)
# PISA2012 <- readPISA(path = "G:/Conference/2019/Data/PISA/2012",
#                      database = "INT",
#                      countries = "*", verbose=TRUE)

PISA2012_ledf <- getData(data = PISA2012, varnames = colnames(PISA2012)[[1]],
                 omittedLevels = TRUE, addAttributes = TRUE)
PISA2012_ledf2 <- getData(data = PISA2012, varnames = c("cnt", "oecd",
                                                        "st62q01", "st62q02", "st62q03", "st62q04", "st62q06", "st62q07", "st62q08", "st62q09", "st62q10", "st62q11", "st62q12", "st62q13", "st62q15", "st62q16", "st62q17", "st62q19", "famcon", "famconc", 
                                                        "st04q01", 
                                                        "st25q01",
                                                        "escs",
                                                        #"homepos",
                                                        #"age",
                                                        #"grade",
                                                        #"homepos",
                                                        "immig",
                                                        "w_fstuwt",
                                                        "math"),
                         omittedLevels = FALSE, addAttributes = TRUE)
head(PISA2012_ledf2)

#rbind.light.edsurvey.data.frame
PISA2012_ledf2_ldf <- do.call(rbind.light.edsurvey.data.frame, PISA2012_ledf2)

as.numeric(unique(PISA2012_ledf2_ldf$cnt))

glimpse(PISA2012_ledf2_ldf)

### Save objects -----
# save.image(file = "G:/Conference/2019/Git/InternationalAssessment_Overclaiming/Code/RObjects.Rdata")
# load("G:/Conference/2019/Git/InternationalAssessment_Overclaiming/Code/RObjects.Rdata")



searchSDF(string = "familiar", data = PISA2012, levels = TRUE)
# The three overclaiming variables are st62q04(Proper Number), st62q11 (Subjunctive Scaling), st62q13 (Declarative Fraction)
# Familiarity with Maths Concepts
# st62q01 - Exponential Function
# st62q02 - Divisor
# st62q03 - Quadratic Function
# *st62q04 - Proper Number
# st62q06 - Linear Equation
# st62q07 - Vectors
# st62q08 - Complex Number
# st62q09 - Rational Number
# st62q10 - Radicals
# *st62q11 - Subjunctive Scaling
# st62q12 - Polygon
# *st62q13 - Declarative Fraction
# st62q15 - Congruent Figure
# st62q16 - Cosine
# st62q17 - Arithmetic Mean
# st62q19 - Probability
# famcon - Familiarity with Mathematical Concepts
# famconc - Familiarity with Mathematical Concepts (Signal Detection  Adjusted)


# Proper Number

# Asked Trang how I could loop this over - below is the answer :)

for (item in c("st62q01", "st62q02") ){
  
  assign(paste0("Math_",item), edsurveyTable(formula = as.formula(paste0("math ~ ",item)), data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife"))
}



### Percentage distribution of fake items -----
Math_st62q01 <- edsurveyTable(formula = math ~ st62q01, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife") 
Math_st62q01_PercentPlot <- Math_st62q01$data %>% 
  ggplot(aes(x = country, y = PCT, fill = st62q01)) +
  geom_bar(stat ="identity") +
  coord_flip()
Math_st62q01_PercentPlot

Math_st62q02 <- edsurveyTable(formula = math ~ st62q02, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife")
Math_st62q02_PercentPlot <- Math_st62q02$data %>% 
  ggplot(aes(x = country, y = PCT, fill = st62q02)) +
  geom_bar(stat ="identity") +
  coord_flip()
Math_st62q02_PercentPlot

Math_st62q03 <- edsurveyTable(formula = math ~ st62q03, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife")

Math_st62q03_PercentPlot <- Math_st62q03$data %>% 
  ggplot(aes(x = country, y = PCT, fill = st62q03)) +
  geom_bar(stat ="identity") +
  coord_flip()
Math_st62q03_PercentPlot

### Percentage distribution of real items -----
# students' response to foil questions (percentage and math scores) -----
#no data for #43 Norway 
Math_st62q04 <- edsurveyTable(formula = math ~ st62q04, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife")

Math_st62q04_PercentPlot <- Math_st62q04$data %>% 
  ggplot(aes(x = country, y = PCT, fill = st62q04)) +
  geom_bar(stat ="identity") +
  coord_flip()
Math_st62q04_PercentPlot


Math_st62q04_plot <- Math_st62q04$data %>% 
  filter(st62q04 %in% c("Never heard of it", "Know it well, understand the concept")) %>% 
  group_by(st62q04) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q04)) +
  geom_point(stat ="identity")
Math_st62q04_plot








Math_st62q11 <- edsurveyTable(formula = math ~ st62q11, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife")

Math_st62q11_plot <- Math_st62q11$data %>% 
  filter(st62q11 %in% c("Never heard of it", "Know it well, understand the concept")) %>% 
  group_by(st62q11) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q11)) +
  geom_point(stat ="identity")
Math_st62q11_plot



Math_st62q13 <- edsurveyTable(formula = math ~ st62q13, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife")

Math_st62q13_plot <- Math_st62q13$data %>% 
  filter(st62q13 %in% c("Never heard of it", "Know it well, understand the concept")) %>% 
  group_by(st62q13) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q13)) +
  geom_point(stat ="identity")
Math_st62q13_plot



#recode foil into only two groups - never heard of vs the rest
Math_st62q04r <- edsurveyTable(formula = math ~ st62q04, data = PISA2012,
                               jrrIMax = Inf, varMethod = "jackknife",
                               recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                               "Heard of it a few times",
                                                               "Heard of it often",
                                                               "Know it well, understand the concept"),
                                                        to=c("Heard of it"))))

Math_st62q04r_plot <- Math_st62q04r$data %>% 
  #filter(st62q04 %in% c("Never heard of it", "Know it well, understand the concept")) %>% 
  group_by(st62q04) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q04)) +
  geom_point(stat ="identity")
Math_st62q04r_plot


Math_st62q11r <- edsurveyTable(formula = math ~ st62q11, data = PISA2012,
                               jrrIMax = Inf, varMethod = "jackknife",
                               recode=list(st62q11=list(from=c("Heard of it once or twice",
                                                               "Heard of it a few times",
                                                               "Heard of it often",
                                                               "Know it well, understand the concept"),
                                                        to=c("Heard of it"))))

Math_st62q11r_plot <- Math_st62q11$data %>% 
  #filter(st62q11 %in% c("Never heard of it", "Know it well, understand the concept")) %>% 
  group_by(st62q11) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q11)) +
  geom_point(stat ="identity")
Math_st62q11r_plot



Math_st62q13r <- edsurveyTable(formula = math ~ st62q13, data = PISA2012,
                               jrrIMax = Inf, varMethod = "jackknife",
                               recode=list(st62q13=list(from=c("Heard of it once or twice",
                                                               "Heard of it a few times",
                                                               "Heard of it often",
                                                               "Know it well, understand the concept"),
                                                        to=c("Heard of it"))))

Math_st62q13r_plot <- Math_st62q13$data %>% 
  filter(st62q13 %in% c("Never heard of it", "Know it well, understand the concept")) %>% 
  group_by(st62q13) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q13)) +
  geom_point(stat ="identity")
Math_st62q13r_plot



### correlation between math score and the familiarty items -----
PISA2012_ledf2_ldf %>% 
  mutate(avg_13 = (as.numeric(st62q01) + as.numeric(st62q02) + as.numeric(st62q03) + as.numeric(st62q06) + as.numeric(st62q07) + as.numeric(st62q08) + as.numeric(st62q09) + as.numeric(st62q10) + as.numeric(st62q12) + as.numeric(st62q15) + as.numeric(st62q16) + as.numeric(st62q17) + as.numeric(st62q19))/13) %>% 
  View()

# in the light.edsurvey.data.frame, change "N/A" "Invalid" and "Missing" levels into NA
for (i in c("st62q01", "st62q02", "st62q03", "st62q04", "st62q06", "st62q07", "st62q08", "st62q09", "st62q10", "st62q11", "st62q12", "st62q13", "st62q15", "st62q16", "st62q17", "st62q19")) {
  PISA2012_ledf2_ldf[[i]] <- factor(PISA2012_ledf2_ldf[[i]], exclude = getAttributes(PISA2012_ledf2_ldf,"omittedLevels"))
}

#calculate the row mean for 13 real items, all 3 fake items, and the avg of 13 real items adjusted.
PISA2012_ledf <- PISA2012_ledf2_ldf %>%        
  rowwise() %>% 
  mutate(avg_13 = mean(c(st62q01, st62q02, st62q03, st62q06, st62q07, st62q08, st62q09, st62q10, st62q12, st62q15, st62q16, st62q17, st62q19), na.rm = TRUE),
         avg_3 = mean(c(st62q04, st62q11, st62q13), na.rm = TRUE),
         avg_13_adjusted = avg_13 - avg_3) %>% 
  #de-activate rowwise()
  ungroup()

# copy the attributes from a similar light.edsurvey.data.frame to the data.frame
attrlist <- attributes(PISA2012_ledf2_ldf)
for (a in names(attrlist)) {
  if (!a %in% c("names","row.names")) {
    attr(PISA2012_ledf,a) <- attr(PISA2012_ledf2_ldf,a)  
  }
}
# the below codes confirms that R is working consistently with STATA :)
PISA2012_ledf %>% 
  filter(oecd == "OECD") %>% 
  summarise(avg = mean(famcon, na.rm = TRUE),
            std = sd(famcon, na.rm = TRUE))
  


# correlation between math score and the average of 13 real items (should be FAMCON)
cor_math_famconc2 <- cor.sdf(x = "math", y = "famcon", data = subset(PISA2012_ledf, cnt %in% "United States of America"),
                            method = "Pearson", weightVar = "default")




cor_math_famconc2 <- cor.sdf(x = "math", y = "famconc", data = PISA2012_ledf,
                           method = "Pearson", weightVar = "default")


# correlation between math score and the average of 3 fake items


# correlation between math score and the adjusted average of 13 real items (avg13 - avg3) (should be FAMCONC)


# correlation (can't seem to agree with the tech report)-----
cor_math_st62q04 <- cor.sdf(x = "math", y = "st62q04", data = PISA2012,
                            method = "Pearson", weightVar = "default")




cor_math_famcon <- cor.sdf(x = "math", y = "famcon", data = PISA2012,
                           method = "Pearson", weightVar = "default")



n_rows <- nrow(PISA2012$covs)
cor_matrix <- NULL
for (i in 1:n_rows){
  cor_matrix[i] <- cor_math_famcon[[i]]$correlation
}
mean(cor_matrix, na.rm = TRUE)



cor_math_famconc <- cor.sdf(x = "math", y = "famconc", data = PISA2012,
                            method = "Pearson", weightVar = "default")
n_rows <- nrow(PISA2012$covs)
cor_matrix <- NULL
for (i in 1:n_rows){
  cor_matrix[i] <- cor_math_famconc[[i]]$correlation
}
mean(cor_matrix, na.rm = TRUE)


cor_math_famcon_unweighted <- cor.sdf(x = "math", y = "famcon", data = PISA2012,
                                      method = "Pearson", weightVar = NULL)
n_rows <- nrow(PISA2012$covs)
cor_matrix <- NULL
for (i in 1:n_rows){
  cor_matrix[i] <- cor_math_famcon_unweighted[[i]]$correlation
}
mean(cor_matrix, na.rm = TRUE)

cor_math_famconc_unweighted <- cor.sdf(x = "math", y = "famconc", data = PISA2012,
                                       method = "Pearson", weightVar = NULL)
n_rows <- nrow(PISA2012$covs)
cor_matrix <- NULL
for (i in 1:n_rows){
  cor_matrix[i] <- cor_math_famconc_unweighted[[i]]$correlation
}
mean(cor_matrix, na.rm = TRUE)
















Math_st42q02r_st62q04r <- edsurveyTable(formula = math ~ st42q02 + st62q04, data = PISA2012,
                               jrrIMax = Inf, varMethod = "jackknife",
                               recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                               "Heard of it a few times",
                                                               "Heard of it often",
                                                               "Know it well, understand the concept"),
                                                        to=c("Heard of it")),
                                           st42q02 = list(from = c("Strongly agree",
                                                               "Agree"),
                                                          to = c("AGREE")),
                                           st42q02 = list(from = c("Disagree",
                                                                   "Strongly disagree"),
                                                          to =c("DISAGREE"))))

Math_st42q04r_st62q04r <- edsurveyTable(formula = math ~ st42q04 + st62q04, data = PISA2012,
                                        jrrIMax = Inf, varMethod = "jackknife",
                                        recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                                        "Heard of it a few times",
                                                                        "Heard of it often",
                                                                        "Know it well, understand the concept"),
                                                                 to=c("Heard of it")),
                                                    st42q04 = list(from = c("Strongly agree",
                                                                            "Agree"),
                                                                   to = c("AGREE")),
                                                    st42q04 = list(from = c("Disagree",
                                                                            "Strongly disagree"),
                                                                   to =c("DISAGREE"))))

Math_st42q10r_st62q04r <- edsurveyTable(formula = math ~ st42q10 + st62q04, data = PISA2012,
                                        jrrIMax = Inf, varMethod = "jackknife",
                                        recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                                        "Heard of it a few times",
                                                                        "Heard of it often",
                                                                        "Know it well, understand the concept"),
                                                                 to=c("Heard of it")),
                                                    st42q10 = list(from = c("Strongly agree",
                                                                            "Agree"),
                                                                   to = c("AGREE")),
                                                    st42q10 = list(from = c("Disagree",
                                                                            "Strongly disagree"),
                                                                   to =c("DISAGREE"))))


#outcome - 
#SCMAT - Mathematics Self-Concept
#PERSEV - Perseverance
#MATBEH - Mathematics Behaviour
# MATHEFF - Mathematics Self-Efficacy
# MATINTFC - Mathematics Intentions
# MATWKETH - Mathematics Work Ethic
# ANXMAT - Mathematics Anxiety
anxmat_st62q04r <- edsurveyTable(formula = anxmat ~ st62q04, data = PISA2012,
                                  jrrIMax = Inf, varMethod = "jackknife",
                                  recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                                  "Heard of it a few times",
                                                                  "Heard of it often",
                                                                  "Know it well, understand the concept"),
                                                           to=c("Heard of it"))))
anxmat_st62q04r_plot <- anxmat_st62q04r$data %>% 
  group_by(st62q04) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q04)) +
  geom_point(stat ="identity")
anxmat_st62q04r_plot


anxmat_st62q11r <- edsurveyTable(formula = anxmat ~ st62q11, data = PISA2012,
                                  jrrIMax = Inf, varMethod = "jackknife",
                                  recode=list(st62q11=list(from=c("Heard of it once or twice",
                                                                  "Heard of it a few times",
                                                                  "Heard of it often",
                                                                  "Know it well, understand the concept"),
                                                           to=c("Heard of it"))))
anxmat_st62q11r_plot <- anxmat_st62q11r$data %>% 
  group_by(st62q11) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q11)) +
  geom_point(stat ="identity")
anxmat_st62q11r_plot


anxmat_st62q13r <- edsurveyTable(formula = anxmat ~ st62q13, data = PISA2012,
                                  jrrIMax = Inf, varMethod = "jackknife",
                                  recode=list(st62q13=list(from=c("Heard of it once or twice",
                                                                  "Heard of it a few times",
                                                                  "Heard of it often",
                                                                  "Know it well, understand the concept"),
                                                           to=c("Heard of it"))))
anxmat_st62q13r_plot <- anxmat_st62q13r$data %>% 
  group_by(st62q13) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q13)) +
  geom_point(stat ="identity")
anxmat_st62q13r_plot





matheff_st62q04r <- edsurveyTable(formula = matheff ~ st62q04, data = PISA2012,
                               jrrIMax = Inf, varMethod = "jackknife",
                               recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                               "Heard of it a few times",
                                                               "Heard of it often",
                                                               "Know it well, understand the concept"),
                                                        to=c("Heard of it"))))
matheff_st62q04r_plot <- matheff_st62q04r$data %>% 
  group_by(st62q04) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q04)) +
  geom_point(stat ="identity")
matheff_st62q04r_plot



matheff_st62q11r <- edsurveyTable(formula = matheff ~ st62q11, data = PISA2012,
                               jrrIMax = Inf, varMethod = "jackknife",
                               recode=list(st62q11=list(from=c("Heard of it once or twice",
                                                               "Heard of it a few times",
                                                               "Heard of it often",
                                                               "Know it well, understand the concept"),
                                                        to=c("Heard of it"))))

matheff_st62q11r_plot <- matheff_st62q11r$data %>% 
  group_by(st62q11) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q11)) +
  geom_point(stat ="identity")
matheff_st62q11r_plot



matheff_st62q13r <- edsurveyTable(formula = matheff ~ st62q13, data = PISA2012,
                               jrrIMax = Inf, varMethod = "jackknife",
                               recode=list(st62q13=list(from=c("Heard of it once or twice",
                                                               "Heard of it a few times",
                                                               "Heard of it often",
                                                               "Know it well, understand the concept"),
                                                        to=c("Heard of it"))))

matheff_st62q13r_plot <- matheff_st62q13r$data %>% 
  group_by(st62q13) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q13)) +
  geom_point(stat ="identity")
matheff_st62q13r_plot




scmat_st62q04r <- edsurveyTable(formula = scmat ~ st62q04, data = PISA2012,
                                  jrrIMax = Inf, varMethod = "jackknife",
                                  recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                                  "Heard of it a few times",
                                                                  "Heard of it often",
                                                                  "Know it well, understand the concept"),
                                                           to=c("Heard of it"))))
scmat_st62q04r_plot <- scmat_st62q04r$data %>% 
  group_by(st62q04) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q04)) +
  geom_point(stat ="identity")
scmat_st62q04r_plot




scmat_st62q11r <- edsurveyTable(formula = scmat ~ st62q11, data = PISA2012,
                                  jrrIMax = Inf, varMethod = "jackknife",
                                  recode=list(st62q11=list(from=c("Heard of it once or twice",
                                                                  "Heard of it a few times",
                                                                  "Heard of it often",
                                                                  "Know it well, understand the concept"),
                                                           to=c("Heard of it"))))

scmat_st62q11r_plot <- scmat_st62q11r$data %>% 
  group_by(st62q11) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q11)) +
  geom_point(stat ="identity")
scmat_st62q11r_plot

scmat_st62q13r <- edsurveyTable(formula = scmat ~ st62q13, data = PISA2012,
                                  jrrIMax = Inf, varMethod = "jackknife",
                                  recode=list(st62q13=list(from=c("Heard of it once or twice",
                                                                  "Heard of it a few times",
                                                                  "Heard of it often",
                                                                  "Know it well, understand the concept"),
                                                           to=c("Heard of it"))))
scmat_st62q13r_plot <- scmat_st62q13r$data %>% 
  group_by(st62q13) %>% 
  arrange(desc(PCT), .by_group = TRUE) %>% 
  mutate(country2 = factor(country, levels=unique(country))) %>%
  ggplot(aes(x = country2, y = MEAN, color = st62q13)) +
  geom_point(stat ="identity")
scmat_st62q13r_plot



# broken down by gender -----
Math_st04q01_st62q04r <- edsurveyTable(formula = math ~ st04q01 + st62q04, data = PISA2012,
                               jrrIMax = Inf, varMethod = "jackknife",
                               recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                               "Heard of it a few times",
                                                               "Heard of it often",
                                                               "Know it well, understand the concept"),
                                                        to=c("Heard of it"))))

Math_st04q01_st62q11r <- edsurveyTable(formula = math ~ st04q01 + st62q11, data = PISA2012,
                                       jrrIMax = Inf, varMethod = "jackknife",
                                       recode=list(st62q11=list(from=c("Heard of it once or twice",
                                                                       "Heard of it a few times",
                                                                       "Heard of it often",
                                                                       "Know it well, understand the concept"),
                                                                to=c("Heard of it"))))

Math_st04q01_st62q13r <- edsurveyTable(formula = math ~ st04q01 + st62q13, data = PISA2012,
                                       jrrIMax = Inf, varMethod = "jackknife",
                                       recode=list(st62q13=list(from=c("Heard of it once or twice",
                                                                       "Heard of it a few times",
                                                                       "Heard of it often",
                                                                       "Know it well, understand the concept"),
                                                                to=c("Heard of it"))))



# broken down by language -----
Math_st25q01_st62q04r <- edsurveyTable(formula = math ~ st25q01 + st62q04, data = PISA2012,
                                       jrrIMax = Inf, varMethod = "jackknife",
                                       recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                                       "Heard of it a few times",
                                                                       "Heard of it often",
                                                                       "Know it well, understand the concept"),
                                                                to=c("Heard of it"))))

Math_st25q01_st62q11r <- edsurveyTable(formula = math ~ st25q01 + st62q11, data = PISA2012,
                                       jrrIMax = Inf, varMethod = "jackknife",
                                       recode=list(st62q11=list(from=c("Heard of it once or twice",
                                                                       "Heard of it a few times",
                                                                       "Heard of it often",
                                                                       "Know it well, understand the concept"),
                                                                to=c("Heard of it"))))

Math_st25q01_st62q13r <- edsurveyTable(formula = math ~ st25q01 + st62q13, data = PISA2012,
                                       jrrIMax = Inf, varMethod = "jackknife",
                                       recode=list(st62q13=list(from=c("Heard of it once or twice",
                                                                       "Heard of it a few times",
                                                                       "Heard of it often",
                                                                       "Know it well, understand the concept"),
                                                                to=c("Heard of it"))))




# regression (predit math using foil_recode and other controls)-----

lm_Math_st62q04r_st04q01_st25q01 <- lm.sdf(formula = math ~ st62q04 + st04q01 + st25q01, data = PISA2012,
                                           recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                                           "Heard of it a few times",
                                                                           "Heard of it often",
                                                                           "Know it well, understand the concept"),
                                                                    to=c("Heard of it"))))

lm_Math_st62q04r <- lm.sdf(formula = math ~ st62q04 , data = PISA2012,
                                           recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                                           "Heard of it a few times",
                                                                           "Heard of it often",
                                                                           "Know it well, understand the concept"),
                                                                    to=c("Heard of it"))))


# wrong object (need to use SPSS/STATA instead)
lm_st62q04r_pv1math_st04q01_st25q01 <- glm.sdf(formula = I(st62q04 == "Heard of it") ~ pv1math + st04q01 + st25q01, data = PISA2012,
                                           recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                                           "Heard of it a few times",
                                                                           "Heard of it often",
                                                                           "Know it well, understand the concept"),
                                                                    to=c("Heard of it"))))
#QC glm.sdf
st04q01_escs_st25q01 <- glm.sdf(formula = I(st04q01 == "Female") ~ escs + st25q01, data = PISA2012$datalist[[18]])

lm_st62q04r_pv1math <- lm.sdf(formula = st62q04 ~ pv1math, data = PISA2012,
                                              recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                                              "Heard of it a few times",
                                                                              "Heard of it often",
                                                                              "Know it well, understand the concept"),
                                                                       to=c("Heard of it"))))
# do it the right way
glm_st62q04r_pv1math_st04q01_st25q01 <- glm.sdf(formula = I(st62q04 == "Heard of it") ~ pv1math + I(st04q01 == "Male") + I(st25q01 == "Language of the test"), data = PISA2012,
                                               recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                                               "Heard of it a few times",
                                                                               "Heard of it often",
                                                                               "Know it well, understand the concept"),
                                                                        to=c("Heard of it"))),
                                               returnVarEstInputs = TRUE )

result <- oddsRatio(glm_st62q04r_pv1math_st04q01_st25q01[[1]]$fitted.values)

coef(glm_st62q04r_pv1math_st04q01_st25q01[[1]])
predict(glm_st62q04r_pv1math_st04q01_st25q01[[1]], type = "response")

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coef(glm_st62q04r_pv1math_st04q01_st25q01[[1]]))




lm_st62q04r_pv1math_st04q01_st25q01 <- lm.sdf(formula = I(st62q04 == "Heard of it") ~ pv1math + I(st04q01 == "Male") + I(st25q01 == "Language of the test"), data = PISA2012,
                                               recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                                               "Heard of it a few times",
                                                                               "Heard of it often",
                                                                               "Know it well, understand the concept"),
                                                                        to=c("Heard of it"))))




lm_st62q04_pv1math <- lm.sdf(formula = st62q04 ~ pv1math, data = PISA2012)
lm_st62q04_pv1math_st04q01_st25q01 <- lm.sdf(formula = st62q04 ~ pv1math + st04q01 + st25q01, data = PISA2012)




PISA2012_ledf2_USA <- PISA2012_ledf2[[63]] %>% 
  mutate(st62q04r = )
  

lm_st62q04r_pv1math <- lm.sdf(formula = st62q04 ~ pv1math, data = PISA2012_ledf2,
                              recode=list(st62q04=list(from=c("Heard of it once or twice",
                                                              "Heard of it a few times",
                                                              "Heard of it often",
                                                              "Know it well, understand the concept"),
                                                       to=c("Heard of it"))))


# try to use the correction method to another index/variable? (don't know how)-----



# Export objects as excels-----

objectList <- list(Math_st62q01, Math_st62q02, Math_st62q03, Math_st62q04, Math_st62q11, Math_st62q13, Math_st62q04r, Math_st62q11r, Math_st62q13r, Math_st04q01_st62q04r, Math_st04q01_st62q11r, Math_st04q01_st62q13r, Math_st25q01_st62q04r, Math_st25q01_st62q11r, Math_st25q01_st62q13r, anxmat_st62q04r, anxmat_st62q11r, anxmat_st62q13r, matheff_st62q04r, matheff_st62q11r, matheff_st62q13r, scmat_st62q04r, scmat_st62q11r, scmat_st62q13r)


names(objectList) <- c("Math_st62q01", "Math_st62q02", "Math_st62q03", "Math_st62q04", "Math_st62q11", "Math_st62q13", "Math_st62q04r", "Math_st62q11r", "Math_st62q13r", "Math_st04q01_st62q04r", "Math_st04q01_st62q11r", "Math_st04q01_st62q13r", "Math_st25q01_st62q04r", "Math_st25q01_st62q11r", "Math_st25q01_st62q13r", "anxmat_st62q04r", "anxmat_st62q11r", "anxmat_st62q13r", "matheff_st62q04r", "matheff_st62q11r", "matheff_st62q13r", "scmat_st62q04r", "scmat_st62q11r", "scmat_st62q13r")

i <- 1
for (object in objectList){
  write_csv(object$data, path = paste0(names(objectList)[[i]], ".csv"))
  i <- i + 1
}
rm(i)








#foil quesitons by some other characteristics
#gender
Math_st62q04_st04q01 <- edsurveyTable(formula = math ~ st62q04 + st04q01, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife")
Math_st62q11_st04q01 <- edsurveyTable(formula = math ~ st62q11 + st04q01, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife")
Math_st62q13_st04q01 <- edsurveyTable(formula = math ~ st62q13 + st04q01, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife")


Math_st62q04_langn <- edsurveyTable(formula = math ~ st62q04 + langn, data = PISA2012,
                                      jrrIMax = Inf, varMethod = "jackknife")
Math_st62q04_st25q01 <- edsurveyTable(formula = math ~ st62q04 + st25q01, data = PISA2012,
                                    jrrIMax = Inf, varMethod = "jackknife")
Math_st62q11_st25q01 <- edsurveyTable(formula = math ~ st62q11 + st25q01, data = PISA2012,
                                      jrrIMax = Inf, varMethod = "jackknife")
Math_st62q13_st25q01 <- edsurveyTable(formula = math ~ st62q13 + st25q01, data = PISA2012,
                                      jrrIMax = Inf, varMethod = "jackknife")







#lang variables important!!!
Math_langn <- edsurveyTable(formula = math ~ langn, data = PISA2012,
                                    jrrIMax = Inf, varMethod = "jackknife")

Math_st25q01 <- edsurveyTable(formula = math ~ st25q01, data = PISA2012,
                            jrrIMax = Inf, varMethod = "jackknife")

#
Math_st04q01_st62q04 <- edsurveyTable(formula = math ~ st04q01 + st62q04, data = PISA2012,
                                      jrrIMax = Inf, varMethod = "jackknife",
                                      returnMeans = TRUE, returnSepct = TRUE)
Math_st04q01_st62q11 <- edsurveyTable(formula = math ~ st04q01 + st62q11, data = PISA2012,
                                      jrrIMax = Inf, varMethod = "jackknife",
                                      returnMeans = TRUE, returnSepct = TRUE)
Math_st04q01_st62q13 <- edsurveyTable(formula = math ~ st04q01 + st62q13, data = PISA2012,
                                      jrrIMax = Inf, varMethod = "jackknife",
                                      returnMeans = TRUE, returnSepct = TRUE)











##Regression
Math <- lm.sdf(formula = math ~ 1, data = PISA2012)
Math_famcon <- lm.sdf(formula = math ~ famcon, data = PISA2012)
Math_famconc <- lm.sdf(formula = math ~ famconc, data = PISA2012)


PISA2012_no43 <- PISA2012[[c(1:42, 44:65)]]
Math_famcon <- lm.sdf(formula = math ~ st62, data = PISA2012)





Math_math <- edsurveyTable(formula = math, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife")
Math_famconc <- edsurveyTable(formula = famconc, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife")



Math_st62q04_st04q01_st25q01 <- edsurveyTable(formula = math ~ st62q04 + st04q01 + st25q01, data = PISA2012,
                              jrrIMax = Inf, varMethod = "jackknife")





# code to see which country is which
PISA2012$covs


Math_st62q04$data %>% 
  View()











# Data Viz

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


### ggplot for stata tables -----
table2 <- read_xls(path = "G:/Conference/2019/Git/InternationalAssessment_Overclaiming/Stata/Stata Output/Output_tables_formatted.xls", sheet = "Table 2-a_ggplot")




table2_long <- table2 %>% 
  mutate(rank_low = rank(`Low claimers`),
         rank_irrational = rank(`Irrational respondents`),
         rank_ideal = rank(`Ideal respondents`),
         rank_over = rank(`Over claimers`),
         rank_all = rank(`All students`)) %>% 
  gather(key = "group" , value = "score", -c(`Education system`, rank_low, rank_irrational, rank_ideal, rank_over, rank_all)) %>%
  rename(Edu = `Education system`)
  


table2_long %>%  ggplot(aes(x = reorder(Edu,rank_ideal), y = score, color = group )) +
  geom_point(aes(shape = group), size = 2.5) +
  scale_shape_manual(values=c(3, 16, 16, 16, 16)) +
  scale_color_manual(values=c( "#000000","#018571", "#80cdc1", "#dfc27d", "#a6611a")) +
  #reference line
  #geom_hline(yintercept = 544.68) +
  coord_flip() +
  
  labs(title="Figure 4. PISA Mathematics scores by student group") +
  theme_bw() +
  scale_x_discrete(name="Education system") +
  scale_y_continuous(name="Mathematic score") 

  


