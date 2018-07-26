### QC for PISA 2000 - 2012
datapath <- "G:/01-EdSurvey (start 06-19)/OECD/PISA/Raw Data/"
### 1. Read-in function
savepath <- '//dc3fs1/dc4work/ESSIN Task 14/NAEP R Program/International Assessment/Read-in Function/Data from R/PISA'
options(digits = 16)
options(scipen=16)
# Math 2012
year <- "2012"
filepath <- paste0(datapath,year)
pisa2012 <- readPISA(path = filepath, database = "INT", cognitive = "score", countries = "*", forceReread = TRUE)



database = "INT"
savepathyear <- paste0(savepath,"/", year,"/",database,"/")
dir.create(savepathyear)
countries_cases <- data.frame("country" = labels(pisa2012$data),"edsurvey" = 0, "getData" = 0, "readCSV" = 0)
for (i in 1:length(labels(pisa2012$data))) {
  temp_df <- tryCatch(getData(pisa2012$data[[i]], varnames = names(pisa2012$data[[i]]), omittedLevels = F, dropUnusedLevels = F),
                      error = function(cond) {
                        message(paste0("Broken dataset: ", labels(pisa2012$data)[i]))
                        message(cond)
                        return(0)
                      })
  countries_cases$edsurvey[i] <-  nrow(pisa2012$data[[i]])
  if(class(temp_df) == "data.frame") {
    data.table::fwrite(temp_df, paste0(savepathyear,tolower(labels(pisa2012$data)[i]),".csv"), col.names = T)
    #countries_cases$getData[i] <- nrow(temp_df)
  }
  cat("\n Finished ")
  cat(labels(pisa2012$data)[i])
}

# combined

combinecsv <- function(filepath, csv_list) {
  filepath <- gsub("/$","", filepath)
  out = list()
  for(i in 1:length(csv_list)) {
    temp <- read_csv(file.path(filepath, csv_list[[i]]), col_types = cols(.default = "c"))
    out[[i]] <- temp
    cat(csv_list[[i]])
    cat("\n")
  }
  ret = do.call('rbind', out)
  return(ret)
}
combined <- combinecsv(savepathyear, 
                       list.files(savepathyear, pattern = "^[a-z][a-z][a-z]\\.csv", ignore.case = T, full.names = F))

# checking combined files
a = fread(file.path(savepathyear,"INT_combined.csv"))

countries_cases$readCSV2 <- sapply(countries_cases$country, function(cnt) {
  fname = file.path(savepathyear,paste0(tolower(cnt),".csv"))
  temp = read_csv(fname)
  return(nrow(temp))
})

saveRDS(countries_cases, file.path(savepathyear, "countries_cases.rds"))
saveRDS(countries_cases, "G:/01-EdSurvey (start 06-19)/OECD/PISA/PISA2012_INT_countries_cases.rds")
# Financial Literacy 2012
pisaFIN2012 <- readPISA(filepath = filepath, database = "FIN", cognitive = "score", countries = "*", forceRead = F)
sum = 0
for (i in 1:length(pisaFIN2012$data)) {
  sum = sum+nrow(pisaFIN2012$data[[i]])
} #OK

savepathyear <- paste0(savepath,"/", year,"/","Financial Literacy","/")
dir.create(savepathyear)


countries_cases <- data.frame("country" = labels(pisaFIN2012$data),"edsurvey" = 0, "getData" = 0, "readCSV" = 0)
for (i in 1:length(labels(pisaFIN2012$data))) {
  temp_df <- tryCatch(getData(pisaFIN2012$data[[i]], varnames = names(pisaFIN2012$data[[i]]), omittedLevels = F, dropUnusedLevels = F),
                      error = function(cond) {
                        message(paste0("Broken dataset: ", labels(pisaFIN2012$data)[i]))
                        message(cond)
                        return(0)
                      })
  countries_cases$edsurvey[i] <- nrow(pisaFIN2012$data[[i]])
  if(class(temp_df) == "data.frame") {
    data.table::fwrite(temp_df, paste0(savepathyear,tolower(labels(pisaFIN2012$data)[i]),".csv"), col.names = T)
    countries_cases$getData[i] <- nrow(temp_df)
  }
  cat("\n Finished")
  cat(labels(pisaFIN2012$data)[i])
}

countries_cases$readCSV <- sapply(countries_cases$country, function(cnt) {
  fname = file.path(savepathyear,paste0(tolower(cnt),".csv"))
  temp = read_csv(fname)
  return(nrow(temp))
})

# Problem Solving 2012
pisaCBA2012 <- readPISA(filepath = paste0(datapath,2012), database = "CBA", cognitive = "score", countries = "*", verbose = TRUE)
savepathyear <- paste0(savepath,"/", year,"/","Problem Solving","/")
dir.create(savepathyear)
countries_cases <- data.frame("country" = labels(pisaCBA2012$data),"edsurvey" = 0, "getData" = 0, "readCSV" = 0)
for (i in 1:length(labels(pisaCBA2012$data))) {
  temp_df <- tryCatch(getData(pisaCBA2012$data[[i]], varnames = names(pisaCBA2012$data[[i]]), omittedLevels = F, dropUnusedLevels = F),
                      error = function(cond) {
                        message(paste0("Broken dataset: ", labels(pisaCBA2012$data)[i]))
                        message(cond)
                        return(0)
                      })
  countries_cases$edsurvey[i] <- nrow(pisaCBA2012$data[[i]])
  if(class(temp_df) == "data.frame") {
    data.table::fwrite(temp_df, paste0(savepathyear,labels(pisaCBA2012$data)[i],".csv"), col.names = T)
    countries_cases$getData[i] <- nrow(temp_df)
  } 
  cat("\n Finished")
  cat(labels(pisaCBA2012$data)[i])
}

countries_cases$readCSV <- sapply(countries_cases$country, function(cnt) {
  fname = file.path(savepathyear,paste0(tolower(cnt),".csv"))
  temp = read_csv(fname)
  return(nrow(temp))
})

saveRDS(countries_cases, "G:/01-EdSurvey (start 06-19)/OECD/PISA/PISA2012_CBA_countries_cases.rds")
# Reading 2009
# International
year <- "2009"
filepath <- paste0(datapath,year)
pisa2009 <- readPISA(filepath = filepath, database = "INT", cognitive = "score", countries = "*")
database = "INT"
savepathyear <- paste0(savepath,"/", year,"/",database,"/")
dir.create(paste0(savepath,"/",year))
dir.create(savepathyear)
for (i in 1:length(labels(pisa2009$data))) {
  temp_df <- tryCatch(getData(pisa2009$data[[i]], varnames = names(pisa2009$data[[i]]), omittedLevels = F, dropUnusedLevels = F),
                      error = function(cond) {
                        message(paste0("Broken dataset: ", labels(pisa2009$data)[i]))
                        message(cond)
                        return(0)
                  s    })
  if(class(temp_df) == "data.frame") {
    data.table::fwrite(temp_df, paste0(savepathyear,tolower(labels(pisa2009$data)[i]),".csv"), col.names = T)
  } 
}

# DRA
# year <- "2009"
# filepath <- paste0(datapath,year)
# pisaDRA2009 <- readPISA(filepath = filepath, database = "ERA", cognitive = "score", countries = "*")
# database = "ERA"
# savepathyear <- paste0(savepath,"/", year,"/","DRA","/")
# dir.create(savepathyear)
# for (i in 1:length(labels(pisaDRA2009$data))) {
#   temp_df <- tryCatch(getData(pisaDRA2009$data[[i]], varnames = names(pisaDRA2009$data[[i]]), omittedLevels = F, dropUnusedLevels = F),
#                       error = function(cond) {
#                         message(paste0("Broken dataset: ", labels(pisaDRA2009$data)[i]))
#                         message(cond)
#                         return(0)
#                         s    })
#   if(class(temp_df) == "data.frame") {
#     data.table::fwrite(temp_df, paste0(savepathyear,tolower(labels(pisaDRA2009$data)[i]),".csv"), col.names = T)
#   } 
# }
# Science 2006
year <- "2006"
filepath <- paste0(datapath,year)
pisa2006 <- readPISA(filepath = filepath, database = "INT", cognitive = "score", countries = "*")
database = "INT"
savepathyear <- paste0(savepath,"/", year,"/",database,"/")
dir.create(paste0(savepath,"/",year))
dir.create(savepathyear)
for (i in 1:length(labels(pisa2006$data))) {
  temp_df <- tryCatch(getData(pisa2006$data[[i]], varnames = names(pisa2006$data[[i]]), omittedLevels = F, dropUnusedLevels = F),
                      error = function(cond) {
                        message(paste0("Broken dataset: ", labels(pisa2006$data)[i]))
                        message(cond)
                        return(0)
                      })
  if(class(temp_df) == "data.frame") {
    data.table::fwrite(temp_df, paste0(savepathyear,tolower(labels(pisa2006$data)[i]),".csv"), col.names = T)
  } 
}


s### 2. EdSurvey Table
### DONE: no error
functionsavepath <- '//dc3fs1/dc4work/ESSIN Task 14/NAEP R Program/International Assessment/PISA/r output'
pisa2012 <- readPISA(filepath = paste0(datapath,2012), database = "INT", cognitive = "score", countries = "*", verbose = TRUE)
alb2012 <- readPISA(filepath, countries = "alb")
alb_res <- edsurveyTable(~st04q01 + st20q01, data = alb2012, omittedLevels = FALSE)
out <- data.frame()
for (cntl in pisa2012$data) {
  cat("\n")
  cat(cntl$country)
  cat("\n")
  error_message = ""
  gender_table <- tryCatch(edsurveyTable(~ st04q01 + st20q01, data = cntl, jrrIMax = Inf),
                           error = function(cond) {
                             message(cond)
                             error_message = gsub(",","",cond)
                             return(0)
                           })
  temp <- data.frame("cnt" = cntl$country,"error" = error_message, stringsAsFactors = F)
  if (length(gender_table) > 1) {
    temp <- cbind(temp,gender_table$data)
    temp$weight <- gender_table$weight
    temp$njk <- gender_table$njk
    temp$npv <- gender_table$npv
    temp$var_method <- gender_table$varMethod
  } 
  out <- plyr::rbind.fill(out, temp)  
}
readr::write_csv(out,paste0(functionsavepath,"/edsurvey.table/PISA2012_GenderbyCountryofBirth.csv"))

### 3. Achievement Levels & Percentiles
### Data Used: Pisa 2012
### reading
showPlausibleValues(pisa2012)
b <- achievementLevels(achievementVars = "read", aggregateBy = "st04q01", 
                       alb2012, cutpoints = NULL)
brazil <- readPISA(filepath = paste0(datapath,2012),countries = "bra")
achievementLevels("read","st04q01",data = brazil, jrrIMax = Inf)
out <- data.frame()
for (i in 1:length(labels(pisa2012$data))) {
  cat("\nCountry: ")
  cnt = labels(pisa2012$data)[i]
  cat(cnt)
  tempDiscrete = achievementLevels(achievementVars = c("read"), aggregateBy = "st04q01", data = pisa2012$data[[i]], jrrIMax = Inf)
  tempDF <- data.frame("cnt" = cnt)
  tempDF$cnt <- cnt
  tempDF$countryName <- pisa2012$data[[i]]$country
  tempDF$n0 <- tempDiscrete$n0
  tempDF$nUsed <- tempDiscrete$nUsed
  tempDF <- cbind(tempDF, tempDiscrete$discrete)
  tempDiscrete$callVars <- lapply(tempDiscrete$callVars, function(l) { paste0(l, collapse = " ")})
  tempDF <- cbind(tempDF,tempDiscrete$callVars)
  out <- rbind(out, tempDF)
}
#dir.create(paste0(functionsavepath, "proficiency.level"))
readr::write_csv(out,paste0(functionsavepath,"/proficiency.level/PISA2012_DiscreteBenchmark(Reading by Gender).csv"))

out <- data.frame()
for (i in 1:length(labels(pisa2012$data))) {
  cat("\nCountry: ")
  cnt = labels(pisa2012$data)[i]
  cat(cnt)
  tempCumulative = achievementLevels(achievementVars = c("read"), aggregateBy = "st04q01", data = pisa2012$data[[i]], returnCumulative = T, jrrIMax = Inf)
  tempDF <- data.frame("cnt" = cnt)
  tempDF$cnt <- cnt
  tempDF$countryName <- pisa2012$data[[i]]$country
  tempDF$n0 <- tempCumulative$n0
  tempDF$nUsed <- tempCumulative$nUsed
  tempDF <- cbind(tempDF, tempCumulative$cumulative)
  tempCumulative$callVars <- lapply(tempCumulative$callVars, function(l) { paste0(l, collapse = " ")})
  tempDF <- cbind(tempDF,tempCumulative$callVars)
  out <- rbind(out, tempDF)
}
readr::write_csv(out,paste0(functionsavepath,"/proficiency.level/PISA2012_CumulativeBenchmark(Reading by Gender).csv"))

### percentile
# investigate
alb <- readPISA(file.path(datapath,2012), countries = "alb", verbose = TRUE)
percentile(variable = "read", c(10,25,50,75,90), data = subset(alb, st04q01 == 'Male'), jrrIMax = Inf)
percentile(variable = "read", c(10,25,50,75,90), data = subset(alb, st04q01 == 'Female'), jrrIMax = Inf)

# ---

pisa2012_male <- percentile(variable = 'read',c(10,25,50,75,90), data = subset(pisa2012, st04q01 == 'Male'), weightVar = "w_fstuwt", jrrIMax = Inf)
pisa2012_female <- percentile(variable = 'read',c(10,25,50,75,90), data = subset(pisa2012, st04q01 == 'Female'), weightVar = "w_fstuwt", jrrIMax = Inf)
gender <- c('male','female')
out0 <- data.frame()
for (g in gender) {
  out <- eval(parse(text = paste0('pisa2012_',g)))
  labels <- as.character(out$labels)
  out <- sapply(out, unlist)
  out <- as.data.frame(out)
  out$labels <- labels
  out$weight <- "w_fstuwt"
  out$gender <- toupper(g)
  out0 <- rbind(out0, out)
}
out <- out0
out <- data.table::as.data.table(out)
out <- out[,-c("confInt.ci_lower", "confInt.ci_upper")]
out$percentile <- paste0("p",out$percentile)
ret <- data.table::dcast(out, labels + weight + gender ~ percentile, value.var = c("estimate","se"))
ret$labels <- toupper(ret$labels)
countryDict <- readr::read_csv(file.path(paste0(datapath,2012),"INT_all-countries.txt"))
countryDict <- countryDict[,c("cnt","country.name")]
ret <- merge(countryDict, ret, by.x = "cnt", by.y = "labels", all.x = F, all.y = T)
readr::write_csv(ret, file.path(functionsavepath,"proficiency.level/percentile(pisa2012).csv"))

### science
#a <- achievementLevels(achievementVars = "scie", aggregateBy = "st87q03", pisa2012$data$ALB)
out <- data.frame()
for (i in 1:length(labels(pisa2012$data))) {
  cat("\nCountry: ")
  cnt = labels(pisa2012$data)[i]
  cat(cnt)
  tempDiscrete = achievementLevels(achievementVars = "scie", aggregateBy = "st87q03", data = pisa2012$data[[i]], jrrIMax = Inf)
  tempDF <- data.frame("cnt" = cnt)
  tempDF$cnt <- cnt
  tempDF$countryName <- pisa2012$data[[i]]$country
  tempDF$n0 <- tempDiscrete$n0
  tempDF$nUsed <- tempDiscrete$nUsed
  tempDF <- cbind(tempDF, tempDiscrete$discrete)
  tempDiscrete$callVars <- lapply(tempDiscrete$callVars, function(l) { paste0(l, collapse = " ")})
  tempDF <- cbind(tempDF,tempDiscrete$callVars)
  out <- rbind(out, tempDF)
}
readr::write_csv(out,paste0(functionsavepath,"/proficiency.level/PISA2012_DiscreteBenchmark(Science by Sense of Belonging).csv"))

out <- data.frame()
for (i in 1:length(labels(pisa2012$data))) {
  cat("\nCountry: ")
  cnt = labels(pisa2012$data)[i]
  cat(cnt)
  tempCumulative = achievementLevels(achievementVars = "scie", aggregateBy = "st87q03", data = pisa2012$data[[i]], returnCumulative = T, jrrIMax = Inf)
  tempDF <- data.frame("cnt" = cnt)
  tempDF$cnt <- cnt
  tempDF$countryName <- pisa2012$data[[i]]$country
  tempDF$n0 <- tempCumulative$n0
  tempDF$nUsed <- tempCumulative$nUsed
  tempDF <- cbind(tempDF, tempCumulative$cumulative)
  tempCumulative$callVars <- lapply(tempCumulative$callVars, function(l) { paste0(l, collapse = " ")})
  tempDF <- cbind(tempDF,tempCumulative$callVars)
  out <- rbind(out, tempDF)
}
write_csv(out,paste0(functionsavepath,"edsurvey.table/PISA2012_CumulativeBenchmark(Reading by Sense of Belonging).csv"))
### 4. Gap Analysis
year <- "2003"
filepath <- paste0(datapath,year)
# investigate
aus <- readPISA(filepath, countries = "aus", verbose = TRUE)
gap(variable = "math", data = aus, groupA = st03q01 == "Male", groupB = st03q01 == "Female",
    weightVar = "w_fstuwt", jrrIMax = Inf)
#
# percentage:
#   pctA   pctAse     pctB   pctBse   diffAB     covAB diffABse diffABpValue    dofAB
# 50.83479 5.857399 49.16521 5.857399 1.669587 -1.715456 8.488178    0.8445701 79.12618
# 
# results:
#   meanA  meanAse    meanB  meanBse   diffAB    covAB diffABse diffABpValue   dofAB
# 526.8926 3.005998 521.5502 2.692823 5.342441 1.108279 3.751102    0.1551554 401.782

pisa2003 <- readPISA(filepath = filepath, database = "INT", cognitive = "score", countries = "*", verbose = TRUE)
A = "Male"
B = "Female"
## Math score difference between M and F in 2003
countryDict <- readr::read_csv(paste0(filepath,"/INT_all-countries.txt"))
result <- gap(variable = "math", data = pisa2003, groupA = st03q01 == "Male", groupB = st03q01 == "Female",
                 weightVar = "w_fstuwt", jrrIMax = Inf)

out <- result$results[,c("labels","meanA","meanB","meanAse","meanBse","diffAB","diffABse","diffABpValue")]
out <- merge(out, result$percentage[,c("labels","pctA","pctB","pctAse","pctBse")], by = "labels")
colnames(out) <- sapply(colnames(out), function(x) {
  if(grepl("^[^d].*se$",x)) {
    return(gsub("(A|B)(se)",paste0("se",substr(x,nchar(x) - 2, nchar(x) - 2)),x))
  }
  return(x)
})

ret = reshape(out, direction = 'long', idvar = c("labels","diffAB","diffABse","diffABpValue"),
              varying = c("meanA","meanseA", "pctA","pctseA","meanB","meanseB","pctB","pctseB"),
              timevar = "ST03Q01", times = c("A","B"),
              v.names = c("mean","meanse","pct","pctse"), new.row.names = NULL)
rownames(ret) <- NULL
ret$ST03Q01 <- sapply(ret$ST03Q01,function(x) get(x))
ret$labels <- toupper(as.character(ret$labels))
ret$labels <- sapply(ret$labels, function(x) {
  countryDict$country.name[which(countryDict$cnt == toupper(x))]
})
ret <- ret[,c("labels", "ST03Q01","mean","meanse","pct","pctse","diffAB","diffABse","diffABpValue")]
readr::write_csv(ret,file.path(functionsavepath,"gap.analysis/PISA 2003_MathbyGender.csv"))

## Math score difference between M and F across 2003 and 2015
# gap function doesn't support variable name change
all_countries = intersect(labels(pisa2003$data), labels(pisa2012$data))
out <- data.frame()
pisa2003_2 <- EdSurvey::rename(pisa2003, "st03q01","gender")
pisa2012_2 <- EdSurvey::rename(pisa2012, "st04q01","gender")

for (cntry in all_countries) {
  cat("\n Country ")
  cat(cntry)
  gap_list <- edsurvey.data.frame.list(list(pisa2003_2$data[[cntry]], pisa2012_2$data[[cntry]]),
                                                 labels = c("2003","2012"))
  gap_temp <- tryCatch(gap(variable = "math", data = gap_list, omittedLevels = F,
                  groupA = gender == "Male" , groupB = gender == "Female", weightVar = 'w_fstuwt', jrrIMax = Inf),
                  error = function(cond) {
                    message(cond)
                    return(0)
                  })
  if (length(gap_temp) > 1) {
    out_temp <- cbind("country" = pisa2012_2$data[[cntry]]$country, gap_temp$results)  
  } else {
    out_temp <- data.frame("country" = pisa2012_2$data[[cntry]]$country)
  }
  
  out <- rbind.fill(out, out_temp)
}
readr::write_csv(out, paste0(functionsavepath,"/gap.analysis/PISA_MathbyGender(2003 vs 2012).csv"))

## Beween US vs. Australia
A = "UNITED STATES"
B = "AUSTRALIA"
us13 <- getData(pisa2003$data$USA, varnames = c("cnt","math","w_fstuwt"), addAttributes = T, defaultConditions = F)
aus13 <- getData(pisa2003$data$AUS, varnames = c("cnt","math","w_fstuwt"), addAttributes = T, defaultConditions = F)
us_aus13 <- rbind.light.edsurvey.data.frame(us13,aus13)
result <- gap(variable = "math", data = us_aus13, groupA = cnt == "USA", 
              groupB = cnt == "AUS", weightVar = "w_fstuwt", jrrIMax = Inf)

out <- result$results[,c("meanA","meanB","meanAse","meanBse","diffAB","diffABse","diffABpValue")]
out <- cbind(out, result$percentage[,c("pctA","pctB","pctAse","pctBse")])
colnames(out) <- sapply(colnames(out), function(x) {
  if(grepl("^[^d].*se$",x)) {
    return(gsub("(A|B)se",paste0("se",substr(x,nchar(x) - 2, nchar(x) - 2)),x))
  }
  return(x)
})
ret = reshape(out, direction = 'long', idvar = c("diffAB","diffABse","diffABpValue"),
              varying = c("meanA","meanseA", "pctA","pctseA","meanB","meanseB","pctB","pctseB"),
              timevar = "country.name", times = c("A","B"),
              v.names = c("mean","meanse","pct","pctse"), new.row.names = NULL)
rownames(ret) <- NULL
ret$country.name <- sapply(ret$country.name,function(x) get(x))
ret <- ret[,c("country.name","mean","meanse","pct","pctse","diffAB","diffABse","diffABpValue")]
readr::write_csv(ret, file.path(functionsavepath, "gap.analysis/PISA2003(US vs Australia).csv"))
### 5. Correlation
year <- 2000
filepath <- paste0(datapath, year)
pisa2000 <- readPISA(filepath, countries = "*", database = "INT", forceRead = T)
pisa2000_cor <- cor.sdf.table(var1 = "read", var2 = "st21q02", data = pisa2000, 
                              var_methods = "Pearson", 
                              weightVar = list("w_fstuwt_read"),
                              omittedLevels = c("Mis","M/R","N/A"))
countryDict <- read_csv(paste0(filepath,"/INT_all-countries.txt"))
out <- merge(countryDict[,c("cnt","country.name")], pisa2000_cor$data, by.x = "cnt", by.y = "Country.Label", all.x = F, all.y = T)
dir.create(paste0(functionsavepath,"correlation"))
write_csv(out, paste0(functionsavepath,"/correlation/PIS2000_READbyST21Q02.csv" ))

### 6. Linear Regression
pisa2012 <- readPISA(filepath = paste0(datapath, 2012), database = "INT", cognitive = "score", countries = "*")
out <- data.frame("IDCNTRY" = character(0),
                  #"CNTRY.NAME" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "coef" = numeric(0),
                  "se" = numeric(0),
                  "t" = numeric(0),
                  "dof" = numeric(0),
                  "pVal" = numeric(0),
                  "weight" = character(0),
                  "n0" = integer(0),
                  "nUsed" = integer(0)
)
# pvmacq ~ st29q06 + sc01q01
dir.create(paste0(functionsavepath,"regression"))
file = file(paste0(functionsavepath,"regression/regression_error.txt"), open = "wt")
sink(file, type = "message", append = T)
for (ci in 1:length(pisa2012$data)) {
  cat("\nCountry ")
  cat(labels(pisa2012$data)[ci])
  lm_temp <- tryCatch(lm.sdf(macq ~ st29q06 + sc01q01, data = pisa2012$data[[ci]], weightVar = 'w_fstuwt', jrrIMax = Inf),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(pisa2012$data[[ci]]$country, nrow(lm_temp$coefmat)))
    out_temp$YVar <- "PVMACQ"
    out_temp$EqVar <- row.names(lm_temp$coefmat)
    out_temp$coef <- lm_temp$coefmat$coef
    out_temp$se <- lm_temp$coefmat$se
    out_temp$t <- lm_temp$coefmat$t
    out_temp$dof <- lm_temp$coefmat$dof
    out_temp$pVal <- lm_temp$coefmat$`Pr(>|t|)`
    out_temp$weight <- lm_temp$weight
    out_temp$n0 <- lm_temp$n0
    out_temp$nUsed <- lm_temp$nUsed
  } else {
    out_temp <- data.frame("IDCNTRY" = pisa2012$data[[ci]]$country, "YVar" = "PVMACQ")
  }
  out <- rbind.fill(out, out_temp)
}
sink(type = "message")
dir.create(paste0(functionsavepath,"regression"))
readr::write_csv(out, file.path(functionsavepath,"regression/PISA2012_MACQ vs ST29Q06 n SC01Q01.csv"))

# math ~ st25q01 + sc03q01 + ic02q07 + ec06q01
cat(file, "\n")
cat(file, "math ~ st25q01 + sc03q01 + ic02q07 + ec06q01")
sink(file, type = "message", append = T)
out <- data.frame()
for (ci in 1:length(pisa2012$data)) {
  cat("\nCountry ")
  cat(labels(pisa2012$data)[ci])
  lm_temp <- tryCatch(lm.sdf(math ~ st25q01 + sc03q01 + ic02q07 + ec06q01, data = pisa2012$data[[ci]], weightVar = 'w_fstuwt', jrrIMax = Inf),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(pisa2012$data[[ci]]$country, nrow(lm_temp$coefmat)))
    out_temp$YVar <- "PVMATH"
    out_temp$EqVar <- row.names(lm_temp$coefmat)
    out_temp$coef <- lm_temp$coefmat$coef
    out_temp$se <- lm_temp$coefmat$se
    out_temp$t <- lm_temp$coefmat$t
    out_temp$dof <- lm_temp$coefmat$dof
    out_temp$pVal <- lm_temp$coefmat$`Pr(>|t|)`
    out_temp$weight <- lm_temp$weight
    out_temp$n0 <- lm_temp$n0
    out_temp$nUsed <- lm_temp$nUsed
  } else {
    out_temp <- data.frame("IDCNTRY" = pisa2012$data[[ci]]$country, "YVar" = "PVMATH")
  }
  out <- plyr::rbind.fill(out, out_temp)
}
sink(type = "message")
dir.create(paste0(functionsavepath,"regression"))
readr::write_csv(out, paste0(functionsavepath,"/regression/PISA2012_MATH vs st25q01 n sc03q01 n ic02q07 n ec06q01.csv"))

### 7. Logistic Regression
dir.create(file.path(functionsavepath,"logistic.regression"))
## b. st04q01 ~ st20q01 + st48q01 + st87q01
pisa_test = glm.sdf(st04q01 ~ st20q01 + st48q01 + st87q01, data = pisaCBA2012$data$AUS)
out <- data.frame("IDCNTRY" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "coef" = numeric(0),
                  "se" = numeric(0),
                  "t" = numeric(0),
                  "dof" = numeric(0),
                  "pVal" = numeric(0),
                  "weight" = character(0),
                  "n0" = integer(0),
                  "nUsed" = integer(0)
)
for (ci in 1:length(pisaCBA2012$data)) {
  cat("\n Processing ")
  cat(labels(pisaCBA2012$data)[ci])
  lm_temp <- tryCatch(glm.sdf(st04q01 ~ st20q01 + st48q01 + st87q01, data = pisaCBA2012$data[[ci]], weightVar = 'w_fstuwt', jrrIMax = Inf),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(pisaCBA2012$data[[ci]]$country, nrow(lm_temp$coefmat)))
    out_temp$YVar <- "ST04Q01"
    out_temp$EqVar <- row.names(lm_temp$coefmat)
    out_temp$coef <- lm_temp$coefmat$coef
    out_temp$se <- lm_temp$coefmat$se
    out_temp$t <- lm_temp$coefmat$t
    out_temp$dof <- lm_temp$coefmat$dof
    out_temp$pVal <- lm_temp$coefmat$`Pr(>|t|)`
    out_temp$weight <- lm_temp$weight
    out_temp$n0 <- lm_temp$n0
    out_temp$nUsed <- lm_temp$nUsed
  } else {
    out_temp <- data.frame("IDCNTRY" = pisaCBA2012$data[[ci]]$country, "YVar" = "ST04Q01")
  }
  out <- plyr::rbind.fill(out, out_temp)
}
readr::write_csv(out, file.path(functionsavepath,"logistic.regression/PISA CBA (gender).csv"))
### 8. Multilevel Regression

