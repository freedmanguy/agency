# install and load libraries
if(!("xfun" %in% installed.packages())){
  install.packages("xfun")
}
xfun::pkg_attach2(c("readr","tm","tidyverse","tidytext","stringr","caret",
                    "SnowballC","ggplot2","mlbench","plotROC","MLeval","fmsb",
                    "rvest","zoo","gbm"))

# import data ----
hearings <- read_csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/US-Legislative-congressional_hearings-19.4.csv")

# remove missing cases
hearings <- filter(hearings, filter_Agency %in% c(0,1))

# create unique id
hearings$myid <- 1:nrow(hearings)

# pre-process data ----
fulldtm <- as.data.frame(hearings) %>%
  filter(grepl("[a-z]",description, ignore.case = T)) %>% 
  unnest_tokens(output = word, input = description) %>%
  filter(!str_detect(word, "^[0-9]*$")) %>% # remove numbers
  anti_join(stop_words) %>% # remove stop words
  mutate(word = SnowballC::wordStem(word)) # stem the words

# create documen-term-matrix ----
fulldtm <- fulldtm %>%
  count(myid, word) %>% # count of each word in each observation
  cast_dtm(document = myid, term = word, value = n) # no weights

# remove observations with insufficient text
hearings.text <- hearings %>% 
  filter(myid %in% as.numeric(as.character(fulldtm$dimnames$Docs)))

# test set ----
# create data frame of test set (random sample with ration 5:1)
set.seed(2400)
testset2 <- hearings.text %>% 
  filter(filter_Agency==1) %>% 
  slice_sample(n=419)
testset2 <- hearings.text %>% 
  filter(filter_Agency==0) %>% 
  slice_sample(n=419*5) %>% 
  bind_rows(testset2, .) %>% 
  arrange(myid)

# reduce population data to exclude test set
popdata2 <- anti_join(hearings.text,testset2)

# training set ----
# create data frame of training set (random sample with ration 5:1)
set.seed(2404)
trainingset2 <- popdata2 %>% 
  filter(filter_Agency==1)

trainingset2 <- popdata2 %>% 
  filter(filter_Agency==0) %>% 
  slice_sample(n = nrow(trainingset2)*5) %>% 
  bind_rows(trainingset2) %>% 
  arrange(myid)

# remove sparce terms
length(fulldtm$dimnames$Terms)
fulldtmS <- fulldtm
fulldtmS <- removeSparseTerms(fulldtmS, sparse = .999)
length(fulldtmS$dimnames$Terms)
fulldtmdf <- as.data.frame(as.matrix(fulldtmS))

# create training set in the form of dtm
mytrain <- fulldtmdf %>% 
  mutate(myid = as.numeric(as.character(rownames(.)))) %>% 
  filter(myid %in% trainingset2$myid)

# remove observations with insufficient text
trainingset2 <- filter(trainingset2, myid %in% mytrain$myid)

# remove id variable
mytrain <- mytrain %>% 
  select(., -myid) %>% 
  as.matrix()

# create test set in the form of dtm
mytest <- fulldtmdf %>% 
  mutate(myid = as.numeric(as.character(rownames(.)))) %>% 
  filter(myid %in% testset2$myid) %>% 
  arrange(myid) %>% 
  select(., -myid) %>% 
  as.matrix()

# Train model (with final specifications) ----
mygbm50 <- train(x = as.matrix(mytrain), # training set
                 y = factor(trainingset2$filter_Agency, # DV
                            levels = c(0,1),
                            labels = c("NotAgency","Agency")),
                 method = "gbm", # use "ranger" for RF
                 # resampling: 
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 10,
                                          repeats = 3, 
                                          classProbs = T,
                                          savePredictions = T), 
                 # tuning parameters: 
                 tuneGrid = data.frame(n.trees = 50, 
                                       n.minobsinnode = 2,
                                       interaction.depth = 10,
                                       shrinkage = .1))

# Predicted probabilities in training set
training_pred <- mygbm50$pred %>% 
  mutate(model = "GBM 50") %>% 
  group_by(obs, rowIndex, model) %>% 
  summarise(Agency_mean = mean(Agency),
            Agency_sd = sd(Agency),
            NotAgency_mean = mean(NotAgency),
            NotAgency_sd = mean(NotAgency))  %>% 
  ungroup() 
training_pred$predAgency <- ifelse(training_pred$Agency_mean>=quantile(training_pred$Agency_mean, .6),
                                   "Agency",
                                   "Not Agency")

# density plot  
training_pred %>% 
  mutate(Observed = factor(obs,
                           levels = c("NotAgency","Agency"),
                           labels = c("Not Agency","Agency"))) %>% 
  ggplot(aes(x = Agency_mean, fill = Observed)) +
  geom_density(alpha = .5) +
  labs(x = "Pr(Agency==1)") +
  theme(legend.position = "bottom")

# ROC curve
evalm(mygbm50)$roc

# Add non-textual features ----
# chamber
myfeatures <- hearings %>% 
  select(myid, filter_House, filter_Joint, subtopic, Congress) %>% 
  unique() %>% 
  rename("House" = "filter_House",
         "Joint" = "filter_Joint")

# proportion subtopic
myfeatures <- hearings %>% 
  group_by(subtopic, Congress) %>% 
  count() %>% 
  group_by(Congress) %>% 
  arrange(Congress, subtopic) %>% 
  mutate(total = sum(n)) %>% 
  mutate(prop_subtopic = n/total) %>% 
  ungroup() %>% 
  select(-n, -total) %>% 
  left_join(myfeatures, .)

# party control
party <- read_html("https://en.wikipedia.org/wiki/Party_divisions_of_United_States_Congresses")
party <- html_nodes(party, "tr td")
party <- gsub("\n","",html_text(party))
congresses <- grep("^[0-9][0-9]?[0-9]?[a-z][a-z]", party, ignore.case = T) 

partyl <- list()
for(i in 1:length(congresses)){
  j <- ifelse(i==length(congresses),1437,(congresses[i+1]-1))
  partyl[[i]] <- data.frame(t(party[c(congresses[i]:j)]))
  print(i)
}

partydf <- plyr::ldply(partyl)
names(partydf) <- c("Congress","Years","Senate_Total","Senate_D","Senate_R",
                    "Senate_Other","Senate_Vac",
                    "House_Total","House_D","House_R","House_Other","House_Vac",
                    "President")
partydf$Congress <- gsub("[a-z]","",as.character(partydf$Congress)) %>% 
  as.character() %>% 
  as.numeric()
partydf$President <- gsub("\\[|\\]|[0-9]","",partydf$President)
partydf$President <- na.locf(partydf$President)

presidents <- read_html("https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States")
presidents <- html_nodes(presidents, "tr td")
presidents <- gsub("\n","",html_text(presidents))
presidents <- presidents[10:411]

siduri <- which(presidents %in% c(1:45))
presidentsl <- list()
for(i in 1:length(siduri)){
  j <- ifelse(i==length(siduri),length(presidents),(siduri[i+1]-1))
  presidentsl[[i]] <- data.frame(t(presidents[c(siduri[i]:j)]))
  print(i)
}

presidentsldf <- plyr::ldply(presidentsl)
presidentsldf <- select(presidentsldf, 4,6)
names(presidentsldf) <- c("President","President_Party")
presidentsldf$President <- gsub("\\[.*\\]","",presidentsldf$President)
presidentsldf$President_Party <- gsub("\\[.*\\]","",presidentsldf$President_Party)

presidentsldf$President <- gsub("William Howard Taft","William H\\. Taft",presidentsldf$President)
partydf <- left_join(partydf, presidentsldf) %>% 
  unique()
partydf$House_D <- as.numeric(as.character(partydf$House_D))
partydf$House_R <- as.numeric(as.character(partydf$House_R))
partydf$Senate_D <- gsub("-.*$|\\[.*$|/.*$|–.*$","",partydf$Senate_D) %>% 
  as.numeric(as.character())
partydf$Senate_R <- gsub("-.*$|\\[.*$|/.*$|–.*$","",partydf$Senate_R) %>% 
  as.numeric(as.character())
partydf$HouseMajority <- ifelse(partydf$House_D>partydf$House_R,
                                "Democrat",
                                "Republican")
partydf$SenateMajority <- ifelse(partydf$Senate_D>partydf$Senate_R,
                                 "Democrat",
                                 "Republican")
partydf$President_Party[partydf$President_Party=="Democratic"] <- "Democrat"
partydf$SamePartyAll <- ifelse(partydf$HouseMajority==partydf$SenateMajority,
                               ifelse(partydf$HouseMajority==partydf$President_Party,1,0),
                               0)
partydf$SamePartyHouseSenate <- ifelse(partydf$HouseMajority==partydf$SenateMajority,
                                       1,
                                       0)
partydf$SamePartyHousePresident <- ifelse(partydf$HouseMajority==partydf$President_Party,
                                          1,
                                          0)
partydf$SamePartySenatePresident <- ifelse(partydf$SenateMajority==partydf$President_Party,
                                           1,
                                           0)

myfeatures <- left_join(myfeatures, select(partydf, 
                                           Congress,
                                           SamePartyAll,
                                           SamePartyHouseSenate,
                                           SamePartyHousePresident,
                                           SamePartySenatePresident))

# dw scores
dw <- read_csv("HSall_members.csv") %>% 
  filter(chamber !="President")

myfeatures <- dw %>% 
  rename("Congress" = "congress") %>%
  mutate(dw_dem = ifelse(party_code==100,
                         nominate_dim1, NA),
         dw_rep = ifelse(party_code==200,
                         nominate_dim1, NA)) %>% 
  group_by(Congress) %>% 
  summarise(avg_dw = mean(nominate_dim1, na.rm = T),
            avg_dw_dem = mean(dw_dem, na.rm = T),
            avg_dw_rep = mean(dw_rep, na.rm = T)) %>% 
  left_join(myfeatures, .)

myfeatures$chamber <- "House"
myfeatures$chamber[myfeatures$House==0] <- "Senate"
myfeatures$chamber[myfeatures$Joint==1] <- "Joint"

myfeatures <- dw %>% 
  rename("Congress" = "congress") %>%
  mutate(dw_dem = ifelse(party_code==100,
                         nominate_dim1, NA),
         dw_rep = ifelse(party_code==200,
                         nominate_dim1, NA)) %>% 
  group_by(Congress, chamber) %>% 
  summarise(avg_dw_chamber = mean(nominate_dim1, na.rm = T),
            avg_dw_dem_chamber = mean(dw_dem, na.rm = T),
            avg_dw_rep_chamber = mean(dw_rep, na.rm = T)) %>% 
  left_join(myfeatures, .)
myfeatures$avg_dw_chamber[myfeatures$Joint==1] <- myfeatures$avg_dw[myfeatures$Joint==1]
myfeatures$avg_dw_dem_chamber[myfeatures$Joint==1] <- myfeatures$avg_dw[myfeatures$Joint==1]
myfeatures$avg_dw_rep_chamber[myfeatures$Joint==1] <- myfeatures$avg_dw[myfeatures$Joint==1]

myfeatures <- hearings %>% 
  group_by(Congress, subtopic) %>% 
  count() %>% 
  group_by(Congress) %>% 
  mutate(subtopic_count_scaled = as.numeric(as.character(scale(n)))) %>% 
  select(-n) %>% 
  left_join(myfeatures, .)

myfeatures <- myfeatures %>% 
  ungroup() %>% 
  select(-subtopic, -Congress, -chamber)

# training set with additional non-textual features
mytrain_nontext <- fulldtmdf %>% 
  mutate(myid = as.numeric(as.character(rownames(.)))) %>% 
  filter(myid %in% trainingset2$myid) %>% 
  left_join(., myfeatures) %>% 
  arrange(myid) %>% 
  select(., -myid) %>% 
  as.matrix()

# test set with additional non-textual features
mytest_nontext <- fulldtmdf %>% 
  mutate(myid = as.numeric(as.character(rownames(.)))) %>% 
  filter(myid %in% testset2$myid) %>% 
  left_join(., myfeatures) %>% 
  arrange(myid) %>% 
  select(., -myid) %>% 
  as.matrix()

# train model with additional non-textual features ----
mygbm50_nontext <- train(x = as.matrix(mytrain_nontext),
                         y = factor(trainingset2$filter_Agency,
                                    levels = c(0,1),
                                    labels = c("NotAgency","Agency")),
                         method = "gbm",
                         # importance = "impurity",
                         trControl = trainControl(method = "repeatedcv", number = 10,
                                                  repeats = 3, classProbs = T,
                                                  savePredictions = T), 
                         tuneGrid = data.frame(n.trees = 50,
                                               n.minobsinnode = 3,
                                               interaction.depth = 5,
                                               shrinkage = .1))
# compare roc curves
evalm(list(mygbm50,mygbm50_nontext),
      gnames = c("50","50 +"))$roc

# Variable importance ----
varImp(mygbm50_nontext)

# Predict on test set ----
testset_predictions <- predict.train(mygbm50_nontext, # trained model
                                     as.matrix(mytest_nontext), # dtm of test set
                                     "prob") %>% # return class probabilities
  mutate(Observed = factor(testset2$filter_Agency,
                           levels = c(0,1),
                           labels = c("Not Agency","Agency")))

# density plot
ggplot(testset_predictions, aes(x = Agency, fill = Observed)) +
  geom_density(alpha = .5) +
  labs(x = "Pr(Agency==1)",
       caption = "Pr(Agency)== Qauntile .6 (60%)") +
  theme(legend.position = "bottom") +
  geom_vline(aes(xintercept=quantile(Agency, .6)), color = "red", linetype = 3) +
  theme(plot.caption = element_text(colour = "red"))

# Applying to unseen data ----

# Assuming you have a dataset of old data
# data need to be pre-processed:
    # convert to a dtm
    # remove numbers, remove stop words, stem words, remove sparse terms
    # include only terms that are in the original training set
    # add non-textual features
# old_hearings <- read_rds("oldhearings.RDS")

# to predict on the pre-processed data uncomment the following:
# old_predicted <- predict.train(mygbm50_nontext,
#                                newdata = olddtm),
# type = "prob")

# assign predicted categories - uncomment:
# old_predicted$Predicted <- ifelse(old_predicted$Agency>=quantile(old_predicted$Agency, .96),
#                                   "Agency",
#                                   ifelse(old_predicted$Agency>=quantile(old_predicted$Agency, .86) & old_predicted$Agency<quantile(old_predicted$Agency, .96),
#                                          "Maybe Agency",
#                                          "Not Agency"))