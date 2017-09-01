library(twitteR)
library(rjson)
library(httr)
library(dplyr)
source("../config.R")
####
# I'm not going to cache credientials:
options(httr_oauth_cache=F)
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# get the nzparliament list of MP twitter handles
# code based on
# https://stackoverflow.com/questions/30063597/obtaining-twitter-screen-names-from-a-twitter-list
# but modified to actually work these days by swapping GET for POST

twowner <- "NZParliament"
twlist <- "mps"
api.url <- paste0("https://api.twitter.com/1.1/lists/members.json?slug=",
                  twlist, "&owner_screen_name=", twowner, "&count=200")
response <- GET(api.url, config(token=twitteR:::get_oauth_sig()))
response.list <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
mps_screennames <- sapply(response.list$users, function(i) i$screen_name)
twlist <- "parties"
api.url <- paste0("https://api.twitter.com/1.1/lists/members.json?slug=",
                  twlist, "&owner_screen_name=", twowner, "&count=200")
response <- GET(api.url, config(token=twitteR:::get_oauth_sig()))
response.list <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
parties_screennames <- sapply(response.list$users, function(i) i$screen_name)



seed_accounts <- c(mps_screennames, parties_screennames)

# for reference, seed_accounts are 
shortlist <- c("Parmjeet_Parmar", "JacquiDeanMP", "DrShaneRetiMP", "Stuart_NashMP",
               "BStewartMP", "barclay_todd", "toddmcclaymp", "stuartsmithmp", "DarrochBall", "PeeniHenare", 
               "TabuteauNZFirst", "toddmullerBoP", "AScottWairarapa", "JennySalesa", "MaureenPughMP", 
               "WoodhouseMP", "JoGoodhewMP", "PotoChchEast", "TickClayton", "PitaParaone", "nzsarahdowie", 
               "AnneTolleyMP", "simonjbridges", "NathanGuyOtaki", "bhudson_nz", "DavidParkerMP", 
               "DamienOConnorMP", "JudithCollinsMP", "CarmelSepuloni", "ianmckelviemp", "NgatiBird", 
               "RinoTirikatene", "TraceyMartinMP", "PaulGoldsmithMP", "mekawhaitiri", "FoxMarama", 
               "jono_naylor", "RaymondHuo", "EugenieSage", "AlfredNgaroMP", "MaheshBindra", "BarbaraKuriger", 
               "AndrewLittleMP", "stevenljoyce", "DeniseRocheMP", "Richard_Prosser", "winstonpeters", 
               "DavidClarkNZ", "MarkMitchellMP", "jo_hayes1", "suemoroney", "NanaiaMahuta", 
               "TeUruroaFlavell", "jamileeross", "PeterDunneMP", "JonathanYoungMP", "riabondMP", 
               "KrisinMana", "hekiaparata", "michaelwoodnz", "cjsbishop", "melissaleemp", "SteffanBrowning", 
               "williamson_nz", "annetterongotai", "MaramaDavidson", "bakshiks", "JulieAnneGenter", "janlogie", 
               "GarethMP", "timmacindoe", "ruthdysonmp", "ChesterBorrows", "MaungakiekieSAM", "jcolemanmp", 
               "maggiebarrynz", "nikkikaye", "amyadamsMP", "PhilTwyford", "mojomathers", "ScottSimpsonMP", 
               "CraigFossMP", "TrevorMallard", "adrianrurawhe", "pmbillenglish", "jamespeshaw", "DavidClendon", 
               "clarecurranmp", "LouiseUpston", "paulabennettmp", "KennedyGraham", "BarryCoates", 
               "greencatherine", "AupitoWSio_MP", "chrisfinlayson", "nickywagner", "chrishipkins", 
               "grantrobertson1", "DavidBennettMP", "Megan_Woods", "metiria", "IainLG", "dbseymour", 
               "jacindaardern", "PARFosterBell", "DavidCarterMP", "SimonOConnorMP", "UnitedNZ", "nz_first", 
               "VicLabour", "Maori_Party", "nationalmps", "nzyoungnats", "bluegreens", "YoungGreensNZ", 
               "younglabournz", "YoungActNZ", "actparty", "NZGreens", "NZNationalParty", "nzlabour")

get_twinfo <- function(x){
  # for a given username as textstring, get the twitter user info
  # who they are following
  # who they are follows
  # their tweets
  # save each of these as a csv:
  ## username_accountinfo.csv (1 entry)
  ## username_follows.csv
  ## username_followed_by.csv
  ## username_tweets.csv
  ## username_favourites.csv
  # into a data_raw/username subfolder
  
  # get user account
  # if it errors due to been deleted between gather username and collecting account,
  # then return without doing anything more
  user <-   tryCatch(getUser(x),
           error = function(c) return()
  )
  
  user_fldr <- paste0("data_raw/",x)
  if(dir.exists(user_fldr)){
    return()
  } # if the user already collected, return
  
  dir.create(user_fldr)
  # save user account
  if(length(user) > 0){
  tabular_user <- user$toDataFrame()
  write.csv(tabular_user, file=paste0(user_fldr,"/", x, "_accountinfo.csv"),
            row.names=FALSE, fileEncoding = "UTF-8")
  }
  # use user info to get followers
  followers <- tryCatch(user$getFollowers(retryOnRateLimit=180), error = function(c) NULL)
  if(length(followers) > 0){
  tabular_followers <- bind_rows(lapply(followers, function(x) {x$toDataFrame()}))
  tabular_followers$user_is <- x
  write.csv(tabular_followers, file=paste0(user_fldr,"/", x, "_followed_by.csv"), 
            row.names=FALSE, fileEncoding = "UTF-8")
  }
  # use user info to get following
  following <- tryCatch(user$getFriends(retryOnRateLimit=180), error = function(c) NULL)
  if(length(following) > 0){
  tabular_following <- bind_rows(lapply(following, function(x) {x$toDataFrame()}))
  tabular_following$user_is <- x
  write.csv(tabular_following, file=paste0(user_fldr,"/", x, "_follows.csv"), 
            row.names=FALSE, fileEncoding = "UTF-8")
  }
  # use x to get tweets
  timeLine <- tryCatch(userTimeline(x, n=3200, includeRts=TRUE, retryOnRateLimit = 180,
                           sinceID=682514219703504896), error = function(c) NULL) # end of 2015
  if(length(timeLine) > 0){
  tabular_tweets <- twListToDF(timeLine)
  write.csv(tabular_tweets, file=paste0(user_fldr,"/", x, "_tweets.csv"), 
            row.names=FALSE, fileEncoding = "UTF-8")
  }
  # use user info to get favourites
  likes <- tryCatch(user$getFavorites(retryOnRateLimit=180), error = function(c) NULL)
  if(length(likes) > 0){
  tabular_likes <- twListToDF(likes)
  tabular_likes$user_is <- x
  write.csv(tabular_likes, file=paste0(user_fldr,"/", x, "_favourites.csv"), 
            row.names=FALSE, fileEncoding = "UTF-8")
  }
}

sapply(seed_accounts, get_twinfo)

# deliberately adding Gareth Morgan for hte TOP party as well,
# for the values/policy analysis comparison

get_twinfo("garethmorgannz")

# now we have all the followers captured, we obtain the unique usernames of all the followers,
# and repeat the process on a second pass using that list
# becuase we are checking if a subfolder exists for the account, we are avoiding 
# double downloading

user_files <- list.files("data_raw", pattern=".*account.*", recursive = TRUE)
user_data <- bind_rows(lapply(paste0("data_raw/",user_files), read.csv, stringsAsFactors=FALSE))

user_favs <- list.files("data_raw", pattern=".*favourites.*", recursive = TRUE)
favs_data <- bind_rows(lapply(paste0("data_raw/",user_favs), read.csv, stringsAsFactors=FALSE))

user_follows <- list.files("data_raw", pattern=".*follows.*", recursive = TRUE)
follows_data <- bind_rows(lapply(paste0("data_raw/",user_follows), read.csv, stringsAsFactors=FALSE))

user_followed <- list.files("data_raw", pattern=".*followed_by.*", recursive = TRUE)
followed_data <- bind_rows(lapply(paste0("data_raw/",user_followed), read.csv, stringsAsFactors=FALSE,
                                  fileEncoding="UTF-8", encoding="UTF-8", colClasses="character"))

user_tweets<- list.files("data_raw", pattern=".*tweets.*", recursive = TRUE)
tweet_data <- bind_rows(lapply(paste0("data_raw/",user_tweets), read.csv, stringsAsFactors=FALSE))

followers_screennames <- followed_data$screenName
unique_followers <- unique(followers_screennames)
random_order<- sample(1:length(unique_followers), length(unique_followers), replace=FALSE)
random_followers <- unique_followers[random_order]

# record the accounts in order if I want to get more later
write.csv(data.frame(random_followers), row.names = FALSE, file="fanned_out_accounts.csv")
get_how_many <- 10000
sapply(random_followers[1:10000], get_twinfo)
