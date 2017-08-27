library(twitteR)
library(rjson)
library(httr)

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

# shortlist is 
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
  ## username_dateObtained_accountinfo.csv (1 entry)
  ## username_dateObtained_follows.csv
  ## username_dateObtained_followed_by.csv
  ## username_dateObtained_tweets.csv
  # into a data_raw/username subfolder
  
  if(dir.exists(paste0("data_raw/",x))){
    return()
  } # if the user already collected, return
  dir.create(paste0("data_raw/",x))
  
  date_today <- Sys.time()
  
  # get user account
  user <- getUser(x)
  tabular_user <- user$toDataFrame()
  
  # use user info to get followers
  followers <- user$getFollowers(retryOnRateLimit=180)
  tabular_followers <- followers$toDataFrame()
  tabular_followers$user_is <- x
  
  # use user info to get following
  following <- user$getFriends(retryOnRateLimit=180)
  tabular_following <- following$toDataFrame()
  tabular_following$user_is <- x
  
  # use user info to get tweets
  timeLine <- userTimeline(x)
  tabular_tweets <- timeLine$toDataFrame()
  tabular_tweets$user_is <- x
  
}
