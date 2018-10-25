# Data Method: Web data
# File: vosonTwitter01
# Example: Twitter using vosonSML

install.packages("vosonSML")
install.packages("magrittr")
library(dplyr)
library(vosonSML)

tic <- print(Sys.time()) 
tc_twitter <- Authenticate("twitter",
                        apiKey="yourkey",
                        apiSecret="yoursecret",
                        accessToken="youraccessToken",
                        accessTokenSecret="yourtokensecret") %>%
  Collect(searchTerm="Ted Cruz", numTweets=300, writeToFile=FALSE, verbose=TRUE)
toc <- print(Sys.time()) 
print(toc-tic)



