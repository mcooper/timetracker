library(tidyverse)
library(lubridate)
library(tcltk)
library(scales)
library(hms)
library(chron)
library(zoo)

today <- substr(now(), 1, 10)

dat <- read.delim(paste0('~/timetracker/log/', today), 
                  header=F, col.names=c('time', 'window'),
                  stringsAsFactors=F)


#Get rid of second bc we dont need that precision
dat$time <- substr(dat$time, 1, 5)

hours <- seq(as.numeric(substr(min(dat$time), 1, 2)),
             as.numeric(substr(max(dat$time), 1, 2)))

combs <- expand.grid(hours, substr(100:159, 2, 3))
range <- paste0(combs$Var1, ':', combs$Var2)
range <- range[range > min(dat$time) & range < max(dat$time)]

dat <- merge(dat,
             data.frame(time=range), 
             all.y=T)

dat$time <- as_hms(paste0(substr(dat$time, 1, 5), ":00"))

workwords <- c("Slack", "@", "pdf", "Stack", "Overleaf", "Gmail", "Zoom", "Ask Ubuntu", 
               'Editorial Manager', 'LibreOffice')

emailwords <- c("Gmail", "Outlook")

slackwords <- c("The New York Times", "Spotify", "The Atlantic", "reddit", "Twitter", 
                "Site Blocked", 'YouTube')


dat <- dat %>%
  mutate(task = case_when(is.na(window) ~ "Idle/Break",
                          grepl(paste(workwords, collapse='|'), window) ~ "Work",
                          grepl(paste(emailwords, collapse='|'), window) ~ "Email",
                          grepl(paste(slackwords, collapse='|'), window) ~ "Slack"),
         task = na.locf(task, na.rm=F, maxgap=2))

dat$y <- 1


dat <- dat[!duplicated(dat$time), ]

t <- data.frame(table(dat$task, useNA='always'))
t$hours <- as_hms(t$Freq*60)

str <- paste0(paste0(t$Var1, ': ', t$hours), collapse=' - ')

X11(width=24, height=4)
ggplot(dat) + 
  geom_bar(aes(x=time, y=y, fill=task), stat='identity', width=60) +
  scale_x_time(expand=c(0,0), breaks=breaks_width('1 hour')) + 
  scale_y_continuous(expand=c(0,0))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  labs(x=today,
       title=str)

capture <- tk_messageBox(message="hit spacebar to close plots")
