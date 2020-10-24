suppressMessages(suppressWarnings(library(tidyverse, warn.conflicts = F, quietly = T)))
suppressMessages(suppressWarnings(library(lubridate, warn.conflicts = F, quietly = T)))
suppressMessages(suppressWarnings(library(tcltk, warn.conflicts = F, quietly = T)))
suppressMessages(suppressWarnings(library(scales, warn.conflicts = F, quietly = T)))
suppressMessages(suppressWarnings(library(hms, warn.conflicts = F, quietly = T)))
suppressMessages(suppressWarnings(library(chron, warn.conflicts = F, quietly = T)))
suppressMessages(suppressWarnings(library(zoo, warn.conflicts = F, quietly = T)))

today <- substr(now() - lubridate::hours(4), 1, 10)

dat <- read.delim(paste0('~/timetracker/log/', today), 
                  header=F, col.names=c('time', 'window'),
                  stringsAsFactors=F, quote='')


#Get rid of second bc we dont need that precision
dat$time <- as_hms(paste0(substr(dat$time, 1, 5), ':00'))

range <- seq(as.numeric(min(dat$time)), 
             as.numeric(max(dat$time)),
             by=60) %>%
  as_hms

dat <- merge(dat,
             data.frame(time=range), 
             all.y=T)

dat$time <- as_hms(paste0(substr(dat$time, 1, 5), ":00"))

workwords <- c("Slack", "@", "pdf", "Stack", "Overleaf", "Zoom", "Ask Ubuntu", 
               'Editorial Manager', 'LibreOffice', "Google Drive", "Google Slides",
               'SpringerLink', 'ScienceDirect', 'PNAS', 'PLOS', 'R ', 'Nature', 'mcooper',
               'pgAdmin', 'AWS', 'S3', 'Vim', 'ubuntu', 'ssh', 'sql', 'zoom', 'Harvard',
               'Python', 'Athena', 'Amazon', 'Google Trends', "DHS", "Lancet", 
               'Google Docs', 'Google Search', 'GLIDE', 'PostgreSQL', 'Meet - ', 'tmux',
               'nvim')

emailwords <- c("Gmail", "Outlook", "University of Maryland Mail")

slackwords <- c("The New York Times", "Spotify", "The Atlantic", "reddit", "Twitter", 
                "Site Blocked", 'YouTube', 'craigslist', 'dataisbeautiful', 
                "TheMotte", "AskHistorians", "Amazon.com", 'AskReddit', 'Car Rental',
                'Enterprise', 'Google Maps', 'Wikipedia', 'ContraPoints')


dat <- dat %>%
  mutate(task = case_when(is.na(window) ~ "Idle/Break",
                          window == '' ~ "Idle/Break",
                          grepl(paste(workwords, collapse='|'), window) ~ "Work",
                          grepl(paste(emailwords, collapse='|'), window) ~ "Email",
                          grepl(paste(slackwords, collapse='|'), window) ~ "Slack",
                          TRUE ~ "Unknown"),
         task = na.locf(task, na.rm=F, maxgap=1))

dat$y <- 1


dat <- dat[!duplicated(dat$time), ]

t <- data.frame(table(dat$task))
t$hours <- as_hms(t$Freq*60)

str <- paste0(paste0(t$Var1, ': ', t$hours), collapse=' - ')

X11(width=18, height=4)
ggplot(dat) + 
  geom_bar(aes(x=time, y=y, fill=task), stat='identity', width=60) +
  scale_x_time(expand=c(0,0), breaks=breaks_width('1 hour')) + 
  scale_y_continuous(expand=c(0,0))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  labs(x=today,
       title=str) + 
  scale_fill_manual(values=c('Email'='#377eb8',
                             'Work'='#4daf4a',
                             'Slack'='#e41a1c',
                             'Idle/Break'='#dddddd',
                             'Unknown'='#333333'))
                    
unknowns <- dat$window[dat$task=='Unknown']
print(unknowns)

capture <- tk_messageBox(message="hit spacebar to close plots")
