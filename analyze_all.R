library(tidyverse)
library(lubridate)
library(tcltk)
library(scales)
library(hms)
library(chron)
library(zoo)

source('~/timetracker/codes.R')

setwd('~/gd/ttlog')

readf <- function(f){
  read.delim(f, 
             header=F, col.names=c('time', 'window'),
             stringsAsFactors=F, quote='', skipNul = TRUE) %>%
    filter(!is.na(time) & time != '') %>%
    mutate(date = f, time = substr(time, 1, 5))
}

dat <- list.files() %>%
  map_dfr(readf)

tseq <- seq(ymd_hms('01-01-01 00:00:00'),
            ymd_hms('01-01-01 23:59:00'), by='1 min') %>%
  format('%H:%M')
dseq <- as.character(seq(ymd(min(dat$date)), ymd(max(dat$date)), by='day'))

dt <- expand.grid(list(date=dseq, time=tseq))

full <- merge(dat, dt, all=T) %>%
  filter(!duplicated(paste0(time, date)))

full <- full %>%
  mutate(task = case_when(is.na(window) ~ "Idle/Break",
                          window == '' ~ "Idle/Break",
                          grepl(paste(workwords, collapse='|'), window) ~ "Work",
                          grepl(paste(emailwords, collapse='|'), window) ~ "Email/Meetings",
                          grepl(paste(slackwords, collapse='|'), window) ~ "Slack",
                          TRUE ~ "Unknown"),
         task = na.locf(task, na.rm=F, maxgap=1))

ggplot(full) + 
  geom_raster(aes(x=ymd(date), y=as_hms(paste0(time, ':00')), fill=task)) + 
  scale_y_time(expand=c(0,0), breaks=breaks_width('1 hour')) + 
  scale_x_date(expand=c(0,0), breaks=breaks_width('1 month'))+
  labs(x='Date', y='Time') + 
  scale_fill_manual(values=c('Email/Meetings'='#377eb8',
                             'Work'='#4daf4a',
                             'Slack'='#e41a1c',
                             'Idle/Break'='#dddddd',
                             'Unknown'='#333333'))

monthsum <- full %>%
  mutate(month = month.abb[as.numeric(substr(date, 6, 7))]) %>%
  group_by(month, task) %>%
  summarize(hours_per_day = (n()/length(unique(date)))/60) %>%
  filter(task != 'Idle/Break') %>%
  mutate(month = factor(month, levels=month.abb))

ggplot(monthsum) + 
  geom_bar(aes(x=month, y=hours_per_day, fill=task), stat='identity') + 
  scale_fill_manual(values=c('Email/Meetings'='#377eb8',
                             'Work'='#4daf4a',
                             'Slack'='#e41a1c',
                             'Unknown'='#333333')) + 
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  labs(y="Hours per Day (Incl. Weekends)",
       x="Month")

# Estimate fraction of unknown that is actuall work
# sample(full$window[full$task == 'Unknown'], 20)

daysum <- full %>%
  mutate(work = case_when(task == 'Work' ~ 1,
                          task == 'Email/Meetings' ~ 1,
                          task == 'Unknown' ~ (7/20),
                          task == 'Slack' ~ 0),
         slack = 1 - work) %>% 
  group_by(date) %>%
  summarize(work = sum(work, na.rm=T)/60,
            slack = sum(slack, na.rm=T)/60) %>%
  mutate(month = month.abb[as.numeric(substr(date, 6, 7))],
         month = factor(month, levels=month.abb),
         wday = wday(ymd(date)),
         wnum = ceiling(row_number()/7))

ggplot(daysum) + 
  geom_tile(aes(x=wnum, y=wday, fill=work, color=month), size=0.5)
  scale_y_continuous(expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))

