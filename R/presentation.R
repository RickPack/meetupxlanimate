library(tidyverse)
library(lubridate)
library(timetk)
library(readr)

# much code borrowed from https://business-science.github.io/timetk/articles/TK04_Plotting_Time_Series.html
# Setup for the plotly charts (# FALSE returns ggplots)
interactive <- TRUE

rta_members <- readRDS("C:/Users/rick2/Downloads/R/meetupxlanimate/data-raw/RTA_meetupxl.rds")
tri_members_sum <- read_csv("C:/Users/rick2/Downloads/R/meetupxlanimate/data-raw/tripass_attendees.csv") %>%
    arrange(most_recent_event_date)%>%
    dplyr::filter(!is.na(most_recent_event_date))
tri_members     <- read_csv("C:/Users/rick2/Downloads/R/meetupxlanimate/data-raw/tripass_members.csv") %>%
    arrange(most_recent_event_date)%>%
    dplyr::filter(!is.na(most_recent_event_date))
tri_events  <- read_csv("C:/Users/rick2/Downloads/R/meetupxlanimate/data-raw/tripass_events_counted.csv") %>%
    arrange(event_date) %>%
    dplyr::filter(!is.na(event_date))

message("Events")

tri_events %>%
    plot_time_series(event_date, rsvp_yes_count,
                     .interactive = interactive,
                     .plotly_slider = TRUE)

data_sci_events <-
    tri_events %>% dplyr::filter(grepl("SCIENCE", toupper(event_name)))

tri_events2 <- tri_events %>%
    mutate(event_type = case_when(grepl("SCIENCE", toupper(event_name)) ~ "Data Science",
                                  grepl("SHOP", toupper(event_name)) ~ "Shop Talk",
                                  grepl("SATURDAY", toupper(event_name)) ~ "SQL SATURDAY",
                                  TRUE ~ "OTHER"))

tri_events2 %>%
    group_by(event_type) %>%
    plot_time_series(event_date, rsvp_yes_count,
                     .facet_ncol = 2, .facet_scales = "free",
                     .interactive = interactive)

message("Members - joined vs. attend")

tri_members_joined_sum <- tri_members %>%
    group_by(joined_date) %>%
    summarise(events_attended_num = sum(events_attended_num)) %>%
    mutate(joined_date = mdy(joined_date)) %>%
    arrange(joined_date)

tri_members_joined_sum %>%
    plot_time_series(joined_date, events_attended_num,
                     .interactive = interactive,
                     .plotly_slider = TRUE)
# anomalies
tri_members_joined_sum %>%
    plot_anomaly_diagnostics(joined_date, events_attended_num,
                             .facet_ncol = 2, .interactive = FALSE)

# Could examine days since start, can't correlate date with anything
# qplot(tri_members_joined_sum$joined_date, tri_members_joined_sum$events_attended_num)

tri_members_recent_sum <- tri_members %>%
    group_by(most_recent_event_date) %>%
    summarise(events_attended_num = sum(events_attended_num)) %>%
    mutate(most_recent_event_date = mdy(most_recent_event_date)) %>%
    arrange(most_recent_event_date) %>%
    dplyr::filter(!is.na(most_recent_event_date))

tri_members_recent_sum %>%
    plot_time_series(most_recent_event_date, events_attended_num,
                     .interactive = interactive,
                     .plotly_slider = TRUE)
