#' Get Meetup members' join date and other data, create animation

#' @return A tibble with the following columns:
#'    * id
#'    * name
#'    * description
#'    * created
#'    * updated
#'    * post_count
#'    * discussion_count
#'    * latest_reply_created
#'    * latest_reply_member_name
#'    * resource
#'
#'@examples
#' \dontrun{
#' meetupgrp_name <- "tripass"
#' join_dates     <- meetupxlanimate(meetupgrp_name)
#'}
#' @export

###########################################
##  Appears to be a need to install this ##
##  repo as of May, 2020                 ##
###########################################
# devtools::install_github('RickPack/meetupr')
# library(meetupr)


#' @export meetupxlanimate

###############################
## openxlsx helper functions ##
## in rExcelhelper           ##
###############################
source("R/rExcelhelper.R")

meetupxlanimate <- function(meetupgrp_name) {

    upcoming_events <- get_events(meetupgrp_name, "upcoming") %>%
        select(id, name, local_date)

    past_events <- get_events(meetupgrp_name, "past") %>%
      dplyr::filter(year(created) %in% c(2018, 2019, 2020)) %>%
      select(id, name, local_date)

    if (nrow(past_events[past_events$local_date >= Sys.Date(), ]) > 0) {
      message("get_events() with past argument sometimes returns an upcoming event")
      message("Now Removing:")
      print(past_events %>%
                dplyr::filter(local_date >= ymd(Sys.Date())))
      past_events <- past_events %>%
        dplyr::filter(local_date < ymd(Sys.Date()))
      message("")
      message("")
    }



    all_yesfrm <- data.frame()

    #############################################################################
    ###                     May explore in the future                         ###
    # if (nrow(upcoming_events) > 0 & nrow(past_events) > 0) {
    #     all_events <- rbind(upcoming_events, past_events)
    #  } else if (nrow(past_events) > 0 ) {
    #     message(str_glue("Downloading data for past events only. No upcoming ",
    #                      "events found."))
    #     all_events <- past_events
    #  } else if (nrow(upcoming_events) > 0) {
    #      message(str_glue("Downloading data for upcoming (scheduled for the future) ",
    #                       "events. No prior event found."))
    #      all_events <- upcoming_events
    #  } else {
    #      message("No Prior or upcoming events found. Terminating code")
    #  }
    #############################################################################

      for (m in 1:length(past_events$id)) {
        id_var     <- past_events$id[m]
        event_name <- past_events$name[m]
        event_date <- past_events$local_date[m]
        yesfrm <- get_event_attendees(meetupgrp_name, id_var) %>%
          dplyr::filter(rsvp_response == "yes") %>%
          mutate(rsvp_yes_row = 1)
        yesfrm$event_name <- event_name
        yesfrm$event_date <- event_date

        all_yesfrm <- bind_rows(all_yesfrm, yesfrm)
        # return data frame to assigned object
        invisible(all_yesfrm)
        if (m %% 5 == 0) {
          message("Pausing every 5 events to avoid Meetup API rate-limit")
          Sys.sleep(3)
        }
      }

    all_yesfrm2 <- all_yesfrm %>%
      dplyr::filter(name != "Former member") %>%
      distinct(id, event_name, event_date, .keep_all = TRUE) %>%
      group_by(name, bio) %>%
      mutate(events_attended_num = n()) %>%
      slice(which.max(event_date)) %>%
      ungroup() %>%
      rename(most_recent_event_name = event_name,
             most_recent_event_date = event_date) %>%
      select(name, bio, id, most_recent_event_name,
             most_recent_event_date, events_attended_num) %>%
      arrange(desc(events_attended_num))

    all_members  <- get_members(meetupgrp_name) %>%
      mutate(joined_date = as.Date(created, tz = "America/New_York")) %>%
      select(-created)
    all_members2  <- all_members %>%
      left_join(., all_yesfrm2 %>% select(id, most_recent_event_date, events_attended_num)) %>%
      select(name, bio, id, city, state, joined_date, most_recent_event_date, events_attended_num)

    summary_frm <- all_members2 %>%
      mutate(
        member_count = n(),
        mean_events_attended = mean(events_attended_num, na.rm = TRUE),
        median_events_attended = median(events_attended_num, na.rm = TRUE),
        mean_most_recent_event_date = mean(most_recent_event_date, na.rm = TRUE),
        median_most_recent_event_date = median(most_recent_event_date, na.rm = TRUE)
        ) %>%
      distinct(member_count, mean_events_attended, median_events_attended,
               mean_most_recent_event_date, median_most_recent_event_date)

    wb <- xlsxformat(wb, namxlsx = paste0(
                         "Meetup_Data_", meetupgrp_name),
                         wksht_name = "Attendees_since_2018",
                         df_inxlsx = all_yesfrm2, nxlsx = 1, max_nxlsx = 3)
    wb <- xlsxformat(wb, namxlsx = paste0(
                         "Meetup_Data_", meetupgrp_name),
                         wksht_name = "All_Members",
                         df_inxlsx = all_members2, nxlsx = 2, max_nxlsx = 3)
    wb <- xlsxformat(wb, namxlsx = paste0(
                         "Meetup_Data_", meetupgrp_name),
                         wksht_name = "Summary_Stats",
                         df_inxlsx = summary_frm, nxlsx = 3, max_nxlsx = 3)

    animate_frm <- all_members %>%
      # mutate(yearmon_var = tsibble::yearmonth(joined_date)) %>%
      mutate(yearmon_varc = zoo::as.yearmon(joined_date)) %>%
      mutate(yearmon_var  = zoo::as.Date(yearmon_varc)) %>%
      arrange(yearmon_var) %>%
      group_by(yearmon_var) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      mutate(CumSum := cumsum(Count))

    lims <- c(floor_date(min(animate_frm$yearmon_var),   "6 months"),
              ceiling_date(max(animate_frm$yearmon_var) + 90, "6 months"))

    animation_plot <-
      ggplot(animate_frm, aes(x = yearmon_var, y = CumSum)) +
        geom_line(color = "#ff0000", size = 3) +
        theme(
          plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 24),
          plot.caption = element_text(size = 12, face = "bold"),
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", size = 14),
          axis.text.y = element_text(face = "bold", size = 14)
        ) +
        geom_point() +
        transition_reveal(yearmon_varc) +
        coord_cartesian(clip = 'off') +
        # adjust the x axis breaks
        scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m",
                     limits = lims) +
        geom_text(aes(y = CumSum,
                      label = paste(" ", CumSum)), hjust = 0,
                  size = 10) +
        labs(
          title = paste0("Join date of ", meetupgrp_name, " Meetup members"),
          subtitle = "{frame_along}",
          y = "Member Count",
          caption = paste0("number of members as of ", Sys.Date(), " = ",
                           nrow(all_members))
          )

    anim_save(animation = animation_plot,
              filename = paste0(meetupgrp_name, "_anim_members.gif"),
              width=1140,height=828)
}
