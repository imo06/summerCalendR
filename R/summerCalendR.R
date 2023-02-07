#' Create Camp calendar
#' @details
#' Helps in the creation of a camp calendar that can be shared or printed.
#' It estimates the total cost using the inputs. It does not account for
#' regional holidays.

summerCampCalendR <- R6::R6Class(
  'summerCampCalendR',
  public = list(
    #' @description Initialize the class
    initialize = function(){
    },
    #' @description Add a camp and a cost
    #' @param ... a camp is put in via `Name` = `cost` arguments that are
    #' internally converted to a list and then a `data.table`. Note: Camps can
    #' be overriden in this manner.
    AddCampCost = function(...){
      args <- list(...)
      args.df <- args |>
        as.data.frame.list() |>
        data.table::as.data.table() |>
        data.table::melt.data.table(measure.vars = names(args), variable.name = 'Camp', value.name = 'Price')
      private$camps <- rbind(private$camps[!Camp %in% args.df$Camp], args.df)
      invisible(self)
    },
    #' @description View the camps you have input. You cannot adjust them here.
    ViewCamps = function(){
      copy(private$camps)
    },
    #' @description Create the calendar internally so you may add events
    #' @param start_date,end_date must be convertible to `date` class with
    #' `start_date` < `end_date`
    CreateCalendar = function(start_date, end_date){
      if(is.na(as.Date(start_date))){stop("start_date must be convertible to a date")}
      if(is.na(as.Date(end_date))){stop("end_date must be convertible to a date")}
      if(as.Date(start_date) > as.Date(end_date)){stop("start_date must be before end_date")}
      private$start_date <- start_date
      private$end_date <- end_date

      plt <- calendR::calendR(start_date = start_date, end_date = end_date)
      dates <- plt$data
      dates$ind <- 1:nrow(dates)
      private$calendar.context <- list(events = rep(NA, nrow(plt$data)), dates = dates)
      invisible(self)
    },
    #' @description Add the date of the summer camp
    #' @param ... input as `FirstDate` = `Camp`. The entire week will be filled.
    #' No half weeks allowed at the moment.
    AddSummerCampDates = function(...){
      argslist <- list(...)
      for(i in seq_along(argslist)){
        private$calendar.context  <-
          private$set.camp(private$calendar.context , argslist[[i]], names(argslist)[[i]])
      }
      invisible(self)
    },
    #' @description add the school dates
    #' @param start_date the first day of school
    AddSchool = function(start_date){
      private$calendar.context$events[subset(private$calendar.context$dates, weekend == 0 & date >= start_date)$ind] <- 'School'
      invisible(self)
    },
    #' @description Calculate the approximate price including taxes
    #' @param tax the amout of the tax rate. Defaults to `1`
    CalculatePrice = function(tax = 1){
      dates <- as.data.table(private$calendar.context$dates)
      dates$Camp <- private$calendar.context$events

      sum(merge(
        dates[weekend == 0, .SD[date == min(date)], by = .(woy)],
        camps,
        by = 'Camp'
      )$Price, na.rm = TRUE) * tax
    },
    #' @description get the dates provided in the creation of the calendar
    GetDates = function(){
      list(start_date = private$start_date, end_date = private$end_date)
    },
    #' @description Get a `data.table` with the dates
    GetEvents = function(){
      dates <- as.data.table(private$calendar.context$dates)
      dates$Camp <- private$calendar.context$events
      dates
    },
    #' @description get the first work day of the week
    GetFirstWorkDayOfWeek = function(){
      self$GetEvents()[dow >= 1, .SD[dow == min(dow)], by = .(year, woy)]
    },
    #' @description generate a `gg` object
    #' @param palette.name the name of the RColorBrewer palette. Defaults to 'Paired'
    #' @param addprice boolean
    Plot = function(palette.name = 'Paired', addprice = TRUE){
      private$calendar.context$events[subset(private$calendar.context$dates, weekend == 1)$ind] <- NA
      clr <- private$create.colours(private$calendar.context, palette.name = palette.name)
      plt <- calendR::calendR(
        start_date = private$start_date, end_date = private$end_date
        ,special.days = private$calendar.context$events
        ,special.col = clr
        ,legend.pos = 'top'
        ,title = 'Summer Camp'
        ,mbg.col = 4,               # Color of the background of the names of the months
        months.col = "white",      # Color text of the names of the months
        bg.col = "#f4f4f4",        # Background color
        title.size = 30,                       # Title size
        orientation = "p"         # Vertical orientation
      )
      plt + ggplot2::labs(subtitle = paste0("Total Camp Cost (excl. tax): C$ ", prettyNum(round(self$CalculatePrice(),0), big.mark = ',')))
    }
  ),
  active = list(
  ),
  private = list(
    start_date = NULL,
    end_date = NULL,
    camps = NULL,
    calendar.context = NULL,
    set.camp = function(context, monday.date, camp){
      id <- subset(context$dates, weekend == 0 & woy %in% subset(context$dates, date %in% as.Date(monday.date))$woy)$ind
      context$events[id] <- camp
      list(events = context$events, dates = context$dates)
    },
    create.colours = function(context, palette.name = 'Paired'){
      colrs <- RColorBrewer::brewer.pal(length(unique(na.omit(context$events))), name = palette.name)
      names(colrs) <- unique(na.omit(context$events))
      colrs[na.omit(names(colrs))]
    }
  )
)
