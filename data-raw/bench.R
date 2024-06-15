# https://gist.github.com/jangorecki/2d254c1451ec4b62c2fbb962a290b41b
# devtools::install_github("andrewuhl/RollingWindow")

test_file <- "bench"
test_width <- c(100, 1000)

a <- 1
width <- test_width[a]
file <- test_file[a]

df <- data.frame(
  "zoo::rollmeanr" = NA,
  "RcppRoll::roll_meanr" = NA,
  "RollingWindow::RollingMean" = NA,
  "fromo::running_mean" = NA,
  "roll::roll_mean*" = NA,
  "roll::roll_mean" = NA,
  "data.table::frollmean*" = NA,
  "data.table::frollmean" = NA,
  "roll::roll_lm*^" = NA,
  "roll::roll_lm^" = NA,
  "roll::roll_lm*" = NA,
  "roll::roll_lm" = NA
)

fn_map <- list(

  # single-thread
  "zoo::rollmeanr" = function() zoo::rollmeanr(data_table, width, fill = NA),
  "RcppRoll::roll_meanr" = function() RcppRoll::roll_meanr(data_matrix, width),
  "fromo::running_mean" = function() fromo::running_mean(data_matrix, width),
  "RollingWindow::RollingMean" = function() RollingWindow::RollingMean(data_table, width),
  "roll::roll_mean*" = function() roll::roll_mean(data_matrix, width),
  "data.table::frollmean*" = function() data.table::frollmean(data_table, width),
  # "RcppArmadillo::fastLmPure" = function() zoo::rollapplyr(data = lm_data, width = width,
  #                                                          x_cols = 1:2, y_cols = 3,
  #                                                          FUN = fastLm_coef, by.column = FALSE),
  "roll::roll_lm*^" = function() roll::roll_lm(data_matrix[ , 1], data_matrix[ , 2], width, online = FALSE),
  "roll::roll_lm*" = function() roll::roll_lm(data_matrix[ , 1], data_matrix[ , 2], width),

  # multi-thread
  "roll::roll_mean" = function() roll::roll_mean(data_matrix, width),
  "data.table::frollmean" = function() data.table::frollmean(data_table, width),
  "roll::roll_lm^" = function() roll::roll_lm(data_matrix[ , 1], data_matrix[ , 2], width, online = FALSE),
  "roll::roll_lm" = function() roll::roll_lm(data_matrix[ , 1], data_matrix[ , 2], width)

)

update_val <- function(val, col, df) {

  n_rows <- nrow(df)
  n_cols <- ncol(df)

  ix <- which(is.na(df[[col]]))[1]

  if (is.na(ix) || (ix > n_rows)) {

    row <- matrix(NA, nrow = 1, ncol = n_cols,
                  dimnames = list(NULL, names(df)))

    row[1, col] <- val
    df <- rbind(df, row)

  } else {

    df[ix, col] <- val

  }

  return(df)

}

update_df <- function(fn_map, col, n_threads) {

  data.table::setDTthreads(n_threads)
  RcppParallel::setThreadOptions(n_threads)

  filename <- paste0(file, "_", width)

  load(paste0(filename, ".rda"))
  df <- get(filename)

  time <- system.time(fn_map[[col]]())
  df <- update_val(time["elapsed"], col, df)

  save(list = eval(filename), file = paste0(filename, ".rda"))

}

check_df <- function(fn_map, col, n_threads) {

  status <- FALSE

  while (!status) {

    filename <- paste0(file, "_", width)

    load(paste0(filename, ".rda"))
    df <- get(filename)

    col_mu <- mean(df[[col]])
    col_sd <- sd(df[[col]])
    col_lower <- col_mu - 2 * col_sd
    col_upper <- col_mu + 2 * col_sd

    ix <- which((df[[col]] < col_lower) | (df[[col]] > col_upper))
    df[[col]][ix] <- NA

    status <- !any(is.na(df[[col]]))

    if (status) {

      save(list = eval(filename), file = paste0(filename, ".rda"))
      update_df(fn_map, col, n_threads)

    }

  }

}

# fastLm_coef <- function(data, x_cols, y_cols) {
#   return(coef(RcppArmadillo::fastLmPure(data[ , x_cols], data[ , y_cols])))
# }

set.seed(5640)
n <- 1000000
data_table <- data.table::setDT(lapply(1:1000, rnorm, n = n))
data_matrix <- as.matrix(data_table)
# lm_data <- cbind(1, data_matrix)

# single-thread
# update_df(fn_map, "zoo::rollmeanr", 1)
update_df(fn_map, "RcppRoll::roll_meanr", 1)
update_df(fn_map, "fromo::running_mean", 1)
update_df(fn_map, "RollingWindow::RollingMean", 1)
update_df(fn_map, "roll::roll_mean*", 1)
update_df(fn_map, "data.table::frollmean*", 1)
# update_df(fn_map, "RcppArmadillo::fastLmPure", 1)
update_df(fn_map, "roll::roll_lm*^", 1)
update_df(fn_map, "roll::roll_lm*", 1)
# system.time(fn_map[["data.table::frollmean*"]]())

# multi-thread
update_df(fn_map, "roll::roll_mean", RcppParallel::defaultNumThreads())
update_df(fn_map, "data.table::frollmean", RcppParallel::defaultNumThreads())
update_df(fn_map, "roll::roll_lm^", RcppParallel::defaultNumThreads())
update_df(fn_map, "roll::roll_lm", RcppParallel::defaultNumThreads())
# system.time(fn_map[["data.table::frollmean"]]())

check_df(fn_map, "roll::roll_lm", , RcppParallel::defaultNumThreads())
