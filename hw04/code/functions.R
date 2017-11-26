## Functions to be Used

# This script prepares the functions to be used in this homework.

## Function remove_missing()

# This function takes a vector as an argument and returns the input vector without
# the missing values.

remove_missing <- function(vector) {
  vector <- vector[!is.na(vector)]
  return(vector)
}

## Function get_minimum()

# This function takes a numeric vector as an argument and an optional logical na.rm
# argument. Its output is the minimum value of the vector. If na.rm is true,
# the function will remove the missing values from the vector.

get_minimum <- function(vector, na.rm) {
  if (is.numeric(vector) == TRUE) {
  if(missing(na.rm)) {
    vector <- sort(vector)
    vector[1]
  } else {
    vector <- remove_missing(vector)
    vector <- sort(vector)
    vector[1]
  } } else {
    print("non-numeric argument")
  }
  
}

## Function get_maximum()

# This function takes a numeric vector as an argument and an optional logical na.rm
# argument. Its output is the maximum value of the vector. If na.rm is true,
# the function will remove the missing values from the vector.

get_maximum <- function(vector, na.rm) {
  if (is.numeric(vector) == TRUE) {
    if(missing(na.rm)) {
      vector <- sort(vector, decreasing = TRUE)
      vector[1]
    } else {
      vector <- remove_missing(vector)
      vector <- sort(vector, decreasing = TRUE)
      vector[1]
    } } else {
      print("non-numeric argument")
    }
}

## Function get_range()

# This function takes a numeric vector as an argument and an optional logical na.rm
# argument. Its output is the range of the vector. If na.rm is true, the function
# will remove the missing values from the vector.

get_range <- function(vector, na.rm) {
  if (is.numeric(vector) == TRUE) {
    if(missing(na.rm)) {
      get_maximum(vector) - get_minimum(vector)
    } else {
      get_maximum(vector) - get_minimum(vector)
    } } else {
      print("non-numeric argument")
    }
}

## Function get_percentile10()

# This function takes a numeric vector as an argument and an optional logical na.rm
# argument. Its output is the 10th percentile of the input vector. If na.rm is true,
# the function will remove the missing values of the vector.

get_percentile10 <- function(vector, na.rm) {
  if (is.numeric(vector) == TRUE) {
    if(missing(na.rm)) {
      quantile(vector, c(.1)) 
    } else {
      vector <- remove_missing(vector)
      quantile(vector, c(.1))
    } } else {
      print("non-numeric argument")
    }
}

## Function get_percentile90()

# This function takes a numeric vector as an argument and an optional logical na.rm
# argument. Its output is the 90th percentile of the input vector. If na.rm is true,
# the function will remove the missing values of the vector.

get_percentile90 <- function(vector, na.rm) {
  if (is.numeric(vector) == TRUE) {
    if(missing(na.rm)) {
      quantile(vector, c(.9)) 
    } else {
      vector <- remove_missing(vector)
      quantile(vector, c(.9))
    } } else {
      print("non-numeric argument")
    }
}

## Function get_median()

# This function takes a numeric vector as an argument and an optional logical na.rm
# argument. Its output is the median of the input vector. If na.rm is true, the 
# function will remove the missing values of the vector.

get_median <- function(vector, na.rm) {
  if(is.numeric(vector) == TRUE) {
    if(missing(na.rm)) {
      vector <- sort(vector)
      if (length(vector) %% 2 == 1) {
        vector[(length(vector) + 1) / 2]
      } else {
        mean(c(vector[length(vector) / 2], vector[length(vector) / 2 + 1])) 
      }
    } else {
      vector <- remove_missing(vector)
      vector <- sort(vector)
      if (length(vector) %% 2 == 1) {
        vector[(length(vector) + 1) / 2]
      } else {
        mean(c(vector[length(vector) / 2], vector[length(vector) / 2 + 1]))
      } } } else {
      print("non-numeric argument")
    }
} 

## Function get_average()

# This function takes a numeric vector as an argument and an optional logical na.rm
# argument. Its output is the numerical average of the input vector. If na.rm is 
# true, the function will remove the missing values of the vector.

get_average <- function(vector, na.rm) {
  if(is.numeric(vector) == TRUE) {
    if(missing(na.rm)) {
      empty <- 0
      for (j in vector) {
        empty <- empty + j
      }
      empty / length(vector)
    }
    else {
      vector <- remove_missing(vector)
      empties <- 0
      for (j in vector) {
        empties <- empties + j
      }
      empties / length(vector)
    } } else {
      print("non-numeric argument")
    }
}

## Function get_stdev()

# This function takes a numeric vector as an argument and an optional logical na.rm
# argument. Its output is the numerical standard deviation of the input vector. If
# na.rm is true, the function will remove the missing values of the vector.

get_stdev <- function(vector, na.rm) {
  if(is.numeric(vector) == TRUE) {
    if(missing(na.rm)) {
      avg <- get_average(vector)
      empty <- 0
      for (j in vector) {
        empty <- empty + (j - avg) ^ 2
      }
      sqrt(empty / (length(vector) - 1))
    } else {
      vector <- remove_missing(vector)
      avg <- get_average(vector)
      empty <- 0
      for (j in vector) {
        empty <- empty + (j - avg) ^ 2
      }
      sqrt(empty / (length(vector) - 1))
    } } else {
      print("non-numeric argument")
    }
}

## Function get_quartile1()

# This function takes a numeric vector as an argument and an optional logical na.rm
# argument. Its output is the first quartile of the input vector. If na.rm is true
# the function will remove the missing values of the vector.

get_quartile1 <- function(vector, na.rm) {
  if(is.numeric(vector) == TRUE) {
    if(missing(na.rm)) {
      quantile(vector, .25)
    }
    else {
      vector <- remove_missing(vector)
      quantile(vector, .25)
    } } else {
      print("non-numeric argument")
    }
}

## Function get_quartile3()

# This function takes a numeric vector as an argument and an optional logical na.rm
# argument. Its output is the third quartile of the input vector. If na.rm is true
# the function will remove the missing values of the vector.

get_quartile3 <- function(vector, na.rm) {
  if(is.numeric(vector) == TRUE) {
    if(missing(na.rm)) {
      quantile(vector, .75)
    }
    else {
      vector <- remove_missing(vector)
      quantile(vector, .75)
    } } else {
      print("non-numeric argument")
    }
}

## Function count_missing()

# This function takes a numeric vector as an argument and returns the number
# of missing values NA in the input vector.

count_missing <- function(vector) {
  length(vector[is.na(vector)])
}

## Function summary_stats()

# This function takes a numeric vector as an argument and returns the minimum,
# tenth percentile, first quartile, median, mean, third quartile, ninetieth 
# percentile, maximum, range, standard deviation, and number missing from the 
# input vector.

summary_stats <- function(vector, na.rm) {
  if (missing(na.rm)) {
  x <- list(get_minimum(vector), get_percentile10(vector), get_percentile10(vector),
            get_quartile1(vector), get_median(vector), get_average(vector),
            get_quartile3(vector), get_percentile90(vector), get_maximum(vector),
            get_range(vector), get_stdev(vector), count_missing(vector))
  names(x) <- c("minimum", "percent10", "quartile1", "median", "mean",
                "quartile3", "percent90", "maximum", "range", "stdev", "missing")
  x
  } else {
    vec <- remove_missing(vector)
    x <- list(get_minimum(vec), get_percentile10(vec), get_percentile10(vec),
              get_quartile1(vec), get_median(vec), get_average(vec),
              get_quartile3(vec), get_percentile90(vec), get_maximum(vec),
              get_range(vec), get_stdev(vec), count_missing(vector))
    names(x) <- c("minimum", "percent10", "quartile1", "median", "mean",
                  "quartile3", "percent90", "maximum", "range", "stdev", "missing")
    x
  }
}

## Function print_stats()

# This function takes a numeric vector as an argument and returns the minimum,
# tenth percentile, first quartile, median, mean, third quartile, ninetieth
# percentile, maximum, range, standard deviation, and number missing from the 
# input vector.

print_stats <- function(vecto, na.rm) {
  if (missing(na.rm)) {
    x <- list(str_pad(paste('minimum', format(round(get_minimum(vecto), 4), nsmall = 4), sep = ' : '), 15),
    str_pad(paste('percent10', format(round(get_percentile10(vecto), 4), nsmall = 4), sep = ' : '), 15),
    str_pad(paste('quartile1', format(round(get_quartile1(vecto), 4), nsmall = 4), sep = ' : '), 15),
    str_pad(paste('median', format(round(get_median(vecto), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('mean', format(round(get_average(vecto), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('quartile3', format(round(get_quartile3(vecto), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('percent90', format(round(get_percentile90(vecto), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('maximum', format(round(get_maximum(vecto), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('range', format(round(get_range(vecto), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('stdvev', format(round(get_stdev(vecto), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('missing', format(round(count_missing(vecto), 4), nsmall = 4), sep = " : "), 15))
    x
  } else {
    vec <- remove_missing(vecto) 
    x <- list(str_pad(paste('minimum', format(round(get_minimum(vec), 4), nsmall = 4), sep = ' : '), 15),
    str_pad(paste('percent10', format(round(get_percentile10(vec), 4), nsmall = 4), sep = ' : '), 15),
    str_pad(paste('quartile1', format(round(get_quartile1(vec), 4), nsmall = 4), sep = ' : '), 15),
    str_pad(paste('median', format(round(get_median(vec), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('mean', format(round(get_average(vec), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('quartile3', format(round(get_quartile3(vec), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('percent90', format(round(get_percentile90(vec), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('maximum', format(round(get_maximum(vec), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('range', format(round(get_range(vec), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('stdvev', format(round(get_stdev(vec), 4), nsmall = 4), sep = " : "), 15),
    str_pad(paste('missing', format(round(count_missing(vecto), 4), nsmall = 4), sep = " : "), 15)) 
    x
  }
}

## Function rescale100()
# This function takes the input of a numeric vector, a minimum xmin, and a 
# maximum value xmax. It returns a rescaled vector with a potential scale
# from 0 to 100. 

rescale100 <- function(vecto, xmin, xmax, na.rm) {
  if (missing(na.rm)) {
    for (j in vecto) {
      vecto <- replace(vecto, vecto == j, 100 * (j - xmin) / (xmax - xmin))
    }
    vecto
  } else {
    vec <- remove_missing(vecto)
    for (j in vec) {
      vec <- replace(vec, vec == j, 100 * (j - xmin) / (xmax - xmin))
    }
    vec
  }
}

## Function drop_lowest()
# This function takes the input of a numeric vector of length n and returns a 
# numeric vector of length n - 1 with the lowest value dropped. 

drop_lowest <- function(vecto, na.rm) {
  if (missing(na.rm)) {
    vecto <- vecto[-c(which.min(vecto))]
    vecto
  } else {
    vec <- remove_missing(vecto)
    vec <- vec[-c(which.min(vecto))]
    vec
  }
}

## Function score_homework()
# This function takes the input of a numeric vector of homework scores of length
# n and an optional logical argument drop. This function returns the average
# score. If drop = TRUE, the lowest homework score will be dropped. 

score_homework <- function(hws, drop) {
  if (missing(drop)) {
    get_average(hws)
  } else {
    hw <- drop_lowest(hws)
    get_average(hw)
  }
}

## Function score_quiz()
# This function takes the input of a numeric vector of quiz scores of length
# n and an optional logical argument drop. This function returns the average
# score. If drop = TRUE, the lowest quiz score will be dropped.

score_quiz <- function(quiz, drop) {
  if (missing(drop)) {
    get_average(quiz)
  } else {
    q <- drop_lowest(quiz)
    get_average(q)
  }
}

## Function score_lab()
# This function takes the input of a numerical value of lab attendance and returns
# the lab score. The attendance value will range from 0 to 12.

score_lab <- function(value) {
  if (value >= 11) {
    return(100)
  }
  else if (value == 10) {
    return(80)
  }
  else if (value == 9) {
    return(60)
  }
  else if (value == 8) {
    return(40)
  }
  else if (value == 7) {
    return(20)
  }
  else {
    return(0)
  }
}

