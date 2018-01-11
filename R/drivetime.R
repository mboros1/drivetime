#' Getting drive distance using Google Distance Matrix API
#'
#' This function gets the distance matrix between two geocodes
#'
#' @param from_lat is the starting latitude
#' @param from_lng is the starting longitude
#' @param to_lat is the ending latitude
#' @param to_lng is the ending longitude
#' @return data frame that contains the distance matrix data from Google's API
#' @author Martin Boros
#' @details
#' The function calls the distance matrix API from Google which returns a JSON containing the destination address,
#' the origin address, the distance in miles (character), distance in meters (numeric), the duration in hours and minutes(character),
#' the distance in minutes(numeric), and the call STATUS.
#' Can be used the same way as the 'mapdist' function in ggmap, but works better with geocodes since it does not require a reverse geocode
#' which often times fails.
#' @importFrom rjson fromJSON
#' @importFrom RCurl getURL
#' @export

get_distance <- function(from_lat,from_lng,to_lat,to_lng) {
  u = paste('https://maps.googleapis.com/maps/api/distancematrix/json?units=imperial&origins=',
            from_lat,',',from_lng,'&destinations=',
            to_lat,',',to_lng,
            sep='')
  r = getURL(u)
  j = fromJSON(r)
  return(data.frame(j))
}


#' Find point certain duration away in a straight line
#'
#' Function takes a starting point and end point to make a line, then finds
#' a point on that line that would take a certain amount of time to drive to.
#'
#' @param start starting point in the form of a geocode list, index 1 is longitude, index 2 is latitude
#' @param end end point in the form of a geocode list, index 1 is longitude, index 2 is latitude
#' @param duration drive duration we are looking to find, in minutes
#' @return geocode of the point that is approximately 'duration' minutes away
#' @author Martin Boros
#' @details
#' Creates a list of geocodes along a line between the start and end point, and uses a binary search
#' algorithm to find the point that is closest to the correct distance.
#' @importFrom geosphere gcIntermediate
#' @importFrom stats runif

calc_point_distance <- function(start, end, duration) {
  miles_to_meters = 1609.344
  n = 200
  min <- 0
  gci <- gcIntermediate(start,end, n = n)
  mid <- n/2
  max <- n
  counter = 0
  stuck_counter = 0
  while(counter <= 10) {
    counter = counter + 1
    if ((max-min) <= 1) {
      stuck_counter = stuck_counter + 1
    }
    else {
      stuck_counter = 0
    }
    if (stuck_counter >= 1) {
      min = min - (20+floor(runif(1,0,20)[1]))
      max = max + (20+floor(runif(1,0,20)[1]))
    }
    dir <- get_distance(start[2], start[1],gci[mid,2],gci[mid,1])
    if (dir$status == "ZERO_RESULTS") {
      max <- mid - 1
      mid <- ceiling((max + min)/2)
    } else if (dir$rows.elements.duration.value/60 < duration * .93) {
      min <- mid
      mid <- ceiling((max + min)/2)
    } else if (dir$rows.elements.duration.value/60 > duration * 1.07) {
      max <- mid
      mid <- ceiling((max + min)/2)
    } else {
      return(gci[mid,])
    }
  }
  return(gci[mid,])
}

#' Create dataframe of geocodes that can be used to map a drivetime isochrone
#'
#' Functions takes a geocode and desired driving duration to create a drivetime isochrone.
#'
#' @param lat latitude
#' @param lng longitude
#' @param duration driving distance
#' @param n number of points in the isochrone
#' @return data frame of geocodes for isochrone to be used in conjunction with a mapping package
#' @author Martin Boros
#' @details
#' Taking a geocode, this will create a drivetime isochrone. Fidelity increases as n increases, but
#' because the user has a limited number of calls to the distance matrix API daily and there are
#' roughly 3*n calls made to the API for each call of drive_time_points, one can quickly burn up
#' all of their daily calls by creating a few drivetime isochrones with a high n value.
#' Currently only supports minutes and miles, and the only valid input is a geocode.
#' @export
#' @importFrom geosphere destPoint

drive_time_points <- function(lat,lng, duration,n=60) {
  miles_to_meters = 1609.344
  angles = c()
  for (i in 0:(n-1)) {angles = append(angles,(360/n)*i)}
  max_distance = miles_to_meters * duration
  min_distance = max_distance*0.25
  mid_distance = max_distance*0.5
  circle_points_max = destPoint(c(lng,lat), b=angles, d=max_distance)
  for (i in 1:n) {
    end_pt <- calc_point_distance(c(lng,lat),circle_points_max[i,],duration)
    print(end_pt)
    circle_points_max[i,] <- end_pt
  }
  return(circle_points_max)
}
