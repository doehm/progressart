#' @title Progress icon type
#' @description Adds fun icons to your progress bar. A watched pot neer boils, ergo progress bars can feel like they take forever. May as well have some fun with it.
#' @param icon The icon type you wish to use. See details
#' @param length The total number of iterations in the loop.
#' @param width Width of the bar. Default is width of console - 20 to allow room for other counters.
#' @details Icon types: \code{fish}, \code{tiefighter}, \code{dagger}, \code{greatsword}
#' @examples
#' \dontrun{
#'  library(progress)
#'  library(stringr)
#'  library(crayon)
#'  n <- 500
#'  bar_fmt <- ":elapsedfull | :fish |"
#'  pb <- progress_bar$new(format = bar_fmt, total = n, clear = FALSE)
#'  fish <- progress_bar_icon("fish", n)
#'  for(j in 1:n){
#'    pb$tick(tokens = list(
#'      fish = token(fish, j)
#'    ))
#'    Sys.sleep(0.03)
#'  }
#' }
#'
#'  bar_fmt <- ":elapsedfull | :tie |"
#'  pb <- progress_bar$new(format = bar_fmt, total = n, clear = FALSE)
#'  tie <- progress_bar_icon("tie", n)
#'  for(j in 1:n){
#'    pb$tick(tokens = list(
#'      tie = token(tie, j)
#'    ))
#'    Sys.sleep(0.03)
#'  }
#' }
#'
#' @rdname progress_bar_icon
#' @seealso [progress::progress_bar]
#' @export




progress_bar_icon <- function(icon, length, width = options()$width-20){

  icons <- list(
    fish = list(
      bigfish = "><(((('>",
      littlefish = "><>"
    ),
    dagger = "cxx|::::::>",
    greatsword = "}xxxxx[::::::::::::::::::::>",
    tiefighter = list(
      tie = "|-O-|",
      tieright = "/-O-/",
      tieleft = "\\-O-\\",
      leader = "(-O-)"
    )
  )

  special <- list(order = rep(0, length), pos = rep(0, length))
  for(k in 1:30){
    st <- sample(1:(length-9), 1)
    special$order[st:(st+8)] <- sort(rep(1:3, 3))
    special$pos[st:(st+8)] <- sample(5:(width-5), 1)
  }

  pbicon <- list(
    icon = icons[[icon]],
    steps = seq(1, 5*width, length = length),
    size = lapply(icons[[icon]], str_length),
    width = width,
    length = length,
    special = special
  )

  class(pbicon) <- append(class(pbicon), icon)

  return(pbicon)
}



