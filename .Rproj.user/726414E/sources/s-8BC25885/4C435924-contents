#' @title Token builder
#' @description The token function build the string to pass to the progress bar object.
#' @param pbicon Progress bar icon object
#' @param j The current iteration
#' @details Each small fish represents 25\% of the progress. When they're all gobbled up by the big fish the job is done.
#' @examples
#' \dontrun{
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
#'
#'  n <- 500
#'  bar_fmt <- green$bold(":elapsedfull | :tie |")
#'  pb <- progress_bar$new(format = bar_fmt, total = n, clear = FALSE)
#'  tie <- progress_bar_icon("tiefighter", n, 60)
#'  for(j in 1:n){
#'    pb$tick(tokens = list(
#'      tie = token(tie, j)
#'    ))
#'    Sys.sleep(0.03)
#'  }
#' }
#' @rdname token
#' @importFrom stringr str_length str_sub
#' @importFrom progress progress_bar
#' @importFrom crayon green
#' @export


token <- function(pbicon, j){
  UseMethod("token", pbicon)
}

#' @title Fish progress bar icon
#' @rdname token
#' @importFrom stringr str_length str_sub str_sub<-
#' @export

token.fish <- function(pbicon, j){
  canvas <- paste0(rep(" ", pbicon$width), collapse = "")
  start <- j %% pbicon$width
  end <- min(start + pbicon$size$bigfish, pbicon$width)
  if(start < pbicon$width) str_sub(canvas, start, end) <- str_sub(pbicon$icon$bigfish, 1, end-start)

  # little fish
  nfish <- 4 - floor(j/pbicon$length*4)
  lil_fish <- paste0(rep(pbicon$icon$littlefish, nfish), collapse = " ")
  lil_end <- min(end + 5 + str_length(lil_fish), pbicon$width-1)
  if(end + 5 < pbicon$width) str_sub(canvas, end + 5, lil_end) <- str_sub(lil_fish, 1, lil_end - end - 5)

  return(str_sub(canvas, 1, pbicon$width-2))
}

#' @title Dagger progress bar icon
#' @details Each dagger box represents 20\% completion.
#' @rdname token
#' @importFrom stringr str_length str_sub str_sub<-
#' @export

token.dagger <- function(pbicon, j){

  canvas <- paste0(rep(" ", pbicon$width), collapse = "")
  start <- min(pbicon$steps[j] %% pbicon$width, pbicon$width - pbicon$size[[1]] - 18)
  end <- min(start + pbicon$size[[1]], pbicon$width - 18)
  if(start < pbicon$width-18) str_sub(canvas, start, end) <- str_sub(pbicon$icon[[1]], 1, end-start)

  nstrikes <- floor((pbicon$steps[j] + pbicon$size[[1]]*2 + 6)/pbicon$width)
  strikes <- paste0(c(rep("[X]", nstrikes), rep("[ ]", 5-nstrikes)), collapse = "")
  str_sub(canvas, pbicon$width-18, pbicon$width-2) <- strikes

  return(str_sub(canvas, 1, pbicon$width-4))
}


#' @title Tie Fighter progress bar icon
#' @details Each tie fighter represents 30\% completion.
#' @rdname token
#' @importFrom stringr str_length str_sub str_sub<-
#' @export

token.tiefighter <- function(pbicon, j){

  canvas <- paste0(rep(" ", pbicon$width), collapse = "")
  rat <- pbicon$steps[j]/max(pbicon$steps)
  a <- 10*(pbicon$length/pbicon$width)/4.23
  move <- sin(rat*a*pi)
  positions <- round(seq(pbicon$width/4, 3*pbicon$width/4, length = 3) + (pbicon$width/4-7)*move)

  if(cos(pbicon$steps[j]/max(pbicon$steps)*a*pi) > 0) {
    if(floor(3*rat) < 1) str_sub(canvas, positions[1]-2, positions[1]+2) <- pbicon$icon$tieright
    if(floor(3*rat) < 2) str_sub(canvas, positions[3]-2, positions[3]+2) <- pbicon$icon$tieright
  }else{
    if(floor(3*rat) < 1) str_sub(canvas, positions[1]-2, positions[1]+2) <- pbicon$icon$tieleft
    if(floor(3*rat) < 2) str_sub(canvas, positions[3]-2, positions[3]+2) <- pbicon$icon$tieleft
  }

  if(j != pbicon$length){
    str_sub(canvas, positions[2]-2, positions[2]+2) <- pbicon$icon$leader
  }else{
    str_sub(canvas, positions[2]-2, positions[2]+2) <- "(.o *"
  }

  debris <- c("0", "o", ".")
  if(pbicon$special$order[j] > 0){
    str_sub(canvas, pbicon$special$pos[j], pbicon$special$pos[j]) <- debris[pbicon$special$order[j]]
  }

  return(str_sub(canvas, 1, pbicon$width-3))
}

