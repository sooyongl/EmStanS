#' @include EmStanS-package.r
NULL

#' colour choices for plots
getColors <- function(pallete = "set2", rep_n = 1) {

  set1<- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6')

  set2 <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')

  pastel <- c('#eae4e9','#fff1e6','#fde2e4','#fad2e1','#e2ece9','#bee1e6','#f0efeb','#dfe7fd','#cddafd')

  pastel1 <- c("#FBEEE6","#ffc0cb","#c9ede7","#e3dcf1","#c0ffee","#FBEEE6","#F5B7B1","#D2B4DE","#AED6F1","#A3E4D7","#F9E79F","#FBEEE6","#ffc0cb","#c9ede7","#e3dcf1","#c0ffee","#FBEEE6","#F5B7B1","#D2B4DE","#AED6F1","#A3E4D7","#F9E79F")


  mypick1 <- c("#DDF8E8","#CDD5D1","#B4A6AB","#946E83","#615055")
  mypick2 <- c("#26547c","#ef476f","#ffd166","#06d6a0","#fcfcfc")


  rainbow <- c("#ffadad","#ffd6a5","#fdffb6","#caffbf","#9bf6ff","#a0c4ff","#bdb2ff","#ffc6ff","#fffffc")

  out <- switch(pallete,
    'set1' = set1,
    'set2' = set2,
    'pastel' = pastel,
    'pastel1' = pastel1,
    'mypick1' = mypick1,
    'mypick2' = mypick2,
    'rainbow' = rainbow
  )

  rep(out, rep_n)

}

#' global colour
colors <- getColors(pallete = "pastel", rep_n = 5)
