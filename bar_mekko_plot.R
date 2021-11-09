
x <- c(1200,700,400)
y <-c(.20,.35,.21)

df = data.frame(x = x
               , y = y)


barmekko <- function(df) {

  colors <-c("cadetblue","dodgerblue1","darkgrey","orange","blue")

  df$xa = cumsum(df$x)
  df$xl <- lag(df$xa,k=1)
  df$xl <- ifelse(is.na(df$xl),0,df$xl)
  df$xl <- df$xl + (df$x/2)
  df$yl <- min(df$y/2)
  df$mrate <- round(sum(df$x*df$y)/sum(df$x),2)
  df$c <- colors[1:nrow(df)]

  barplot(df$y
        , df$x
        , col=df$c
        , space = 0
        , ylim = c(0,max(df$y*1.1))
        , las=1
        #, main="Title"
        )

    axis(side = 1, at = c(0,df$xa,max(df$xa*1.1)))
    text(df$xl,df$yl,df$x)
    strh <- strheight('W')
    text(df$xl,df$y+strh,df$y)

    lines(c(0,df$xa*1.1)
      , c(min(df$mrate),df$mrate)
      , lty=c(1)
      , type="l"
      , col="red",
      lwd = 3)

    text(strwidth(max(df$mrate)),max(df$mrate)+strh,max(df$mrate),col="red")

}

barmekko(df)
