
FigureTest <- function(data){
  q <- ggplot(data, aes(x=x, y=y))
  q <- q + geom_point()
  q <- SMAOgraphs::SMAOFormatGGPlot(q)
  SMAOgraphs::SMAOpng("results/test.png")
  print(q)
  dev.off()
}
