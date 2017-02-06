# Functions
make_footnote <- function(footnoteText = date(),
                          size = .7, color = grey(.5),
                          position = c("right", "bottom")) {
  require(grid)
  
  # Adds footnote to bottom-right hand corner of image. 
  # Should be called immediately after ggplot object has been printed.
  # I stole this from the internet but cannot find/remember
  # the original source ¯\_(ツ)_/¯
  #
  # Args:
  #   footnoteText: The text to be added.
  #   size: The size of the text.
  #   color: Colour of the text.
  #
  # Returns:
  #   NA 
  
  pushViewport(viewport())
  grid.text(label = footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y = unit(2, "mm"),
            just = position,
            gp = gpar(cex = size, col = color))
  popViewport()
}