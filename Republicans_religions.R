### Republicans' ratings of religions

Repub = data.frame(religion = c("Evangelicals","Jews","Catholics","Mormons","Buddhists",
                                "Hindus","Atheists","Muslims"), 
                   ratings = c(71,67,66,52,49,47,34,33),
                   tiers = c(rep("warm",3),rep("medium",3),rep("cold",2)))
library(ggthemes)
ggplot(Repub, aes(x = reorder(religion, -ratings),y = ratings)) + 
  geom_bar(stat = "identity", fill = clrs) + 
  labs(title = "Republicans' ratings of religions", x = "", y = "percent") +
  coord_cartesian(ylim = c(0,100)) + theme_economist()


###

library(plyr)
clrs = as.character(mapvalues(Repub$tiers, from = c("warm","medium","cold"), 
                 to = c("red","green","blue")))
library(plotrix) # for this: do.first = expression(abline(h = seq(0,100,10))) - and it doesn't even work. At this point, it's better to just do a ggplot
par(mar = c(6,5,3,2))
barplot(Repub$ratings, names.arg = Repub$religion, las = 2, col = clrs,
        main = "Republicans' ratings of religions", ylab = "percent", ylim = c(0,100))
# abline(h = c(seq(0,100,10)), lty = 2, col = "gray")
legend("topright", inset = .02, c("warm","medium","cold"), 
       fill = c("red","green","blue"), horiz = TRUE, cex = .6)
# grid(NA, 10, lwd = 1)
# ticks = seq(0,100,10)
# axis(4, at = ticks, labels = ticks)






