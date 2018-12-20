#### Different kinds of data visualizations:
# (alphabetical order)

### Table of Contents:
## Grouped barplots


### Grouped barplots 
## (for things like before and after, or two factors with one of each in the other, see example below)

# Whether you do grouped barplots obviously depends on what your data is like:
animals = data.frame(category = rep(c("declined","improved"),4),
                     reason = rep(c("genuine","misclassified","taxonomic","unclear"),each = 2),
                     species_n = c(24,16,41,85,2,7,41,117))

ggplot(animals, aes(factor(reason),species_n, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  
  ggtitle("number of species that declined or improved for various reasons") +
  xlab("reason for improvement or decline") + 
  ylab("number of species")

shayak = data.frame(part = rep(c(1,2),6),
                    difficulty = rep(c(0,1,2,3,4,6),each=2),
                    count = c(6,8,24,6,29,19,14,11,1,7,1,4))

ggplot(shayak, aes(factor(difficulty),count, fill = factor(part))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  
  ggtitle("number of problems in parts 1 & 2 at each difficulty level") +
  xlab("difficulty level") + 
  ylab("number of problems")
## Pretty straightforward. You just need to ...
# 1) set the "separate" categories as your x axis factor ['separate' is my terminology]
# 2) make your grouped category your fill and put it in the aesthetics
# 3) add a position = 'dodge'
# 4) and use a scale fill brewer with a palette of your choice



