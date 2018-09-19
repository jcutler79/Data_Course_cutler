###############################
########## SAS CLASS ##########
###############################

### text data
getwd()
lec2text = read.csv("/Users/jamescutler/Desktop/lec2text.csv", header = FALSE) # header needs to be false here cuz there is no header
colnames(lec2text) = c("id","year3","year4","board_exam","pass_board")


ggplot() +
  geom_point(data = lec2text, mapping = aes(x = id, y = year3)) +
  geom_point(data = lec2text, mapping = aes(x = id, y = year4), col = "red", alpha = .3) + 
  geom_point(data = lec2text, mapping = aes(x = id, y = board_exam), col = "blue", alpha = .3) +
  ylab("years 3, 4 and board exam (black, red, and blue)")

length(which(lec2text$year3 > 50)) # black
length(which(lec2text$year4 > 50)) # red
length(which(lec2text$board_exam > 50)) # blue
length(which(lec2text$year4 < 50)) # red
length(which(lec2text$board_exam < 50)) # blue

mean(lec2text$year3)
mean(lec2text$year4)
mean(lec2text$board_exam)
nrow(lec2text)
t.test(lec2text$year3,lec2text$year4)


#####################################################################################

# Week 3

# dem = read.csv("/Users/jamescutler/Desktop/Lecture 3 demographic data.csv") # I GOT RID OF THIS DATASET CUZ IT'S BORING
colnames(dem)
length(which(dem$gender == "F"))
length(which(dem$gender == "M"))
Fdem = dem[which(dem$gender == "F"),]
unique(dem$ethnic_group)

length(which(dem$ethnic_group == 2)) + length(which(dem$ethnic_group == 1))
nrow(dem) - (length(which(dem$ethnic_group == 2)) + length(which(dem$ethnic_group == 1)))
length(which(is.na(dem$ethnic_group)))


theNAs = data.frame(NAs = which(is.na(dem$ethnic_group)), 
                    Y = rep(1,length(which(is.na(dem$ethnic_group)))))
ggplot(theNAs, aes(NAs,Y)) +
  geom_point(col = "red", alpha = .3)

df = data.frame(weight = sample(100:300,10), height = sample(60:72,10))
sort(df$weight)
df$names = c("Aaron","Alice","Bobby","Cassandra","Denice","Fanny","Godwin","Harry","James","Paul")

df[order(df$weight),]
df[order(df$height),]
hist(df$weight)
hist(df$height)
ggplot(df, aes(weight,height)) + 
  geom_point() +
  geom_text(data = df, mapping = aes(weight,height, label = names), 
            size = 3, angle = 45, vjust = -.1, hjust = -.1) + 
  coord_cartesian(xlim = c(100,310),ylim = c(60,74)) +
  scale_y_continuous(breaks = seq(60,74,2))
  


###########################################################################################

# Week 4

mcat = read.csv("/Users/jamescutler/Desktop/SAS/mcat.csv")

tapply(mcat$average_mcat_score, mcat$gender, mean)
tapply(mcat$ms_gpa, mcat$gender, mean)
mcat.gpa = mcat[-which(is.na(mcat$ms_gpa)),]
tapply(mcat.gpa$ms_gpa, mcat.gpa$gender, mean)

tapply(mcat$age, mcat$gender, median)
hist(mcat[which(mcat$gender == "Male"),8], 
     main = "Ages of males and females \n(females in red)", 
     xlab = "age")
hist(mcat[which(mcat$gender == "Female"),8], col = "red", add = TRUE)
length(which(mcat$gender == "Male"))
length(which(mcat$gender == "Female"))/length(which(mcat$gender == "Male"))



