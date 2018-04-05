################################################################

####### SAT by state with state expenditure on education ####### 

################################################################

mysat = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/cool_data_sets/SAT_by_state.csv")
?par
?order
?rbind
newsat = mysat[order(mysat$sat),]

par(mar = c(8,4,4,2) + .2)
barplot(newsat$sat,newsat$state, col = "grey50", main = "Average SAT scores by state",
        ylab = "SAT score", ylim = c(0,5+max(newsat$sat)), xlab = "", space = 1)
end_point = .5 + nrow(newsat) + nrow(newsat) - 1
text(seq(1.5, end_point, by=2), par("usr")[3]-.25, srt=60,adj=1,xpd=TRUE, 
labels = paste(rownames(newsat)), cex = .65)

barplot(newsat$sat, names.arg = newsat$state, las=2)

# SAT as function of teacher salary
plot(newsat$salary,newsat$sat)
sat.model = lm(newsat$sat ~ newsat$salary)
abline(sat.model)
cor(newsat$salary,newsat$sat) # poor but negative correlation of -.4399

# SAT as function of percentage of eligible students
plot(newsat$frac, newsat$sat)
reg.sat.frac = lm(newsat$sat ~ newsat$frac)
abline(reg.sat.frac)
cor(newsat$frac, newsat$sat) # AMAZING NEGATIVE correlation of -.887
# Fraction of eligible students taking it by state
par(mar = c(7,4,2,2))
barplot(newsat$frac, names.arg = newsat$state, las=2) # I GOT IT!! IT'S THE
# PROGRESSIVELY SMALLER FRACTION THAT'S DRIVING UP SCORES OF TOP-PERFORMING STATES.
## How to control for this? How can I rank states by their SAT scores AND fractions?
# I could just multiply the two column values--SAT and fraction:
newsat$sat.frac = newsat$sat*newsat$frac
# Now rank the states:
barplot(newsat$sat.frac, names.arg = newsat$state, las=2) # It's exactly the same as
# the fraction barplot. Literally.



# SAT as function of pupil-teacher ratio
plot(newsat$ratio,newsat$sat)
reg.sat.ratio = lm(newsat$sat ~ newsat$ratio)
abline(reg.sat.ratio)
cor(newsat$ratio,newsat$sat) # VIRTUALLY NO correlation: .081

# SAT as function of expenditure per student
plot(newsat$expend, newsat$sat)
reg.sat.expend = lm(newsat$sat ~ newsat$expend)
abline(reg.sat.expend)
cor(newsat$expend, newsat$sat) # poor but negative correlation of -.381