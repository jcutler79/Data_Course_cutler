######## Pew - Muslim populations by country; want sharia law as official law of country


# FOR THE SHARIA LAW SUPPORT: http://www.pewresearch.org/fact-tank/2017/08/09/muslims-and-islam-key-findings-in-the-u-s-and-around-the-world/
# FOR THE POPULATION NUMBERS (2010 DATA): https://web.archive.org/web/20110202125134/http://features.pewforum.org:80/muslim-population/?sort=Pop2010
M = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/Pew_Muslims_pop_2010.csv")
colnames(M) = c("Country","90Mpop","90percM","10Mpop","10percM","30Mpop","30percM")
class(M$`10Mpop`)
class(M$`10percM`)
M$`10Mpop` = as.character(M$`10Mpop`)
M$`10Mpop` = gsub(",","",M$`10Mpop`) # IT WORKS!!!
M$`10Mpop` = as.numeric(M$`10Mpop`) # IT WORKS!!!
sum(M$`10Mpop`)
sharia = c("Russia","Kosovo","Albania","Kyrgyzstan","Tajikistan","Turkey","Kazakhstan","Azerbaijan","Malaysia",
           "Thailand","Indonesia","Afghanistan","Pakistan","Bangladesh","Iraq","Palestinian Territories",
           "Morocco","Egypt","Jordan","Tunisia","Lebanon","Niger","Djibouti","DR Congo","Nigeria",
           "Uganda","Ethiopia","Mozambique","Kenya","Mali","Ghana","Senegal","Cameroon","Liberia",
           "Chad","Guinea Bissau","Tanzania")
M[which(M$Country %in% sharia),1]
length(which(M$Country %in% sharia))
sharia.perc = c(.42,.2,.12,.35,.27,.12,.1,.08,.86,.77,.72,.99,.84,.82,.91,.83,.74,.71,.56,.29,
                .86,.71,.66,.65,.65,.64,.63,.58,.55,.53,.47,.37)
length(sharia.perc)
length(sharia)
cbind(sharia.perc,sharia)
sort(sharia)
sort(M[which(M$Country %in% sharia),1])
not.in.pop = c("Djibouti","DR Congo","Guinea Bissau","Liberia","Palestinian Territories")
sharia = sharia[-not.in.pop]
class(sharia)
length(sharia)
sharia %in% not.in.pop
sharia = sharia[! sharia %in% not.in.pop]
length(sharia)
length(sharia.perc)
cbind(sharia.perc,sharia)

m.s = M[which(M$Country %in% sharia),c(1,4)]
s.32 = as.data.frame(cbind(sharia.perc,sharia))
arrange(s.32, sort(sharia))
s.32 = s.32[order(s.32[,'sharia']),]
m.s = m.s[order(m.s[,'Country']),]
m.s$perc.sharia = s.32$sharia.perc
sum(m.s$`10Mpop`)
class(m.s$`10Mpop`)
class(m.s$perc.sharia)
m.s$perc.sharia = as.character(m.s$perc.sharia)
m.s$perc.sharia = as.numeric(m.s$perc.sharia)
class(m.s$perc.sharia)
m.s$pop.sharia = m.s$`10Mpop`*m.s$perc.sharia
sum(m.s$pop.sharia)/sum(m.s$`10Mpop`)
