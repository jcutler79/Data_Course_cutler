### The flashcards notepad (apps and decks)

   ### The Flashcard functions ###

# variable lengths
# 3-sided
# 4-sided
# reverse 3
# reverse 4

### flashcards for many-sided cards (best for lists of variable lengths):
flashcards.many = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    if (length(inum.cum) == length(yourlist)-1){
      inum = mysamp[! mysamp %in% inum.cum]
      print(c(length(yourlist)-1, names(yourlist)[inum]))
      inum.cum = c(inum.cum,inum)
      print(inum.cum)
      Sys.sleep(7)
      for (j in 1:length(yourlist[[inum]])){
        print(yourlist[[inum]][j])
        Sys.sleep(10)
      }
      continue = ""
    } else{
      inum = sample(mysamp[-inum.cum],1)
      print(c(i, names(yourlist)[inum]))
      for (j in 1:length(yourlist[[inum]])){
        show = readline(prompt = "Hit 'l' to show next side: ")
        if (show == "l"){
          print(yourlist[[inum]][j])
        } else{print("you suck")}
      }
      inum.cum = c(inum.cum,inum)
      i = i+1
      continue = readline(prompt = "Hit 'y' to continue, enter to quit: ") 
    }
  }
  print("Peace.")
}

### flashcards for 3 sided-cards:
flashcards3 = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    if (length(inum.cum) == length(yourlist)-1){
      inum = mysamp[! mysamp %in% inum.cum]
      print(c(length(yourlist)-1, names(yourlist)[inum]))
      inum.cum = c(inum.cum,inum)
      print(inum.cum)
      Sys.sleep(10)
      print(yourlist[[inum]][1])
      Sys.sleep(5)
      print(yourlist[[inum]][2])
      continue = ""
    } else{
      inum = sample(mysamp[-inum.cum],1)
      print(c(i, names(yourlist)[inum]))
      show1 = readline(prompt = "Hit 'y' to show other side: ")
      if (show1 == "y"){
        print(yourlist[[inum]][1])
      } else{print("Alrighty, then.")}
      show2 = readline(prompt = "Hit 'y' to show the other other side: ")
      if (show2 == 'y'){
        print(yourlist[[inum]][2])
      } else{print("Uh ... WTF.")}
      inum.cum = c(inum.cum,inum)
      i = i+1
      continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
    }
  }
  print("Peace out.")
}
### flashcards for 4-sided cards:
flashcards4 = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    if (length(inum.cum) == length(yourlist)-1){
      inum = mysamp[! mysamp %in% inum.cum]
      print(c(length(yourlist)-1, names(yourlist)[inum]))
      inum.cum = c(inum.cum,inum)
      print(inum.cum)
      Sys.sleep(10)
      print(yourlist[[inum]][1])
      Sys.sleep(7)
      print(yourlist[[inum]][2])
      Sys.sleep(5)
      print(yourlist[[inum]][3])
      continue = ""
    } else{
      inum = sample(mysamp[-inum.cum],1)
      print(c(i, names(yourlist)[inum]))
      show1 = readline(prompt = "Hit 'y' to show other side: ")
      if (show1 == "y"){
        print(yourlist[[inum]][1])
      } else{print("Alrighty, then.")}
      show2 = readline(prompt = "Hit 'y' to show the other other side: ")
      if (show2 == 'y'){
        print(yourlist[[inum]][2])
      } else{print("Uh ... WTF.")}
      show3 = readline(prompt = "Hit 'y' to show the other other other side: ")
      if (show3 == 'y'){
        print(yourlist[[inum]][3])
      } else{print("F U too then.")}
      inum.cum = c(inum.cum,inum)
      i = i+1
      continue = readline(prompt = "Hit 'y' to continue, enter to quit: ") 
    }
  }
  print("Peace.")
}

### reverse flashcards for 3-sided cards:
flashback3 = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    if (length(inum.cum) == length(yourlist)-1){
      inum = mysamp[! mysamp %in% inum.cum]
      print(c(length(yourlist)-1, names(yourlist)[inum]))
      inum.cum = c(inum.cum,inum)
      print(inum.cum)
      Sys.sleep(10)
      print(yourlist[[inum]][2])
      Sys.sleep(7)
      print(yourlist[[inum]][1])
      continue = ""
    } else{
      inum = sample(mysamp[-inum.cum],1)
      print(c(i, yourlist[[inum]][2]))
      show1 = readline(prompt = "Hit 'y' to show other side: ")
      if (show1 == "y"){
        print(yourlist[[inum]][1])
      } else{print("Alrighty, then.")}
      show2 = readline(prompt = "Hit 'y' to show the other other side: ")
      if (show2 == 'y'){
        print(names(yourlist)[inum])
      } else{print("Uh ... WTF.")}
      inum.cum = c(inum.cum,inum)
      i = i+1
      continue = readline(prompt = "Hit 'y' to continue, enter to quit: ") 
    }
  }
  print("F U.")
}
### reverse flashcards for 4-sided cards:
flashback4 = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    if (length(inum.cum) == length(yourlist)-1){
      inum = mysamp[! mysamp %in% inum.cum]
      print(c(length(yourlist)-1, names(yourlist)[inum]))
      inum.cum = c(inum.cum,inum)
      print(inum.cum)
      Sys.sleep(10)
      print(yourlist[[inum]][3])
      Sys.sleep(7)
      print(yourlist[[inum]][2])
      Sys.sleep(5)
      print(yourlist[[inum]][1])
      continue = ""
    } else{
      inum = sample(mysamp[-inum.cum],1)
      print(c(i, yourlist[[inum]][3]))
      show1 = readline(prompt = "Hit 'y' to show other side: ")
      if (show1 == "y"){
        print(yourlist[[inum]][2])
      } else{print("Alrighty, then.")}
      show2 = readline(prompt = "Hit 'y' to show the other other side: ")
      if (show2 == 'y'){
        print(yourlist[[inum]][1])
      } else{print("Uh ... okay.")}
      show3 = readline(prompt = "Hit 'y' to show the other other other side (the front): ")
      if (show3 == 'y'){
        print(names(yourlist)[inum])
      } else{print("Then I'll see you in hell!")}
      inum.cum = c(inum.cum,inum)
      i = i+1
      continue = readline(prompt = "Hit 'y' to continue, enter to quit: ") 
    }
  }
  print("Have a nice day.")
}
############################################################################################

############################################################################################

                                   ### Flash cards ###

                                   ###################

## Ideas for future flashcard decks:
# The Enlightenment - the history, the philosophy, the people, the ideas, the values, the polemic (what were they reacting to or arguing against; i.e. not just what were they for, but what traditional ideas were they against)
# Socialism - socialism vs communism; empirical facts about socialist phenomena; misconceptions
# Capitalism - Chomsky facts about; history; ideas; values; misconceptions
# Political right vs left - what are the left wing ideas, phenomena, practices, etc.? what are the right wing ones? Have they ever changed? When did right and left emerge in political philosophy?

## SEPARATE FROM THE REST OF THE DECKS - USMLE step 1 (15 categories + high yield):
# biochem
# cardiology
# dermatology
# endocrinology
# gastroenterology
# hematology and oncology
# immunology
# microbiology
# musculoskeletal
# nehprology / genitourinary
# neurology
# pathology
# pharmacology
# psychiatry
# public health sciences
# pulmonology / respiratory
# reproduction
# high yield

## In alphabetical order by list (deck) name:
# ancient.names.3 (a list of ancient people(s) who were/are known by phonetically distinct names)
# BofM.EME.4 (Book of Mormon EME vocab passages)
# bythenumbers.3 (topic, number, source)
# climate.two.per.lecture (climate change Great Course lectures)
# definitions.variable
# people.important.variable
# PharaohList.3 (a list of 30 famous or interesting pharaohs)
# psalms.in.BofM.4
# random.facts.4 (a list of truly random facts--with topic name, two facts about it, and a source)
# random.lists.variable (a list of random lists; the lists can be however long)
# scriptures.variable (lists of scripture refences, with pertinent excerpts, on some topic)
# TreeLifeList.3 (68 of the dichotomous trees in the tree of life)


# Ancient peoples known by two phonetically dissimilar names:
ancient.names.3 = list(one = "filler",
                       Phrygians = c("Mushki","The Mushki were an Iron Age people of Anatolia who appear in sources from Assyria but not from the Hittites. Several authors have connected them with the Moschoi of Greek sources and the Georgian tribe of the Meskhi. Josephus Flavius identified the Moschoi with the Biblical Meshech. ***Assyrian sources identify the Western Mushki with the Phrygians,*** but Greek sources clearly distinguish between the Phrygians and the Moschoi. The Encyclopedia of Indo-European Culture notes that the Armenians according to Diakonoff, are then an amalgam of the Hurrian (and Urartians), Luvians and the Proto-Armenian Mushki (or Armeno-Phrygians) who carried their IE language eastwards across Anatolia.According to Greek mythographers,[9] the first Phrygian Midas had been king of the Moschi (Mushki), also known as Bryges (Brigi) in the western part of archaic Thrace.Assyrian sources from the 8th century BC speak of a king Mita of the Mushki, identified with king Midas of Phrygia. The Phrygians were an ancient Indo-European people, initially dwelling in the southern Balkans – according to Herodotus – under the name of Bryges (Briges), changing it to Phryges after their final migration to Anatolia, via the Hellespont. - Wikipedia 'Mushki' and 'Phrygians'"),
)


# 27 (26) items in the BMList flashcard deck:
BofM.EME.4 = list(one = "filler",
                   fN6_3 = c("desire","require","And it mattereth not to me that I am particular to give a full account of all the things of my father, for they cannot be written upon these plates, for I desire the room that I may write of the things of God."),
                   fN7_15 = c("choice","judgment","Now behold, I say unto you that if ye will return unto Jerusalem ye shall also perish with them. And now, if ye have choice, go up to the land, and remember the words which I speak unto you, that if ye go ye will also perish"),
                   fN8_12 = c("desirous","desirable","And as I partook of the fruit thereof, it filled my soul with exceeding great joy. Wherefore I began to be desirous that my family should partake of it also, for I knew that it was **desirous** above all other fruit."),
                   fN8_21_A14_27 = c("obtain","reach (a place)","1 Nephi 8:21 - And I saw numberless concourses of people, many of whom were pressing forward, that they might obtain the path which led unto the tree by which I stood. Alma 14:27 - And it came to pass that so great was their fear that they fell to the earth, and did not obtain the outer door of the prison"),
                   fN18_9 = c("to that","until","And after we had been driven forth before the wind for the space of many days, behold, my brethren and the sons of Ishmael and also their wives began to make themselves merry, insomuch that they began to dance, and to sing, and to speak with much rudeness, yea, even **to** that they did forget by what power they had been brought thither"),
                   fN22_13 = c("turn upon","fall upon","And the blood of that great and abominable church, which is the whore of all the earth, shall turn upon their own heads; for they shall war among themselves, and the sword of their own hands shall fall upon their own heads"),
                   sN1_26 = c("manifest","expound, declare","and that which ye call anger was the truth, according to that which is in God, which he could not restrain, manifesting boldly concerning your iniquities"),
                   Ms3_19 = c("but if","unless","For the natural man is an enemy to God, and has been from the fall of Adam, and will be, forever and ever, **but if** he yields to the enticings of the Holy Spirit, and putteth off the natural man and becometh a saint through the atonement of Christ the Lord"),
                   A1_9_A5_53_A8_13 = c("withstand","oppose, deny, contradict","1:9 - Now, because Gideon withstood him with the words of God he was wroth with Gideon, and drew his sword and began to smite him. 5:53 - And now my beloved brethren, I say unto you, can ye withstand these sayings; 8:13 - Now when the people had said this, and withstood all his words, and reviled him, and spit upon him, and caused that he should be cast out of their city, he departed thence"),
                   A7_5_A15_3 = c("by the cause of","on account of, by reason of","nevertheless I do not desire that my joy over you should come by the cause of so much afflictions and sorrow which I have had for the brethren at Zarahemla; 15:3 - Zeezrom lay sick at Sidom, with a burning fever, which was caused by the great tribulations of his mind on account of his wickedness, for he supposed that Alma and Amulek were no more; and he supposed that they had been slain **by the cause of** his iniquity"),
                   A11_2 = c("stripe","whip, beat","Now if a man owed another, and he would not pay that which he did owe, he was complained of to the judge; and the judge executed authority, and sent forth officers that the man should be brought before him; and he judged the man according to the law and the evidences which were brought against him, and thus the man was compelled to pay that which he owed, or be **striped** [not 'stripped'], or be cast out from among the people as a thief and a robber."),
                   A11_25_A24_13_A59_10_tN3_10_Mni7_8 = c("retain","hold back","when thou had it in thy heart to retain them from me. 24:13 - Nay, let us retain our swords that they be not stained with the blood of our brethren. 59:10 - therefore he retained all his force; 3 Nephi 3:10 - which have dissented away from you because of your wickedness in retaining from them their rights of government. Moroni 7:8 - wherefore it is counted unto him the same as if he had retained the gift."),
                   A37_37_A39_10 = c("counsel","to consult, ask counsel of","**Counsel** [not 'counsel with'] the Lord in all thy doings, and he will direct thee for good. 39:10 - And I command you to take it upon you to **counsel** [not 'counsel with'] your elder brothers in your undertakings--for behold, thou art in thy youth and ye stand in need to be nourished by your brothers--and give heed to their counsel."),
                   A44_7 = c("extinct","dead (of an individual)","I will command my men that they shall fall upon you and inflict the wounds of death in your bodies, that ye may become extinct"),
                   A46_17 = c("gave (instead of 'named')","give = describe","And it came to pass that when he poured out his soul to God, he **gave** [not 'named'] all the land which was south of the land Desolation--yea, and in fine all the land, both on the north and on the south--a chosen land and the land of liberty."),
                   A63_5 = c("curious","ingenious","And it came to pass that Hagoth, he being an exceedingly curious man, therefore he went forth and built him an exceedingly large ship"),
                   H1_15 = c("pitch (battle)","set in array","And they came down again that they might pitch battle against the Nephites."),
                   H7_16 = c("hurl","drag","Yea, how could ye have given away to to the enticing of him who art seeking to hurl away your souls down to everlasting misery and endless woe?"),
                   H8_11 = c("depart","divide","Behold, my brethren, have ye not read that God gave power unto one man, even Moses, to smite upon the waters of the Red Sea, and they departed [not 'parted'] hither and thither"),
                   H9_17 = c("detect","expose","And now behold, we will detect this man, and he shall confess his fault and make known unto us the true murderer of this judge."),
                   tN1_29 = c("became","began to act","And there was also a cause of much sorrow among the Lamanites; for behold, they had many children who did grow up and began to wax strong in years, that they became for themselves, and were led away by some who were Zoramites"),
                   Eth6_10a = c("mar","hinder","And thus they were driven forth; and no monster of the sea could break them, neither whale that could mar them"),
                   Eth6_10b = c("break","stop","And thus they were driven forth; and no monster of the sea could break them, neither whale that could mar them"),
                   Eth12_41 = c("commend","recommend (to do a thing)","And now, I would commend you to seek this Jesus of whom the prophets and apostles have written"),
                   Mni10_26 = c("do away","dismiss, reject","And woe unto them which shall do these things away and die, for they die in their sins and they cannot be saved in the kingdom of God."),
                   tpage_scattered = c("scattered","separated (from the main body)","An abridgment taken from the book of Ether also, which is a record of the people of Jared, which were scattered at the time the Lord confounded the language of the people when they were building a tower to get to heaven"))

# y = 5
# x = 2
# z = y
# y = x
# x = z
# y
# x

for (i in 2:27){
  z = BMList.4.27[[i]][3]
  BMList.4.27[[i]][3] = BMList.4.27[[i]][1]
  BMList.4.27[[i]][1] = z
}
BMList.4.27 # THIS IS THE ***REAL*** BMLIST!!

BMList[3] # gives you everything
BMList[[3]] # gives you just what's "in" scripture reference #3
BMList[[3]][2] # gives you just the second sub-item in contents of scripture reference #3


## bythenumbers.3 
# topic, number, source
bythenumbers.3 = list(one = "filler",
                      antibody.drug.industry.revenue = c("Antibodies made by the pharmaceutical industry rake in about $100 billion a year.","source: Nature News - 25 May 2018 - Heidi Ledford"),
                      )


## climate random facts (variable)
# topic name, facts, sources
climate.random.variable = list(one = "filler",
                               Younger.Dryas = c("The Younger Dryas lasted from 11,000 to 9,000 years BP.","Glaciers began to melt quickly following the maximum glacial advance about 18,000 years BP. Global warming ended the most recent Ice Age beginning around 14,000 years BP.","A brief warm period called the Allerod occurred roughly 11,500 years BP, followed the cooling trend of the Younger Dryas in which glaciers began to readvance.","source: geologycafe.com"),
                               climate.since.Younger.Dryas = c("After the Younger Dryas, the Earth became dramatically warmer. The period between about 8,000 and 7,000 years BP is called the Hypisthermal/Altithermal, or Climatic Optimum. More on the Climatic Optimum to follow ...","The Climatic Optimum was the warmest period within the Holocene. Global temperatures may have been as much as 4 C higher than today. Africa was wetter, and the midwest US was more desert-like.","Since the Climatic Optimum, global temperatures have fallen haltingly: ...","A minor warm period called the Little Climatic Optimum began around 300 BC and lasted for roughly 1500 years. Then a cool period called the Little Ice Age began in the 1300s and ended in the 1840s. Earth has been warming ever since. What would normally be happening next (and right now) ...","... is that the Earth should, based on the patterns of previous interglacial stages and Milankovitch Cycles, be heading towards another ice age after the Climatic Optimum."),
                               )


# 15 (14) cards in climate.two.per.lecture:
climate.two.per.lecture = list(one = "filler",
                               earth.warming = c("Since the mid-20th century, our uncertainty is down to 0.05 Celsius, due to the growing number of monitoring stations around the world.","We don't report an actual temperature, but rather a change from 0, because it's 'easier' to talk about temp changes from a particular station, or from a vast number of stations."),
                               BGH.satellites.strato = c("Microwave emission from oxygen molecules can be measured by satellites in order to get an idea of the temperature for vast volumes of atmosphere.","While the troposphere has warmed, the stratosphere has undergone a cooling trend--an effect scientists would expect in a greenhouse scenario."),
                               iceAges.recently = c("Atmospheric water vapor is O-18-depleted, because O-18 doesn't evaporate as readily. Moreover, the further poleward the condensation goes, the more O-18-depleted it becomes. But more O-18 makes it to the poles the warmer the planet is.","Ten independent studies pointed to the same overall temperature trend during the last thousand years; averaging them yields a very gradual cooling trend up until the 20th century."),
                               greenhouse = c("The distinction between energy balance and equilibrium: When a system reaches the same temperature as its surroundings, that is equilibrium. When the rate at which energy flows into a system is the same as the rate of energy leaving the system, that is energy balance, even if the system's temperature is not the same as its surroundings.","The earth gets 240 watts per m^2 from sunlight. Or, rather, it's about 164 watts per m^2. However, that's an average over 24 hours, over the whole earth. The extraterrestrial solar radiation is actually 1367 watts per square meter at the upper reaches of the atmosphere. At the ground, that amount comes down to 1050 Watts/m^2."),
                               which.molec.planets = c("Which molecule dominates the greenhouse effect? Water vapor! But it precipitates back out of the gas phase within about a week on average.","Earth's orbital eccentricity somehow has 100k- and 413k-year cycles (how, though?). Earth's axial tilt (of 23 deg.) has a 41k-year cycle; in other words, it takes 41k years for the tilt to go from 22.1 to 24.5 degrees and back again, while its rotational axis precesses. It's maximum tilt of 24.5 was reached around 8,700 BC, and the 22.1 minimum will be reached around 11,000 AD."),
                               cycles = c("The water cycle is fast (average time a water molecule spends in the atmosphere is about a week). Guess how long the average carbon-containing molecule stays in the atmosphere? 5 years! WTF?? But the carbon cycle is not just one cycle--there are many widely varying timescales. The 5 year cycle is misleading because if carbon goes into a plant, it will end up cycling right back into the atmosphere really quickly. The carbon we emit will cycle through the atmosphere and transient 'sinks' for a good 300 to 1,000 years.","There are a whopping 39,000 Gt of carbon in the deep ocean. There might be around 10,000 Gt of carbon in fossil fuels, or maybe half that, or maybe twice that. No one knows for sure."),
                               humans = c("Fossil fuel CO2 is C-13-depleted relative to volcanic CO2, because photosynthetic life preferrentially takes up C-12.","Atmospheric CO2 was at 280 ppm before the industrial revolution, and now it's at 400. That's more than a 40% increase. CO2 levels were flat over the last 1,000 years, until the rapid upswing starting with the industrial revolution. Furthermore, while the temperature today might be near the top of the range we see over the last several hundred thousand years, the CO2 is way above."),
                               the.future = c("The equation that calculates the temperature of the earth's surface based solely on its distance from the Sun is a zero-dimensional model. A 1D, two-box climate model involving the atmosphere and the surface, with arrows running between, is more complicated. A 2D model is more complicated still, with a curved surface and heat diffusing from the equator to the poles. Global Circulation Models are 3D models, and are more complicated still, even though the earth is pretty symmetric east to west. Even without adding the fourth dimension--time--computers are a necessity for modeling climate. There are climate model runs that have taken the better part of a year of straight computing.","Validating computer models can be done with the vertical temperature curve of the atmosphere, and the latitudinal variation of precipitation, as well as the Mt. Pinatubo eruption, which proved climate models can be quite accurate."),
                               impacts = c("Wolfson says the sea level rise in the 20th century is 1-2 mm per year. Nerem et al. (PNAS Feb 2018) say that since 1993 it has been rising at 3 mm per year give or take .4 mm. They also say the rate of rise, since 1993, has been accelerating by .084 mm per year per year, give or take .025 mm.","Even with 2 feet of sea level rise, Bangladesh stands to lose 20% of its land area. The economic cost of switching from fossil fuels to renewables should factor in the cost of staying on fossil fuels."),
                               energy = c("The US represents about 4% of the global population, but we consume about a quarter of the total energy budget.","About 87% of global energy consumption is from fossil fuels."),
                               alternatives = c("There will be an energy crisis when peak oil occurs. Peak oil is not when the last rop of oil is used, but rather when either the demand vs supply trend changes from a stable one to a discrepant one.","All the radioactive waste produced by a nuclear power plant in one year could be condensed down into a container the size of the space occupied by a dinner table. Every year, one truck load is all that is needed to restock a nuclear power plant, while a comparable coal-fired plant needs 110 train-cars of coal every week."),
                               sustainable.futures = c("What is a dangerous level of CO2? Double the pre-industrial levels? So 560 ppm?","One wind turbine of the largest size only produces 5 megawatts at its peak. That's 200 times less than the capacity of a coal plant."),
                               Extras = c("Greenhouse: O2 and N2 don't absorb infrared like CO2 and H2O do. These triatomic molecules can vibrate in ways that absorb more radiation, which the diatomics can't do--that's why they trap heat. The greenhouse effect is about 33 Celsius, or 60 Fahrenheit, making the earth's average temperature go up from the 0 F it would be without an atmosphere to about 60 F.","If you look at a graph showing the paleoclimate reconstructions of the last millennium, the recent upswing to today's temperature anomoly is high but only at around .5 deg. Celsius above the next highest temp peak in antiquity. If temperature continues to rise at this rate, by 2100 it will be way off that chart, at 3 deg. Celsius higher. 3 degrees doesn't sound like much until you see that graph."),
                               Questions = c("Why does the precession of the equinoxes have a 26,000-year cycle?","Why exactly, in the real world, do a few degrees matter? Ivar Giaever commented on the significance of the apparent rise in temperature when he stated, 'What does it mean that the temperature has gone up 0.8 degrees Kelvin: probably nothing.'"))


# definitions.variable flashcard deck:
definitions.variable = list(one = "filler",
                     The.World.Bank = c("The World Bank is an international financial institution that provides loans to [not necessarily poorer] countries, with the goal of reducing poverty.","It was created in 1944 at the Bretton Woods Conference along with the IMF (International Monetary Fund). Both are based in Washington DC and work closely with each other.","The President of the bank is traditionally an American, nominated by the US, which is the largest shareholder in the bank (the managing director of the IMF having always been a European).","Joseph Stiglitz was chief economist at the World Bank from '97 to '00.","45 countries pledged $25 billion in aid for the world's poorest countries (in 2007??). The World Bank has received criticism due to wealthier nations funding their own aid projects, like those for diseases.","IDA money is, according to Robert Zoellick, former president of the World Bank, the core funding that the poorest developing countries rely on.","The World Bank has also been criticized, most strongly, for the way it is governed. It is supposed to represent 188 countries, but it is run by a small number of economically powerful ones. Those countries choose the bank's leaders, and their interests dominate its activities. Some have argued that rather than alleviating poverty, its projects have primarily served to expand government bureaucracy."),
                     The.IMF = c())


# important people
people.important.variable = list(one = "filler",
                                 Otto_Warburg = c("1883-1970; son of physicist Emil Warburg (friend of Einstein's), German Jewish (Emil's parents were Orthodox Jews, though Emil had converted to Protestantism) Nobel (1931, for his 'discovery of the nature and mode of action of the respiratory enzyme') laureate","Warburg Hypothesis: To quote Warburg himself, 'Cancer, above all other diseases, has countless secondary causes. But, even for cancer, there is only one prime cause. Summarized in a few words, the prime cause of cancer is the replacement of the respiration of oxygen in normal body cells by a fermentation of sugar.' Warburg thought that cancer is caused by cells generating ATP mainly by anaerobic breakdown of glucose (i.e. fermentation/anaerobic respiration), in contrast to healthy cells, which undergo oxidative breakdown of pyruvate. Because pyruvate is oxidized in the mitochondria, Warburg thought cancer should be interpreted as a mitochondrial dysfunction."),
                                 )


# 31 (30) items in the PharaohList.3 flaschard deck:
PharaohList.3 = list(one = "filler", 
                     BC3100a = c("Narmer","Pharaoh of the 1st dynasty in the Early Dynastic Period. First to unite the lands of upper and lower Egypt."),
                     BC3100b = c("Menes","Believed to be the same person as Narmer. Founded the 1st dynasty, according to Herodotus."),
                     BC2670 = c("Djoser","Pharaoh of the 3rd dynasty during the Old Kingdom. Built the step pyramid as part of his funeral complex at Saqqara. Later pharaohs thought his reign to be the beginning of pharaonic history."),
                     BC2613 = c("Sneferu","First pharaoh of the 4th dynasty, Old Kingdom. Built the first true pyramid, changed the orientation of the funerary complex to east-west. Buried in the Red Pyramid."),
                     BC2589 = c("Khufu","aka Greek 'Cheops' / 'Suphis'. 4th dynasty, Old Kingdom. Built the Great Pyramid, with burial chamber in the center rather than at the bottom of the pyramid. Depicted as a cruel tyrant by ancient Greek authors, but as a pious, generous ruler by [contemporary?] Egyptian sources. Main protagonist of the famous Westcar Papyrus. The first imprinted papyri originate from his reign."),
                     BC2558 = c("Khafre","aka Greek 'Cephren'. 4th dynasty, Old Kingdom. Second largest pyramid at Giza. Built the Great Sphinx."),
                     BC2460 = c("Neferefre","5th dynasty, Old Kingdom. His pyramid only reached its lowest courses. Records state he built a sun-temple. A store of papyri was discovered in his pyramid temple providing information about the economic, administrative, and religious practices of his time."),
                     BC2278 = c("Neferkare Pepi II","6th dynasty, Old Kingdom. His rule was Egypt's longest at 94 years. The first half of his reign was prosperous, the second half was a time of economic crisis."),
                     BC2180 = c("Neitiqerty Siptah and Nitocris","Last pharaoh of the 6th dynasty, Old Kingdom. The male king gave rise to the legendary queen mentioned by Herodotus and Manetho. Modern scholars doubt she ever existed."),
                     BC1971 = c("Kheperkare Senusret I","aka Sesostris I. 12th dynasty, Middle Kingdom. Built the White Chapel. Reigned during a time of peace with no records of military campaigns found to date. First pharaoh to begin irrigating the Faiyum. His statues show signs of his actual appearance, marking a new trend in Egyptian art."),
                     BC1878 = c("Sesostris III","12th dynasty, Middle Kingdom. Most powerful pharaoh of the 12th dynasty. The pharaoh of Abraham, according to John Gee. Also Amenemhat III, the next pharaoh, could have been a pharaoh of Abraham's day."),
                     BC1550 = c("Ahmose I","Founder of the 18th dynasty, beginning the New Kingdom. Began the reunification of Egypt after the second intermediate period. Fought battles throughout Egypt, Palestine and Kush as he sought to banish the Hyksos. Goods and artwork show a Minoan influence during this time. He built projects at Memphis and Thebes (his religious capital), especially at Karnak."),
                     BC1541 = c("Amenhotep I","18th dynasty, New Kingdom. Continued his father's building projects and military campaigns. The workmen at Deir el-Medina (the town of the builders of the tombs in the Valley of the Kings) worshipped him and his mother as their patron gods for centuries."),
                     BC1492 = c("Thutmose II","18th dynasty, New Kingdom. His wife, Hatshepsut, attempted to replace his name on monuments with hers. His son later tried to restore his father's name, resulting in conflicting information about this pharaoh's life. His mummy was found in the royal cache at the Temple of Hatshepsut, showing signs of diseases that caused his death."),
                     BC1479 = c("Hatshepsut","18th dynasty, New Kingdom. Second known female ruler of Egypt. Married her uncle. Ruled during the height of Egypt's power. Began her reign merely as regent because her son, the next heir, was still a child. She claimed to be the child of Amun and transformed herself into a king by wearing the symbols of kingship. Known for her building projects. Sent trade missions to the land of Punt and brought back various exotic goods."),
                     BC1458 = c("Thutmose III","18th dynasty, New Kingdom. Conducted military campaigns in the Levant, conquered most of Palestine. His mother's name and monuments were not dishonored until the end of his reign. Built many monuments."),
                     BC1425 = c("Amenhotep II","18th dynasty, New Kingdom. Completed the dishonoring of Hatshepsut's monuments to end any claims by her family to the right to rule. Built various temples, including one to worship Horemakhet, a god associated with the Great Sphinx."),
                     BC1401 = c("Thutmose IV","18th dynasty, New Kingdom. Son of Amenhotep II but not the crown prince. His unexpected rule was legitimized by the Dream Stele, according to which he had a vision of Horemakhet-Khepri-Ra-Atum, while sleeping under the shadow of the Great Sphinx, telling him to restore the Sphinx by removing the sand that had partially buried it, and he would be given the kingship."),
                     BC1390 = c("Amenhotep III","18th dynasty, New Kingdom. Harvests during his time were rich, and he later became a fertility god. Called his palace 'the gleaming Aten' and emphasized the worship of various solar deities. Built a large tomb in the Valley of the Kings and the Colossi of Memnon near his mortuary temple."),
                     BC1352 = c("Amenhotep IV","aka Akhenaten. 18th dynasty, New Kingdom. Came to the throne at a time when the priests of Amun were wealthy and powerful. Built a temple to Aten at Karnak during the first few years of his reign. Built a new capital at Akhetaten (now Amarna), changed his name, and declared Aten the only god in Egypt (described as monolatristic, henotheistic, or even quasi-monotheistic. Started what is known as the Amarna Period."),
                     BC1334 = c("Neferneferuaten","18th dynasty, New Kingdom. Many scholars believe she was Akhenaten's queen, Nefertiti, others that she was Meritaten, the daughter of Akhenaten and Nefertiti."),
                     BC1332 = c("Tutankhamun","18th dynasty, New Kingdom. Son of Akhenaten, became pharaoh at age 9. During the first year of his reign, he abandoned Amarna and restored the cults of the traditional deities. He restored the power of Thebes. Egyptologists found his treasures and body intact when they excavated his tomb in the 1920s."),
                     BC1292 = c("Ramesses I","Founded the 19th dynasty, New Kingdom. Succeeded Horemheb, who was born a commoner, and became a general during the Amarna Period, and who obliterated images of the Amarna pharaohs and destroyed buildings and monuments associated with them. And this pharaoh, who succeeded Horemheb, was also of non-royal birth. This pharaoh's reign marked the transition between the stabilizing reign of Horemheb and the rule of the powerful pharaohs of the 19th dynasty (in particular his son Seti I and grandson Ramesses II)."),
                     BC1290 = c("Seti I","19th dynasty, New Kingdom. Conducted military campaigns to raise money for his building projects. After the enormous social upheavals of Akhenaten's religious reform, Horemheb, Ramesses I and this pharaoh's main priority was to re-establish order in the kingdom and reaffirm Egypt's sovereignty over Canaan and Syria, by confronting the Hittites."),
                     BC1279 = c("Ramesses II (The Great)","19th, NK. Known as Ozymandias in the Greek sources (transliteration of his throne name, Usermaatre Setepenre - 'The justice of Re is powerful--chosen of Re'). One of the most powerful pharaohs of all time. He had at least 95 children. Reigned for 67 years. Built a massive tomb for his children in the Valley of the Kings. He usurped monuments made by older pharaohs by erasing their names and carving his own instead. He declared himself a god before the tenth year of his reign and outlived his 12 oldest sons. He is famous for the Battle of Kadesh, against the Hittites. During his reign, the Egyptian army is estimated to have totaled 100,000 men."),
                     BC1213 = c("Merenptah / Merneptah","19th dynasty, New Kingdom. 13th son of Ramesses II. The Israel Stele is a name commonly given to his victory stele in which he is said to have laid waste to 'ysriar'--the earliest apparent textual reference to Israel known to date, and the first from Egypt."),
                     BC1191 = c("Twosret","19th dynasty, New Kingdom. A female pharaoh, wife of Seti II. Third female pharaoh to rule during the New Kingdom."),
                     BC1186 = c("Ramesses III","20th dynasty, New Kingdom. Not a relative of the previous Ramesses's. After Twosret's death, there was a period of lawlessness that this pharaoh's father ended. This pharaoh had to fight various invaders trying to take advantage of Egypt's internal turmoil. He reorganized temple administrations and land allocations, giving one third of the farm land over to the temples, causing a food shortage which led to one of the first recorded strikes of the workers at Deir el-Medina. It also led to a weakening of the central government."),
                     BC530 = c("Cambyses II","1st Persian dynasty (Achaemenid Empire) = 27th dynasty, in the Late Period. First foreign (absentee) pharaoh, ruling from Persia."),
                     BC332 = c("Alexander the Great","In Egypt, he was portrayed as the son of Nectanebo II, the last pharaoh before the Persian conquest. His defeat of Darius was depicted as Egypt's salvation, 'proving' Egypt was still ruled by an Egyptian. The greatest city he founded was Alexandria, in Egypt. He was regarded as a liberator in Egypt. He was pronounced son of the deity Amun at the Oracle of Siwa Oasis in the Libyan desert. Henceforth, Alexander often referred to Zeus-Ammon as his true father, and after his death, currency depicted him adorned with the horns of a ram as a symbol of his divinity."),
                     BC51_30 = c("Cleopatra VII","Last pharaoh of the Ptolemaic dynasty. Rome was moving to invade Egypt but Mark Antony, her lover, helped her stand against Rome. Then Augustus invaded Egypt and killed Mark Antony. This pharaoh then committed suicide, marking the end of pharaonic Egypt."))


## Political stuff:
# Front side: topic; back sides: some facts with sources
political.facts.variable = list(one = "filler",
                                Israel.2005.disenggmnt.Gaza.part1 = c("aka 'Gaza expulsion', it was the withdrawal of the Israeli army from and dismantling of all 21 Israeli settlements in the Gaza Strip in 2005. Despite the disengagement, the Gaza Strip is sill considered to be under military occuption by Israel by ...","... the UN, international human rights organizations and most legal scholars. - source: Andrew Sanger, 'The Contemporary Law of Blockade and the Gaza Freedom Flotilla', in _Yearbook of International Humanitarian Law 2010_ (2011) ...","... What are the reasons listed by Sanger (2011) for rejecting the Israeli claim of disengagement? ...","Reason 1: Israel's own Disengagement Plan provides for Israel monitoring the external land perimeter of the Gaza Strip. ...","... Reason 2: Israel maintains exclusive authority in Gaza air space and maintains security activity off the Gaza coast. ...","... Reason 3: Israel reserves the right to reenter Gaza at will and maintains a military presence on the Egyptian-Gaza border. ...","Reason 4: Israel continues to control six of Gaza's seven land crossings as well as its maritime borders, and controls the movement of goods and people in and out of the territory. ...","Reason 5: Israeli Defence Force regularly enters parts of the territory, deploys missile attacks, drones and sonic bombs into Gaza. ...","... Reason 6: Israel has declared a no-go buffer zone stretching deep into Gaza: if Gazans enter this zone they can be shot on sight. ...","... Reason 7: Gaza is also dependent upon Israel for water, electricity, telecommunications and other utilities, currency, issuing IDs, and permits to enter and leave the territory. Israel also maintains sole control over the Palestinian Population Registry (through which they classify who is a Palestinian, who is a Gazan or West Banker)."),
                                Israel.2005.disnggmnt.Gaza.part2.aftermath = c("The disengagement/expulsion took place in August and finished in September. Many Israelis resisted and some protesters pelted Israeli military/security with rocks and barricaded themselves. Some told their children to walk with their hands raised in reference to the Holocaust. Right-wingers fiercely opposed Ariel Sharon's action to disengage, including Benjamin Netanyahu, who threated to resign then took it back. But Sharon did win unusual support from the left for the Gaza withdrawal move. So why did Sharon do it? Did he go back on his campaign rhetoric in a sudden inexplicable abandonement of his former professed interests? Apparently it was all about demographics: Israeli democratic majority. Palestinian population growth was outpacing that of Israelis in Gaza.","What happened with the greenhouses? ...","... The following people (7) strongly believed that Israelis left behind all their greenhouses in perfect condition out of an act of selfless charity and good will, only to have them all demolished by rabid Palestinians who wanted to destroy Israeli gifts as much as Israeli people: ...","... Ezra Levant, writing in the Toronto Sun in July 2014. ...","... Charles Krauthammer, writing in the Washington Post in July 2014. ...","... Richard Chesnoff, writing in the Huffington Post in July 2014. ...","J J Goldberg, writing in The Atlantic in July 2014. ...","... Alan Dershowitz, writing in the Jerusalem Post in July 2014. ...","... Lee Smith, writing in Tablet (Israeli magazine), in November 2014. ...","... and Yair Rosenberg, writing in Tablet in July 2014.","What was the reality of the greenhouse debacle? ...","... In reality, the Israeli greenhouse owners were supposed to receive extra compensation for leaving the greenhouses behind. Non-profits like the Economic Cooperation Foundation raised millions in donations. But many Palestinians objected to Israeli settlers being given extra compensation, and the laws of many donor countries and international institutions prevent giving aid to a relatively wealthy country like Israel. About half of the 1,000 acres of greenhouses were destroyed by the owners themselves after giving up waiting for the money they were promised. - source: Steven Erlanger, writing in the New York Times in July 2005 (writing at a time before Palestinians could have destroyed them?? Maybe not?) ...","... The greenhouses that were left were looted by Palestinians. Palestinian Authority security forces attempted to stop them but didn't have the manpower. Then, the Palestinian Economic Development Company invested $20 million and by October the industry was back on its feet. ...","... However, the harvest intended for export via Israel to Europe was lost due to Israeli restrictions on the Karni crossing, which was closed more often than not, leading to losses in excess of $120,000 per day. Economic consultants estimated that the closures cost the whole agricultural sector in Gaza $450,000 a day in lost revenue. 25 truckloads of produce a day through that crossing were needed to render the project viable, but they were lucky to get 3. - sources: ...","... (source for Palestinian looting and Israeli cause of harvest losses: Justin Schwegel, writing in Mondoweiss in August 2014)"),
                                )
## Links to sources:
# Andrew Sanger (2011) - Yearbook of International Humanitarian Law 2010 ... https://books.google.com/books?id=hYiIWVlpFzEC&pg=PA429#v=onepage&q&f=false
# Steven Erlanger (July 2005) - New York Times ... https://www.nytimes.com/2005/07/15/world/middleeast/israeli-settlers-demolish-greenhouses-and-gaza-jobs.html
# Justin Schwegel (August 2014) - Mondoweiss ... http://mondoweiss.net/2014/08/propaganda-dehumanize-palestinians/
# Wikipedia source for the Israeli Disengagement of Gaza in 2005: https://en.wikipedia.org/wiki/Israeli_disengagement_from_Gaza#cite_note-occ-2


## Psalms in the Book of Mormon (49 - 1 = 48):
# Phrase (front side), Book of Mormon reference, Psalm reference
psalms.in.BofM.4 = list(zero = "filler",
                      one = c("tender mercies are over all","1 Nephi 1:20","Psalm 145:9"),
                      two = c("to take away my life","1 Nephi 7:16","Psalm 31:13"),
                      three = c("according to the multitude of his tender mercies","1 Nephi 8:8; Ether 6:12","Psalm 51:1 and 69:16"),
                      four = c("rod of iron","1 Nephi 8:19","Psalm 2:9"),
                      five = c("my rock and my salvation","1 Nephi 13:36","Psalm 62:2,6"),
                      six = c("broken heart ... contrite spirit","2 Nephi 2:7; 2 Nephi 4:32; 3 Nephi 9:20; Mormon 2:14; Ether 4:15","Psalm 51:17 and 34:18"),
                      seven = c("great ... goodness ... trust","2 Nephi 4:17,19","Psalm 31:19"),
                      eight = c("cry/cried","2 Nephi 4:23,25","Psalm 30:8"),
                      nine = c("heart ... rejoice","2 Nephi 4:28","Psalm 28:7"),
                      ten = c("because of mine enemies","2 Nephi 4:27,29","Psalm 27:11"),
                      eleven = c("my God and the rock of my salvation","2 Nephi 4:30","Psalm 89:26"),
                      twelve = c("I will praise thee forever","2 Nephi 4:30","Psalm 52:9"),
                      thirteen = c("to take away my life","2 Nephi 5:2","Psalm 31:13"),
                      fourteen = c("clean hands and a pure heart","2 Nephi 25:16; Alma 5:19","Psalm 24:4"),
                      fifteen = c("water my couch/pillow by night with my tears","2 Nephi 33:3","Psalm 6:6"),
                      sixteen = c("as in the provocation ... in the day of temptation in the wilderness","Jacob 1:7","Psalm 95:8"),
                      seventeen = c("pains of hell","Jacob 3:11","Psalm 18:4-5 and 116:3"),
                      eighteen = c("in great mercy ... over all his works","Jacob 4:10","Psalm 145:8-9"),
                      nineteen = c("the stone which the builders refused is become the head stone of the corner","Jacob 4:17","Psalm 118:22"),
                      twenty = c("today if ye will hear his voice, harden not your heart","Jacob 6:6; Alma 12:36","Psalm 95:7-8"),
                      twentyone = c("ye are called ... his sons ... this day he hath begotten you","Mosiah 5:7","Psalm 2:7"),
                      twentytwo = c("at the right hand (of God)","Mosiah 5:9","Psalm 110:1 - this one is a huge stretch"),
                      twentythree = c("break ... bands ... death","Mosiah 15:8; Alma 5:7; Alma 22:14","Psalm 107:14, also 18:4-5 and 116:3"),
                      twentyfour = c("delivered my soul from ... hell","Alma 5:6","Psalm 86:13"),
                      twentyfive = c("mercy ... long-suffering","Alma 5:6","Psalm 86:15"),
                      twentysix = c("chains/sorrows of hell","Alma 5:7,9,10","Psalm 18:5"),
                      twentynine = c("in the paths of righteousness","Alma 7:19","Psalm 23:3"),
                      thirtyone = c("after the order of Melchizedek/his son","Alma 13:1,2,9","Psalm 110:4"),
                      thirtytwo = c("wrath ... enter into ... rest","Alma 12:35,37","Psalm 95:8,11"),
                      thirtythree = c("enter into my rest","Alma 13:6","Psalm 95:11"),
                      thirtyfour = c("pains of hell","Alma 14:6; Alma 26:13","Psalm 18:4-5 and 116:3"),
                      thirtysix = c("darkness ... into marvelous light","Alma 26:3","Psalm 118:23,27"),
                      thirtyseven = c("gather ... give thanks to his holy name ... praise","Alma 26:6,8","Psalm 106:47"),
                      thirtyeight = c("boast of my God","Alma 26:12,35","Psalm 44:8"),
                      forty = c("laugh ... to scorn","Alma 26:23","Psalm 22:7"),
                      fortyone = c("God/the Lord is mindful of","Alma 26:37","Psalm 115:12"),
                      fortytwo = c("bring forth fruit","Alma 32:37","Psalm 1:3"),
                      fortythree = c("in the midst of thy congregations","Alma 33:9","Psalm 74:4"),
                      fortyfour = c("keep ... preserve ... generation","Alma 37:4","Psalm 12:7"),
                      fortyfive = c("as chaff before the wind","Alma 37:15; Mormon 5:16","Psalm 35:5"),
                      fortysix = c("marvelous works","Alma 37:41","Psalm 9:1; also 118:23 and 139:14"),
                      fortyseven = c("because of their transgression ... afflicted","Alma 37:42","Psalm 107:17"),
                      fortynine = c("pure in hear ... shall see God","3 Nephi 12:8","Psalm 24:4,6"),
                      fifty = c("depart from me all ye workers of iniquity","3 Nephi 14:23","Psalm 6:8"),
                      fiftyone = c("the light of thy countenance","3 Nephi 19:25","Psalm 4:6"),
                      fiftytwo = c("out of the mouth of babes","3 Nephi 26:16","Psalm 8:2"),
                      fiftysix = c("counted unto him for righteousness","Moroni 7:7","Psalm 106:31"),
                      fiftyseven = c("none that doeth good, no, not one","Moroni 10:25","Psalm 14:3 (also 53:3--53 and 14 are the same)"))


## Random facts to remember (with 4 components, usually the last one is a source. So, 
# a topic name, two facts about it, and a source):
random.facts.4 = list(one = "filler",
                      humans.v.volcanoes_CO2_sources = c("Humans emit ~24 billion tons of CO2 a year","Volcanoes emit ~200 million tons CO2. That means volcanoes emit .83% what humans emit","This is according to the USGS. Also, the federally funded Carbon Dioxide Information Analysis Center says CO2 has gone up year after year, regardless of whether there have been major volcanic eruptions. - from a Scientific American article from probably 2009"),
                      layy.v.lehi_F.M.Cross_plus.sources = c("The connection of the name Lei (classical Arabic Layy, meaning 'bend, twist') with Lehi is based on a linguistic blunder. ... I would not support layy = lehi anymore than I would confuse Lee with Locke.","lyy and lhy cannot be confused in Semitic. The 'h' is a strong laryngeal spirant in Semitic, like the German ch in Buch or Scottish ch in loch.","I read this F M Cross quote from BAR in Jeffrey Chadwick's 2009 Khirbet Beit Lei and the Book of Mormon: An Archaeologist's Evaluation."),
                      Beit.Lei.archaeology_when.was.it.not = c("Was there any ancient Israelite or Jewish settlement at Khirbet Beit Lei during the time of biblical Samson (ca. 1100 BC; Iron Age I) or during the time of the Book of Mormon prophet Lehi (ca. 600 BC; Iron Age II)?","Answer: No. The archaeological survey of Khirbet Beit Lei carried out by Yehuda Dagan in the 1970s revealed no evidence of any Iron Age I or Iron Age II settlement at the site. This was confirmed by the archaeological excavations of Joseph Patrich and Yoram Tsafrir in the 1980s and by the current excavations being carried out by Oren Gutfeld and Yakov Kalman. Not even a single sherd of Iron Age I or II pottery has been found at Khirbet Beit Lei, nor any architectural component from those periods. There was no city, nor town, nor village, nor private estate at Khirbet Beit Lei during the time of Samson or of Lehi.","So could there have been any kind of settlement at Khirbet Beit Lei around 600 BC that could be called a city of Lehi or Beit Lehi, or was there any community at the site around 600 BC in which Lehi might have prophesied? Answer: No city or town; no community at all. No one lived at the site in 600 BC."),
                      Beit.Lei.archaeology_when.was.it.then = c("When was the village at Khirbet Beit Lei an active community?","Answer: All of the archaeologists (mentioned in another flashcard) affirm that the site was first utilized as an oil-pressing complex during the Hellenistic and early Roman periods (ca. 300 BC to AD 70). Underground oil presses and dovecotes have been excavated at the site by Gutfeld and Kalman. Later, during the Byzantine period (fourth to sixth centuries AD), the site was used as a Christian monastic complex with an elaborately decorated chapel, which was excavated by Patrich and Tsafrir. The site seems to have been abandoned thereafter until it was resettled and built up as an Arab village during the Mameluke period (thirteenth to fifteenth centuries AD). The ruins of houses and public buildings from this era have been cleared by Gutfeld and Kalman; pottery remains from the Hellenistic, Roman, Byzantine, and Mameluke periods have been recovered from all over the site.","Was Khirbet Beit Lei ever a Jewish site? Answer: Possibly. According to Gutfeld, the site seems to have been taken over by Jewish forces during the time of John Hyrcanus, around 100 BC. The site may have been used by Jews of Judea from that time until the Roman war against Judea, which culminated in AD 70."),
                      Otto_Warburg = c("1883-1970; son of physicist Emil Warburg (friend of Einstein's), German Jewish (Emil's parents were Orthodox Jews, though Emil had converted to Protestantism) Nobel (1931, for his 'discovery of the nature and mode of action of the respiratory enzyme') laureate","Warburg Hypothesis: To quote Warburg himself, 'Cancer, above all other diseases, has countless secondary causes. But, even for cancer, there is only one prime cause. Summarized in a few words, the prime cause of cancer is the replacement of the respiration of oxygen in normal body cells by a fermentation of sugar.' Warburg thought that cancer is caused by cells generating ATP mainly by anaerobic breakdown of glucose (i.e. fermentation/anaerobic respiration), in contrast to healthy cells, which undergo oxidative breakdown of pyruvate. Because pyruvate is oxidized in the mitochondria, Warburg thought cancer should be interpreted as a mitochondrial dysfunction."),
                      )

## Random lists of variable lengths:
# topic (title?) and date and _Magazine/Journal; author?; facts (however many; with sources if needed); commentaries/interpretations/conclusions (however many; by whom); further reading
# Then develop another deck of flashcards with just facts (first) then you have to guess the source
random.lists.variable = list(one = "filler",
                             intersex.conditions.10 = c("XX CAH (congenital adrenal hyperplasia)","XX progestin-induced virilization","XY AIS (androgen-insensitivity syndrome)","XY 5-ARD (5-alpha reductase deficiency)","XY PMDS (persistent Mullerian duct syndrome)","XY anorchia","XXY (Klinefelter)","XX male (de la Chapelle syndrome)","XY gonadal dysgenesis (Swyer syndrome)","true hermaphroditism"),
                             quantum.mechanics_old.new.schools.17 = c("old - Hendrik Lorentz","old - Max Planck","old - Albert Einstein","new - Niels Bohr","Max Born","Erwin Schrodinger","Arthur Compton","Louis de Broglie","Wolfgang Pauli","Paul Dirac","Werner Heisenberg","Enrico Fermi","Richard Feynman","Julian Schwinger","Sin-Itiro Tomonaga","J Robert Oppenheimer"),
                             pathbreakers.early20th.12 = c("Einstein - 1905 papers on special relativity, Brownian motion, m=E/c^2, and photoelectric effect","Picasso - 1907 Les Demoiselles d'Avignon","Matisse - 1904 Luxe, Calme et Volupte; and 1905 Woman with a Hat","Stravinsky - 1913 Rite of Spring","Schoenberg","Joyce","Eliot","Proust","Diaghilev","Freud","Wittgenstein","and dozens of others"),
                             world.scripture.11 = c("Biyan Lu (Blue Cliff Record) - Zen Classic - 12th century","Wumenguan (Gateless Gate) - Zen Classic - 13th century","Jewish Zohar - 13th century","Rumi's Masnavi - organized around a series of fables - 13th century","Bar-do Thos-grol (Book of the Dead) - Tibetan - 14th century","Mayan Popol Vuh - 16th century","Adi Granth - Sikhs - 17th century","Kitab-i-Iqan (Book of Certitude) - Bahai - 1861","Kitab-i-Aqdas (Most Holy Book) - Bahai - 1873","Hindu Puranas and Epics such as the Mahabharata and Ramayana","Buddhist Jataka tales"),
                             minerals.that.make.helium.5 = c("cleveite","pitchblende","carnotite","monazite","these minerals contain uranium and thorium; the helium is produced as alpha particle emission via radioactive decay"),
                             NT.apocrypha.12 = c("Gospel of Thomas","Gospel of Philip","Apocryphon of James","Pistis Sophia","close candidate for canonization - Epistle of Barnabas","close candidate for canonization - Epistles of Clement","Epistles of Ignatius","Apocalypse of Paul","close candidate for canonization - Shepherd of Hermas","close candidate for canonization - Didache","close candidate for canonization - Apocalypse of Peter (also the only book never accepted as canonical that was commented on by a Church Father","close candidate for canonization - Third Epistle to the Corinthians"),
                             NT.apocrypha.close.candidates.6 = c("Epistle of Barnabas","Epistles of Clement","Shepherd of Hermas","Didache","Apocalypse of Peter","Third Epistle to the Corinthians"),
                             Biblical.Christianity.so.called.10 = c("The following really have no effect on your 'Biblical' Christianity?","Platonism","Aristotelianism","Neoplatonism","Manichaeism","Augustinianism","Averroism","Thomism","Calvinism","modern Fundamentalism"),
                             Rupert.Sheldrake.10dogmasofscience.10 = c("Nature is mechanical; the universe is like a machine; plants and humans and everything can be understood in principle as machines","Matter is unconscious: stars and planets and atoms. Not even humans are conscious.","The laws of nature are fixed: they're the same as they were at the big bang, and they'll stay the same forever into the future","The total amount of matter-energy in the universe is always the same","Nature is purposeless: evolution has no purpose; no other natural process is teleological","Biological heredity is material (he could have just said everything is material according to philosophical materialism","Memories are material modifications in the brain (again, philosophical materialism)","Your mind is in your brain (again, everything is material according to philosophical materialism)","Psychic phenomena are impossible","Mechanistic medicine is the only kind that works--complementary and alternative therapies don't work"),
                             ancientDNA.1Jun18_Science = c("(news article) authors: Achilli, Olivieri, Semino, Torroni",""),
                             ice.cores.Roman.lead.pollution.14May18_PNAS = c("Joseph McConnell (leading expert in ice core analysis; at the Desert Research Institute (DRI) in Reno), Andrew Wilson (classical archaeologist at Oxford; Head of School of Archaeology there), and others","Lead pollution in Arctic ice reflects midlatitude emissions from ancient lead-silver mining and smelting.","Though measurements have been extrapolated to infer performance of ancient economies before, including that of the Roman Empire, past studies were based on sparse sampling and inaccurate dating. This study shows that annual European lead emissions closely varied with historical events, like imperial expansion, wars, and major plagues.","Emissions rose coeval with Phoenician expansion, and reached a maximum under the Roman Empire.","Their results indicate sustained economic growth during the first two centuries of the Roman Empire, terminated by the 2nd century Antonine plague.","Materials/Methods (M/M): They analyzed parts of the NGRIP2 ice core--originally collected in 1998--using the DRI's continuous melter system.","M/M cont.: The DRI ice-core analytical system includes two high-resolution inductively coupled plasma mass spectrometers (HR-ICP-MS) operating in parallel. These MSs, along with a lot of other instruments, are capable of simultaneous measurements of ~30 elements, isotopes, and chemical species.","M/M cont.: All measurements were exactly coregistered in depth.","M/M cont.: The part of the NGRIP2 core they examined was between depths 160 m and 580 m. They got 21,000 low (~9 samples / a --meaning 9 samples per year!) and 48,000 medium (~19 samples / a) slit resolution measurements of lead. This afforded them an effective depth resolution for the HR-ICP-MS measurements of .015 m, equivalent to ~12 samples/a during the Roman era.","M/M cont.: Lead concentration detection limits (defined as 3xs the SD of the blank??) were .01 and .18 pg/g--well below the average NGRIP2 lead concentrations of 1.4 and 3 pg/g during background (1100-1000 BC) and Roman periods. Concentrations ranged from .4 pg/g to more than 20 pg/g during these periods.","M/M cont.: It was *assumed* that total lead comprised three components: 1) crustal lead from windblown dust, 2) volcanic lead largely from quiescent emissions, and 3) pollution lead. Deriving the crustal and voclanic lead levels is an interesting, not too lengthy story. See article for full explanation.","Independent ice core chronology for NGRIP2: Because the NGRIP2 measurements did not extend to the surface, they used the distinct sulfur concentration maximum associated with the well-known Samalas volcanic eruption of 1257 as a signal to synchronize the NGRIP2 with the NEEM_2011_S1 volcanic record. The remaining 162 to 582 m NGRIP2 was independently dated through annual layer counting."),
                             Korean.War_Prager = c("The Korean War, starting just 5 years after WWII ended, was the first major clash between democratic and communist forces.","On June 25th, Soviet-backed communist North Korea crossed the 38th parallel and invaded its US-backed, anti-communist South Korean neighbor. [So they started it.] Within weeks the communists had absorbed nearly the entire country.","The US was confused over whether it should or even could respond. (The US had slashed its military budget at the end of WWII.) The Soviet Union sensed America's lack of resolve and encouraged the North's aggression.","Then Truman rushed troops to Pusan to save the last sliver of unconquered territory.","General Douglas MacArthur pushed further north into North Korean territory in order to unite the entire peninsula [so it was a noble aim].","By the 1980s, South Korea had developed into an economic powerhouse.","Was the Korean war worth the cost (35,000 American lives is the only cost mentioned in the video. The loss of Korean lives is never once brought up.)?","Answer: The natural dividend of saving the South was the evolution of today's democratic and prosperous South Korea. It brought 50 million South Koreans undreamed of freedom and affluence, and it brought the world top flight products. South Korea is a model global citizen and a strong ally of the US, standing in stark contrast to [the hole that is] North Korea. Had it not been for the US, the monstruous Pyongyang regime would now rule all of Korea."," The US's intervention was an effort to save South Koreans. It was also a message that the free world under US leadership would no longer tolerate communist military takeovers of free nations. This deterrent prevented the Soviets from trying similar tactics on Japan, Taiwan, and western Europe.","Finally, the Korean War awakened the US to the dangers of disarmament and isolationism. It led to the bipartisan policy of containment of global communism.","Though not a full victory (didn't unite the peninsula), the Korean War was a victory nonetheless, and not just a military victory, but a moral one as well (it kept half the Korean people free). Want further evidence that it was a moral victory? ...","... evidence: Korea did not have a single material resource that would have benefited America."),
                             Korean.War_basic.facts = c("The Korean War started 25 June 1950, ended 27 July 1953."))


# Scripture references
scriptures.variable = list(one = "filler",
                           drunkenness.Bible.4 = c("Isaiah 5:22","I5:22 - Woe unto them that are mighty to drink wine","1 Corinthians 6:10","1C6:10 - nor ... drunkards shall inherit the kingdom of God","Galatians 5:21","G5:21 - drunkenness ... do such things shall not inherit the kingdom","Ephesians 5:18","E5:18 - be not drunk with wine, wherein is excess"),
                           )


# 71 (70) dichotomous branchings in this tree of life:
TreeLifeList.3 = list(one = "filler",
                      LUCA = c("Eubacteria","Archaea"),
                      Eubacteria = c("Alphaproteobacteria --> mitochondria","All the other bacteria"),
                      Archaea = c("Lokiarchaeota --> Eukarya","All the other archaea"),
                      Eukarya = c("Bikonta --> plants and chromalveolates","Unikonta --> Amoebazoa, Fungi, Choanozoa + Animalia"),
                      Animalia = c("Porifera, Placozoa, Cnidarians","Eumetazoa"),
                      Eumetazoa = c("Ctenophora","Bilateria"),
                      Bilateria = c("Xenacoelomorpha","Nephrozoa --> Kimberella (possibly a protostome?)"),
                      Nephrozoa = c("Protostomes (~million species alive today)","Deuterostomes (~70,000 species alive today--incliuding 66,000 vertebrates)"),
                      Protostomes = c("Spiralia","Ecdysozoa"),
                      Spiralia = c("Rotifers and Platyhelminthes","Mollusks (largest marine phylum--at 23% of all named marine organisms) and Annelids"),
                      Ecdysozoa = c("Priapulids and Kinorhynchs","Nematodes and Panarthropoda"),
                      Panarthropoda = c("Onychophora","Tardigrades and Arthropods"),
                      Mollusca = c("Gastropods (70k)","Bivalves (20k - clams, oysters, scallops, and mussels) and Cephalopods (900)"),
                      Annelida = c("earthworms","leeches and ragworms"),
                      Gastropods = c("snails","slugs"),
                      Cephalopods = c("squid","octopus (and other things: nautilus, cuttlefish"),
                      Deuterostomes = c("Ambulacraria","Chordates"),
                      Ambulacraria = c("Echinoderms --> sea cucumbers, sea stars, sea lilies, sea urchins, sand dollars,","Hemichordates --> acorn worms"),
                      Chordates = c("Cephalochordates --> Lancelet","Olfactores"),
                      Olfactores = c("Urochordates","Vertebrates"),
                      Urochordates = c("tunicates","Appendicularia"),
                      Vertebrates = c("Cyclostomes","Gnathostomes"),
                      Gnathostomes = c("Chondrychthyes","Osteichthyes"),
                      Osteichthyes = c("Actinopterygii (and Teleostomes)","Sarcopterygii"),
                      Sarcopterygii = c("lobe-finned fish","Tetrapods"),
                      Tetrapods = c("Anamniotes (amphibians)","Amniotes"),
                      Amniotes = c("Sauropsids","Synapsids"),
                      Sauropsids = c("Diapsids","?"),
                      Diapsids = c("Lepidosaurs","Archosaurs"),
                      Lepidosaurs = c("lizards (and don't forget tuataras)","snakes"),
                      Archosaurs = c("Pterosaurs","Dinosaurs"),
                      Dinosaurs = c("Ornithischia","Saurischia"),
                      Ornithischia_ = c("Ceratops","Stegasaurs"),
                      Saurischia = c("Theropods (originated in Middle Triassic, ancestrally carnivorous, but also included other -vores; eventually gave rise to birds)","Sauropods"),
                      Sauropods = c("Brachiosaurus and Brontosaurus","Diplodocus and Apatosaurus"),
                      Synapsids = c("Eupelycaosauria","?"),
                      Therapsids = c("Biarmosuchia","Eutherapsids"),
                      Eutherapsids = c("Dinocephalia","Neotherapsids"),
                      Theriodontia = c("Gorgonopsid","Eutheriodontia"),
                      Cynodontia = c("Cynognathia","Probainognathia"),
                      Mammaliaformes = c("examples include Morganucodon, Docodonta --> Castorocauda, and Hadrocodium","Mammalia"),
                      Mammalia = c("Prototheria --> Monotremes","Theria"),
                      Theria = c("Metatheria --> Marsupials","Eutheria --> Placentals"),
                      Placentals = c("Atlantogenata","Boreoeutheria"),
                      Atlantogenata = c("Xenarthra","Afrotheria"),
                      Xenarthra = c("armadillos","anteaters and sloths"),
                      Afrotheria = c("elephant shrews, tenrecs, and aardvarks","elephants, hyraxes and manatees"),
                      Boreoeutheria = c("Laurasiatheria","Euarchontoglires"),
                      Laurasiatheria = c("Eulipotyphla --> moles, shrews, and hedgehogs","Scrotifera"),
                      Scrotifera = c("Chiroptera","Ferungulata"),
                      Ferungulata = c("Euungulata","Ferae"),
                      Euungulata = c("Cetartiodactyla","Perissodactyla"),
                      Cetartiodactyla = c("Cetaceans","Artiodactyls"),
                      Artiodactyls = c("e.g. pigs, peccaries, hippos, camels, llamas","e.g. giraffes, deer, cattle, buffalos, antelopes"),
                      Perissodactyla = c("horses, donkeys, zebras","rhinos ; and tapirs"),
                      Ferae = c("Pholidota --> pangolins","Carnivora"),
                      Carnivora = c("Feliforms","Caniforms"),
                      Feliforms = c("cats","mongooses; hyenas; civets"),
                      Caniforms = c("bears, wolves, wolverines/red pandas/skunks/etc.","Pinnipeds"),
                      Euarchontaglires = c("Glires ... and also treeshrews","Euarchonta"),
                      Glires = c("rodents","lagomorphs"),
                      Euarchonta = c("Dermoptera (flying lemurs, aka colugos)","Primates"),
                      Primates = c("Strepsirrhines --> lemurs and lorises","Haplorhines"),
                      Haplorhines = c("tarsiers","Simiiformes"),
                      Simiiformes = c("Platyrrhines","Catarrhines"),
                      Catarrhines = c("Cercopithicoidea","Hominoidea"),
                      Hominoidea = c("Hylobates --> gibbons","Hominidae"),
                      Hominidae = c("Ponginae","Homininae"),
                      Homininae = c("Gorillas","Hominini"),
                      Hominini = c("Pan","Homo"))




###################################################################

### USMLE step 1 decks:

## biochem lists:
# chromosomal abnormalities
# vitamins and nutrition
USMLE.biochem.chromosomal_abnormalities = list(one = "filler",
                     VonHippel.Lindau_disease = c(""),
                     renal_cell_carcinoma = c(""),
                     adult_polycystic_kidney_disease = c(""),
                     Huntington_disease = c(""),
                     Cri.du.chat_syndrom = c(""),
                     familial_adenomatous_polyposis = c(""),
                     Williams_syndrome = c(""),
                     cystic_fibrosis = c(""),
                     Friedreich_ataxia = c(""),
                     Wilms_tumor = c(""),
                     Patau_syndrome = c(""),
                     Wilson_disease = c(""),
                     Prader.Willi_syndrome = c(""),
                     Angelman_syndrome = c(""),
                     neurofibromatosis_type_1 = c(""),
                     neurofibromatosis_type_2 = c(""),
                     Edward_syndrome = c(""),
                     Down_syndrome = c(""),
                     DiGeorge_syndrome = c(""),
                     fragile_X_syndrome = c(""),
                     X.linked_agammaglobulinemia = c(""),
                     Kleinfelter_syndrome = c(""))

USMLE.biochem.vitamins = list(one = "filler",
                              vitamin.B5 = c("aka: pantothenate","description: ","deficiency: "),
                              Wernicke.Korsakoff = c("SS: ","key concept 1: "," key concept 2: "),
                              dry_beriberi = c("SS: ","etiology: "),
                              vitamin.B9 = c("aka: folic acid","description: ","deficiency: ","key concep 1: ","key concept 2: "),
                              vitamin.B3 = c("aka: niacin","description: ","deficiency: "),
                              Hartnup_disease = c("description: ","key concept: ","tx: "),
                              vitamin.B12 = c("aka: cobalamin","description: ","deficiency: ","key concept 1: ","key concept 2: ","key concept 3: ","dx: "),
                              wet_beriberi = c("SS: ","description: "),
                              vitamin.A = c("aka: retinol","description: ","deficiency: ","acute excess: ","chronic excess: ","key concept: "),
                              vitamin.B7 = c("aka: biotin","description: ","deficiency: ","key concept: "),
                              vitamin.B2 = c("aka:riboflavin","description: ","deficiency: "),
                              vitamin.B1 = c("aka: thiamine","description: ","deficiency: ","dx: "),
                              vitamin.B6 = c("aka: pyridoxine"," description: ","deficiency: ","key concept: "))

USMLE.biochem.pt.presentations.3 = list(zero = "filler",
                                        one = c("3-yo male is found to have enlarged testes, large jaw, and a murmur on examination at his well-child visit.","Fragile X syndrome"),
                                        two = c("17-yo male is found to have elevated cholesterol. His father had an MI at 47 and his brother has been found to have normal cholesterol levels.","Familial hypercholesterolemia (hyperlipidemia type IIA)"),
                                        three = c("A 7-yo male is found to fall below an approprate growth percentile. He also had short arms and legs, a large head, and a prominent forehead.","Achondroplasia"),
                                        four = c("A 29-yo male presents to the clinic for blood in his urine and flank pain. His chart shows an elevated blood pressure on multiple occasions.","Autosomal dominant polycistic kidney disease"),
                                        five = c("A 21-yo female presents to the dermatologist for a reddish rash on her face that has been present since childhood. Her doctor notices that she also has lighter patches of skin on her arms.","Tuberous sclerosis"),
                                        six = c("A 3-yo male is found to have red-brown spots on his iris and brown, flat lesions on his trunk.","Neurofibromatosis type 1"),
                                        seven = c("A newborn male is found to have a small head and jaw, clenched hands and rounded bottoms on his feet. A holosystolic murmur is auscultated on examination.","Edwards syndrome"),
                                        eight = c("A 4-yo male is taken to the doctor for frequent falls. His mother reports that he has been having trouble standing and getting up from his falls.","Duchenne muscular dystrophy"),
                                        nine = c("A 38-yo female complains that her arms feel jerky and uncontrollable. She is adopted but was told that her father had difficulty with his mood and was hospitalized many times for aggressive behavior.","Huntington disease"),
                                        ten = c("A newborn boy with an abnormal 1st trimester ultrasound is found to have a cleft palate and rounded soles on the bottom of his feet.","Patau syndrome"),
                                        eleven = c("A 19-yo male complains of blurred vision. His doctor notes that he is exceptionally tall with long and thin limbs, and his sternum appears to protrude on his chest. He has been previously told that he has a benign cardiac murmur.","Marfan syndrome"),
                                        twelve = c("An 11-yo male is hospitalized with pneumonia. This is his third hospitalization in the last two years. He also has trouble gaining weight and is below average in size for his age.","Cystic fibrosis"))
                                      






