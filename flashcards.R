### The flashcards notepad (apps and decks)


                           ####### The flashcard functions ####### 


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
        Sys.sleep(5)
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

## In alphabetical order by list (deck) name:
# ancient.names.3 (a list of ancient people(s) who were/are known by phonetically distinct names)
# BMList.4.27 (Book of Mormon EME vocab passages)
# climate.two.per.lecture (climate change Great Course lectures)
# PharaohList.3 (a list of 30 famous or interesting pharaohs)
# random.facts.4 (a list of truly random facts--with topic name, two facts about it, and a source)
# random.lists.variable (a list of randoms lists of things; lists can be however long)
# TreeLifeList.3 (68 of the dichotomous trees in the tree of life)


# Ancient peoples known by two phonetically dissimilar names:
ancient.names.3 = list(one = "filler",
                       Phrygians = c("Mushki","The Mushki were an Iron Age people of Anatolia who appear in sources from Assyria but not from the Hittites. Several authors have connected them with the Moschoi of Greek sources and the Georgian tribe of the Meskhi. Josephus Flavius identified the Moschoi with the Biblical Meshech. ***Assyrian sources identify the Western Mushki with the Phrygians,*** but Greek sources clearly distinguish between the Phrygians and the Moschoi. The Encyclopedia of Indo-European Culture notes that the Armenians according to Diakonoff, are then an amalgam of the Hurrian (and Urartians), Luvians and the Proto-Armenian Mushki (or Armeno-Phrygians) who carried their IE language eastwards across Anatolia.According to Greek mythographers,[9] the first Phrygian Midas had been king of the Moschi (Mushki), also known as Bryges (Brigi) in the western part of archaic Thrace.Assyrian sources from the 8th century BC speak of a king Mita of the Mushki, identified with king Midas of Phrygia. The Phrygians were an ancient Indo-European people, initially dwelling in the southern Balkans – according to Herodotus – under the name of Bryges (Briges), changing it to Phryges after their final migration to Anatolia, via the Hellespont. - Wikipedia 'Mushki' and 'Phrygians'"),
)


# 27 (26) items in the BMList flashcard deck:
BMList.4.27 = list(one = "filler",
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


## Random facts to remember (with 4 components, usually the last one is a source. So, 
# a topic name, two facts about it, and a source):
random.facts.4 = list(one = "filler",
                      humans.v.volcanoes_CO2_sources = c("Humans emit ~24 billion tons of CO2 a year","Volcanoes emit ~200 million tons CO2. That means volcanoes emit .83% what humans emit","This is according to the USGS. Also, the federally funded Carbon Dioxide Information Analysis Center says CO2 has gone up year after year, regardless of whether there have been major volcanic eruptions. - from a Scientific American article from probably 2009"),
                      layy.v.lehi_F.M.Cross_plus.sources = c("The connection of the name Lei (classical Arabic Layy, meaning 'bend, twist') with Lehi is based on a linguistic blunder. ... I would not support layy = lehi anymore than I would confuse Lee with Locke.","lyy and lhy cannot be confused in Semitic. The 'h' is a strong laryngeal spirant in Semitic, like the German ch in Buch or Scottish ch in loch.","I read this F M Cross quote from BAR in Jeffrey Chadwick's 2009 Khirbet Beit Lei and the Book of Mormon: An Archaeologist's Evaluation."),
                      Beit.Lei.archaeology_when.was.it.not = c("Was there any ancient Israelite or Jewish settlement at Khirbet Beit Lei during the time of biblical Samson (ca. 1100 BC; Iron Age I) or during the time of the Book of Mormon prophet Lehi (ca. 600 BC; Iron Age II)?","Answer: No. The archaeological survey of Khirbet Beit Lei carried out by Yehuda Dagan in the 1970s revealed no evidence of any Iron Age I or Iron Age II settlement at the site. This was confirmed by the archaeological excavations of Joseph Patrich and Yoram Tsafrir in the 1980s and by the current excavations being carried out by Oren Gutfeld and Yakov Kalman. Not even a single sherd of Iron Age I or II pottery has been found at Khirbet Beit Lei, nor any architectural component from those periods. There was no city, nor town, nor village, nor private estate at Khirbet Beit Lei during the time of Samson or of Lehi.","So could there have been any kind of settlement at Khirbet Beit Lei around 600 BC that could be called a city of Lehi or Beit Lehi, or was there any community at the site around 600 BC in which Lehi might have prophesied? Answer: No city or town; no community at all. No one lived at the site in 600 BC."),
                      Beit.Lei.archaeology_when.was.it.then = c("When was the village at Khirbet Beit Lei an active community?","Answer: All of the archaeologists (mentioned in another flashcard) affirm that the site was first utilized as an oil-pressing complex during the Hellenistic and early Roman periods (ca. 300 BC to AD 70). Underground oil presses and dovecotes have been excavated at the site by Gutfeld and Kalman. Later, during the Byzantine period (fourth to sixth centuries AD), the site was used as a Christian monastic complex with an elaborately decorated chapel, which was excavated by Patrich and Tsafrir. The site seems to have been abandoned thereafter until it was resettled and built up as an Arab village during the Mameluke period (thirteenth to fifteenth centuries AD). The ruins of houses and public buildings from this era have been cleared by Gutfeld and Kalman; pottery remains from the Hellenistic, Roman, Byzantine, and Mameluke periods have been recovered from all over the site.","Was Khirbet Beit Lei ever a Jewish site? Answer: Possibly. According to Gutfeld, the site seems to have been taken over by Jewish forces during the time of John Hyrcanus, around 100 BC. The site may have been used by Jews of Judea from that time until the Roman war against Judea, which culminated in AD 70."),
)

## Random lists of variable lengths:
random.lists.variable = list(one = "filler",
                             intersex.conditions.10 = c("XX CAH (congenital adrenal hyperplasia)","XX progestin-induced virilization","XY AIS (androgen-insensitivity syndrome)","XY 5-ARD (5-alpha reductase deficiency)","XY PMDS (persistent Mullerian duct syndrome)","XY anorchia","XXY (Klinefelter)","XX male (de la Chapelle syndrome)","XY gonadal dysgenesis (Swyer syndrome)","true hermaphroditism"),
                             quantum.mechanics_old.new.schools.17 = c("old - Hendrik Lorentz","old - Max Planck","old - Albert Einstein","new - Niels Bohr","Max Born","Erwin Schrodinger","Arthur Compton","Louis de Broglie","Wolfgang Pauli","Paul Dirac","Werner Heisenberg","Enrico Fermi","Richard Feynman","Julian Schwinger","Sin-Itiro Tomonaga","J Robert Oppenheimer"),
                             pathbreakers.early20th.12 = c("Einstein - 1905 papers on special relativity, Brownian motion, m=E/c^2, and photoelectric effect","Picasso - 1907 Les Demoiselles d'Avignon","Matisse - 1904 Luxe, Calme et Volupte; and 1905 Woman with a Hat","Stravinsky - 1913 Rite of Spring","Schoenberg","Joyce","Eliot","Proust","Diaghilev","Freud","Wittgenstein","and dozens of others"),
                             world.scripture.11 = c("Biyan Lu (Blue Cliff Record) - Zen Classic - 12th century","Wumenguan (Gateless Gate) - Zen Classic - 13th century","Jewish Zohar - 13th century","Rumi's Masnavi - organized around a series of fables - 13th century","Bar-do Thos-grol (Book of the Dead) - Tibetan - 14th century","Mayan Popol Vuh - 16th century","Adi Granth - Sikhs - 17th century","Kitab-i-Iqan (Book of Certitude) - Bahai - 1861","Kitab-i-Aqdas (Most Holy Book) - Bahai - 1873","Hindu Puranas and Epics such as the Mahabharata and Ramayana","Buddhist Jataka tales"),
                             minerals.that.make.helium.5 = c("cleveite","pitchblende","carnotite","monazite","these minerals contain uranium and thorium; the helium is produced as alpha particle emission via radioactive decay"),
                             NT.apocrypha.12 = c("Gospel of Thomas","Gospel of Philip","Apocryphon of James","Pistis Sophia","close candidate for canonization - Epistle of Barnabas","close candidate for canonization - Epistles of Clement","Epistles of Ignatius","Apocalypse of Paul","close candidate for canonization - Shepherd of Hermas","close candidate for canonization - Didache","close candidate for canonization - Apocalypse of Peter (also the only book never accepted as canonical that was commented on by a Church Father","close candidate for canonization - Third Epistle to the Corinthians"),
                             NT.apocrypha.close.candidates.6 = c("Epistle of Barnabas","Epistles of Clement","Shepherd of Hermas","Didache","Apocalypse of Peter","Third Epistle to the Corinthians"),
                             Biblical.Christianity.so.called.10 = c("The following really have no effect on your 'Biblical' Christianity?","Platonism","Aristotelianism","Neoplatonism","Manichaeism","Augustinianism","Averroism","Thomism","Calvinism","modern Fundamentalism"),
                             Rupert.Sheldrake.10dogmasofscience.10 = c("Nature is mechanical; the universe is like a machine; plants and humans and everything can be understood in principle as machines","Matter is unconscious: stars and planets and atoms. Not even humans are conscious.","The laws of nature are fixed: they're the same as they were at the big bang, and they'll stay the same forever into the future","The total amount of matter-energy in the universe is always the same","Nature is purposeless: evolution has no purpose; no other natural process is teleological","Biological heredity is material (he could have just said everything is material according to philosophical materialism","Memories are material modifications in the brain (again, philosophical materialism)","Your mind is in your brain (again, everything is material according to philosophical materialism)","Psychic phenomena are impossible","Mechanistic medicine is the only kind that works--complementary and alternative therapies don't work"))


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







