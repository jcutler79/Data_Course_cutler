
# Script.Research - all new methods and algorithms for research OT, NT, BofM text

# (functions from NT.OT.BofM.research will be used here)

# Libraries:
library(stringr)
library(ggplot2)
library(gridExtra)
library(pracma) # for the strcmp function for comparing two strings to see if they're identical
library(dplyr) # for filtering data frames


# The texts:
load("/Users/jamescutler/Desktop/Data_Course_cutler/The_real_KJNT.RData")
load("/Users/jamescutler/Desktop/Data_Course_cutler/The_real_KJOT.RData")
load("/Users/jamescutler/Desktop/Data_Course_cutler/real_bofm.RData") # Load bkofm, the one with the multiple spaces problem fixed




# Name frequency in the OT

# New get.passages function with very little surrounding text (for names):
get.passages.names = function(aword,abook,todos = TRUE){
  word.greg = gregexpr(pattern = aword,abook)
  if (isTRUE(todos)){
    for (i in 1:length(word.greg[[1]])){
      print(str_sub(abook, word.greg[[1]][i]-15,word.greg[[1]][i]+15))
    } 
  } else{
    comienzo = readline(prompt = "Where to start the range of passages: ")
    finish = readline(prompt = "Where to end the range of passages: ")
    for (i in comienzo:finish){
      print(str_sub(abook, word.greg[[1]][i]-15,word.greg[[1]][i]+15))
    }
  }
}


Anames = "Aarat, high or holy ground
Aaron, a teacher; lofty; mountain of strength, a teacher or lofty
Abba, father
Abaddon, the destroyer; see Apollyon a destroyer, the angel of the bottomless pit (Revelation 🙂
Abagtha, father of the wine-press, a Persian chamberlain, God-given; one of the seven eunuchs in the Persian court of Ahasuerus in charge of the wine
Abana, made of stone; a building, perennial, stony
Abarim, passages; passengers, regions beyond
Abda, a servant; servitude
Abdeel, a vapor; a cloud of God, servant of God
Abdi, my servant
Abdiel, servant of God
Abdon, servant; cloud of judgment, servile
Abednego, servant of light; shining, servant of Nego = Nebo
Abel, a city; mourning, vanity; breath; transitoriness
Abel-beth-maachah, mourning to the house of Maachah, meadow of the house of Maachah, also called ABEL-MAIM
Abel-maim, mourning of waters
Abel-meholah, mourning of sickness, meadow of dancing, or the dancing-meadow
Abel-mizraim, the mourning of Egyptians
Abel-shittim, mourning of thorns, meadow of the acacias
Abez, an egg; muddy
Abi, my father, my father is Jehovah
Abiah, the Lord is my father
Abi-albon, most intelligent father, father of strength, also called ABIEL
Abiasaph, consuming father; gathering, father of gathering, i.e. gathered father of gathering; the gatherer
Abiathar, excellent father; father of the remnant, father of abundance, i.e. liberal, father of abundance, or my father excels
Abib, green fruit; ears of corn, an ear of corn, green fruits
Abidah (or Abida), father of knowledge
Abidan, father of judgment, father of the judge
Abiel, God my father, father (i.e., “possessor”) of God = “pious”
Abiezer (or Abieezer), father of help, father of help, helpful
Abigail, the father’s joy, father, i.e. source, of joy
Abihail, the father of strength, father of, i.e. possessing, strength
Abihu, he is my father, he (God) is my father, father of Him; i.e., “worshipper of God”
Abihud, father of praise; confession, father of renown, famous, father (i.e., “possessor”) of renown
Abijah, the Lord is my father, father (i.e., “possessor or worshipper”) of Jehovah
Abijam, father of the sea, father of the sea; i.e., “seaman”, Abijah or Abijam: my father is Jehovah
Abilene, father of mourning, land of meadows
Abimael, a father sent from God, father of Mael
Abimelech, father of the king
Abinadab, father of a vow, or of willingness father of nobleness; i.e., “noble”
Abinoam, father of pleasantness
Abiram, high father; father of deceit,[00] father of height; i.e., “proud”
Abishag, ignorance of the father
Abishai, the present of my father, father of (i.e., “desirous of”)
Abishalom, father of peace
Abishua, father of salvation, father of welfare; i.e., “fortunate”
Abishur, father of the wall; father of rightness, father of the wall
Abital, the father of the dew; or of the shadow, father of the dew
Abitub, father of goodness,
Abiud, father of praise
Abner, father of light
Abram, high father
Abraham, father of a great multitude
Absalom, father of peace
Accad, a vessel; pitcher; spark
Accho, close; pressed together
Aceldama, field of blood
Achab, brother of the father
Achaia, grief; trouble
Achaicus, a native of Achaia; sorrowing; sad
Achan, or Achar, he that troubles, troubler, valley of trouble
Achaz, one that takes, or possesses
Achbor, a rat; bruising
Achim, preparing; revenging; confirming
Achish, thus it is; how is this
Achmetha, brother of death
Achor, trouble
Achsah, adorned; bursting the veil
Achshaph, poison; tricks
Achzib, liar; lying; one that runs
Adadah, testimony of the assembly
Adah, an assembly, ornament, ornament, beauty
Adaiah, the witness of the Lord
Adaliah, one that draws water; poverty; cloud; death
Adam, earthy; red
Adamah, red earth; of blood
Adami, my man; red; earthy; human
Adar, high; eminent
Adbeel, vapor
Addi, my witness; adorned; prey
Addin, adorned; delicious; voltuous
Addon, basis; foundation; the Lord
Adiel, the witness of the Lord
Adin, Adina, adorned; voltuous; dainty
Adithaim, assemblies; testimonies
Adlai, my witness; my ornament
Admah, earthy; red; bloody
Admatha, a cloud of death; a mortal vapor
Adna, pleasure; delight
Adnah, eternal rest[00]
Adoni-bezek (or Adonibezek), the lightning of the Lord; the Lord of lightning
Adonijah, the Lord is my master
Adonikam, the Lord is raised
Adoniram, my Lord is most high; Lord of might and elevation
Adoni-zedek, justice of the Lord; lord of justice
Adoraim, strength of the sea
Adoram, their beauty; their power
Adrammelech, the cloak, glory, grandeur or power of the king
Adramyttium, the court of death
Adriel, the flock of God
Adullam, their testimony; their prey; their ornament
Adummim, earthy; red; bloody things
Aeneas (or Æneas), praised; praiseworthy
Aenon (or Ænon), a cloud; fountain; his eye
Agabus, a locust; the father’s joy or feast
Agag, roof; per floor
Agar, or Hagar, a stranger; one that fears
Agee, a valley; deepness
Agrippa, one who causes great pain at his birth, king, hero-like
Agur, stranger; gathered together
Ahab, uncle, or father’s brother
Aharah, a smiling brother; a meadow of a sweet savor
Aharhel, another host; the last sorrow; a brother’s sheep
Ahasbai, trusting in me; a grown- brother
Ahasuerus, prince; head; chief
Ahava, essence; being; generation
Ahaz, one that takes or possesses
Ahaziah, seizure; vision of the Lord
Ahi, my brother; my brethren
Ahiah, brother of the Lord
Ahiam, mother’s brother; brother of a nation
Ahian, brother of wine
Ahiezer, brother of assistance
Ahihud, brother of vanity, or of darkness, or of joy, or of praise; witty brother
Ahijah, brother of the Lord
Ahikam, a brother who raises or avenges
Ahilud, a brother born, or begotten
Ahimaaz, a brother of the council
Ahiman, brother of the right hand
Ahimelech, my brother is a king; my king’s brother
Ahimoth, brother of death
Ahinadab, a willing brother; brother of a vow
Ahinoam, beauty of the brother; brother of motion
Ahio, his brother; his brethren
Ahira, brother of iniquity; brother of the shepherd
Ahiram, brother of craft, or of protection
Ahisamach, brother of strength
Ahishahur, brother of the morning or dew; brother of blackness
Ahishar, brother of a prince; brother of a song
Ahithophel, brother of ruin or folly[00]
Ahitub, brother of goodness
Ahlab, made of milk, or of fat; brother of the heart
Ahlai, beseeching; sorrowing; expecting
Ahoah, a live brother; my thorn or thistle
Aholah, his tabernacle; his tent
Aholiab, the tent of the father
Aholibah, my tent, or my tabernacle, in her
Aholibamah, my tabernacle is exalted
Ahumai, a meadow of waters; a brother of waters
Ahuzam, their taking or possessing vision
Ahuzzah, possession; seizing; collecting
Ai, or Hai, mass; heap
Aiah, vulture, raven; an isle; alas, where is it?
Aiath, same as Ai; an hour; eye; fountain
Aijeleth-Shahar (or Aijeleth Shahar), the land of the morning
Ain, same as Aiath
Ajalon, a chain; strength; a stag
Akkub, foot-print; splanting; crookedness; lewdness
Akrabbim, scorpions
Alammelech, God is king
Alemeth, hiding; youth; worlds; on the dead
Alian, high
Alleluia, praise the Lord
Allon, an oak; strong
Allon-bachuth, the oak of weeping
Almodad, measure of God
Almon, hidden
Almon-diblathaim, hidden in a cluster of fig trees
Alpheus, a thousand; learned; chief
Alush, mingling together
Alvah, his rising ; his highness
Amad, people of witness; a prey
Amal, labor; iniquity
Amalek, a people that licks
Aman, mother; fear of them
Amana, integrity; truth; a nurse
Amariah, the Lord says; the integrity of the Lord
Amasa, sparing the people
Amasai, strong
Amashai, the people’s gift
Amashi-ali, same as Amaziah
Ami, mother; fear; people
Amaziah, the strength of the Lord
Aminadab, my people is liberal
Amittai, true; fearing
Ammah, my, or his, people
Ammi, same as Ammah
Ammiel, the people of God
Ammihud, people of praise
Amminadab, my people is liberal[00]
Ammishaddai, the people of the Almighty; the Almighty is with me
Ammizabad, dowry of the people
Ammon, a people; the son of my people
Amnon, faithful and true; tutor
Amok, a valley; a depth
Amon, faithful; true
Amorite, bitter; a rebel; a babbler
Amos, loading; weighty
Amoz, strong; robust
Amplias, large; extensive
Amram, an exalted people; their sheaves; handfuls of corn
Amraphel, one that speaks of secrets
Amzi, strong, mighty
Anab, a grape; a knot
Anah, one who answers; afflicted
Anaharath, dryness, burning, wrath
Anaiah, Jehovah answers
Anak, a collar; ornament
Anamim, a fountain; answer; affliction
Anammelech, answer; poverty of the king
Anani, a cloud; prophecy; divination
Ananias, or Ananiah, the cloud of the Lord
Anathema, separated; set apart
Anathoth, or Anath, answer; song; poverty
Andrew, a strong man, manly
Andronicus, a man excelling others
Anem, or Anen, an answer; their affliction
Aner, answer; song; affliction
Aniam, a people; the strength or sorrow of people
Anim, answerings; singings; afflicted
Anna, gracious; one who gives
Annas, one who answers; humble
Antichrist, an adversary to Christ
Antioch, speedy as a chariot
Antipas, for all, or against all
Antipatris, for, or against the father
Antothijah, answers or songs of the Lord; afflictions
Anub, same as Anab
Apelles, exclusion; separation
Apharsathchites, Apharsites (from a root meaning) dividing or rending
Aphek, Aphekah, strength; a rapid torrent
Aphekah, a city in Judah
Aphik, Aphekah, Aphik ; a rapid torrent
Aphiah, speaking, blowing
Apocalypse, uncovering, revelation
Apocrypha, hidden
Apollonia, perdition, destruction
Apollonius, destroying
Apollos, one who destroys; destroyer
Apollyon, a destroyer,[00] angel of the bottomless pit
Appaim, face; nostrils
Apphia, productive; fruitful
Aquila, an eagle
Ar, awakening; uncovering
Ara, cursing; seeing
Arab, multiplying; sowing sedition; a window; a locust
Arabia, evening; desert; ravens
Arad, a wild ass; a dragon
Arah, the way; a traveler
Aram, highness, magnificence, one that deceives; curse
Aran, an ark; their curse, wild goat
Ararat, the curse of trembling
Araunah, ark; song; joyful cry
Arba, four
Archelaus, the prince of the people
Archippus, a master of horses
Arcturus, a gathering together
Ard, one that commands; he that descends, one that descending, descent
Ardon, ruling; a judgment of malediction
Areli, the light or vision of God
Areopagus, the hill of Mars
Aretas, agreeable, virtuous
Argob, a turf, or fat land
Ariel, altar; light or lion of God
Arimathea, a lion dead to the Lord
Arioch, long; great; tall
Aristarchus, the best prince
Aristobulus, a good counselor, the best counsellor
Armageddon, hill of fruits; mountain of Megiddo
Arnon, rejoicing; sunlight
Aroer, heath; tamarisk
Árpád, the light of redemption
Arphaxad, a healer; a releaser
Artaxerxes, King of Persia; honoured king; great warrior
Artemas, whole, sound
Arumah, high; exalted
Asa, physician; cure
Asahel, creature of God
Asaiah, the Lord hath wrought
Asaph, who gathers together
Asareel, the beatitude of God
Asenath, peril; misfortune
Ashan, smoke
Ashbel, an old fire
Ashdod, effusion; inclination; theft
Asher, happiness
Asherah, a Semitic mother goddess
Ashima, crime; offense[00]
Ashkenaz, a fire that spreads
Ashnah, change
Ashriel, same as Asareel
Ashtaroth, Ashtoreth, flocks; sheep; riches
Ashur, National god of Assyria, an Assyrian city (also who is happy; or walks; or looks, black)
Asia, muddy; boggy
Asiel, the work of God
Askelon, weight; balance; fire of infamy
Asnapper, unhappiness; increase of danger
Asriel, help of God
Assir, prisoner; fettered
Asshurim, liers in want; beholders
Assos, approaching; coming near
Assur, same as Ashur
Assyria, country of Assur or Ashur
Aspim, gatherings
Asyncritus, incomparable
Atad, a thorn
Atarah, a crown
Ataroth, crowns
Ataroth-addar, crowns of power
Ater, left hand; shut
Athach, thy time
Athaiah, the Lord’s time
Athaliah, the time of the Lord
Athlai, my hour or time
Attai, same as Athlai
Attalia, that increases or sends from Attalus
Augustus, increased, augmented venerable
Ava, variation of Eve from Chavvah meaning life, or Ivah, iniquity
Aven, iniquity; force; riches; sorrow
Avim, wicked or perverse men
Avith, wicked, perverse
Azaliah, near to the Lord
Azaniah, hearing the Lord; the Lord’s weapons
Azariah, he that hears the Lord
Azaz, strong one
Azazel, the scape-goat
Azaziah, strength of the Lord
Azekah, strength of walls
Azgad, a strong army; a gang of robbers
Azmaveth, strong death; a he-goat
Azmon, bone of a bone; our strength
Aznoth-tabor, the ears of Tabor; the ears of purity or contrition
Azor, a helper; a court
Azotus, effusion; inclination; theft
Azrael, the Angel of Death
Azriel, help of God
Azrikam, help, revenging
Azubah, forsaken
Azur, he that assists or is assisted
Azzan, their strength
Azzur, he that assists or is assisted"
Anames
Anames = unlist(strsplit(Anames, split = "\n"))
length(Anames)

Anames1 = gsub(",.*","",Anames) # Get rid of description of person - official list
Anames2 = gsub(" .*","",Anames1)
Anames2 = gsub("-","",Anames2) # The searchable version

# tog = gregexpr("Aaron",theOT); length(tog[[1]])
# og = gregexpr("Andrew",theOT); og[[1]][1]

mylens = vector()
og = NULL
for (i in 1:length(Anames2)){
  og[i] = gregexpr(Anames2[i],theOT)
  if (og[i][[1]][1] < 0){
    mylens[i] = 0
  } else{
    mylens[i] = length(og[i][[1]])
  }
}
mylens
Adf = data.frame(names = Anames1,
                 counts = mylens)
class(Adf$names) # it's a factor

filter(Adf, counts == 0) %>% select(names)
# Spellings I need to change based on which names oddly turned up zeros:
## Achab? DOESN'T APPEAR TO BE ANYTHING LIKE ACHAB IN THE OT VIA "A[[:alpha:]]ab"
## Achaz? DOESN'T APPEAR TO BE ANYTHING LIKE ACHAZ IN THE OT VIA "A[[:alpha:]]az"
### Adonizedek to Adonizedec
## Agar? - PROBABLY THE NT VERSION OF HAGAR
### Ahishahur to Ahishahar
## Ahuzzah --> Ahuzam already exists
## AijelethShahar - The chief musician according to Psalm 22 intro (not in the text I have)
## Amashi-ali - NOT SURE WHAT'S GOING ON WITH THIS ONE ... same as Amaziah?
## Aminadab - AMINADAB IS A NT NAME OF AN OT PERSON IN JESUS' GENEALOGY; THERE IS AN AMMINADAB IN THE OT
### Arpad without accents
## Asherah - ALWAYS 'GROVES' IN THE KJB!!
## Aspim? - NO CLUE
## Azrael - Azriel already exists; destroying Angel in the Zohar
get.passages.names("A[[:alpha:]]ur",theOT)
get.passages.names("Haman",theOT)
length(gregexpr("groves",theOT)[[1]])

# The changes below are due to what I end up finding above:
Anames3 = gsub("Adonizedek","Adonizedec",Anames2)
Anames3 = gsub("Ahishahur","Ahishahar",Anames3)
Anames3 = gsub("Árpád","Arpad",Anames3)

mylens2 = vector()
og2 = NULL
for (i in 1:length(Anames3)){
  og2[i] = gregexpr(Anames3[i],theOT)
  if (og2[i][[1]][1] < 0){
    mylens2[i] = 0
  } else{
    mylens2[i] = length(og[i][[1]])
  }
}
mylens2
Adf$counts2 = mylens2

firsttry = data.frame(filter(Adf, counts2 != 0) %>% select(names,counts2) %>% arrange(desc(counts2)))

Anames4 = paste0(Anames3," ")
Anames4[245]

mylens3 = vector()
og3 = NULL
for (i in 1:length(Anames4)){
  og3[i] = gregexpr(Anames4[i],theOT)
  if (og3[i][[1]][1] < 0){
    mylens3[i] = 0
  } else{
    mylens3[i] = length(og[i][[1]])
  }
}
mylens3

Adf$counts3 = mylens3
secondtry = data.frame(filter(Adf, counts3 != 0) %>% select(names,counts3))
'%ni%' = Negate('%in%')
firsttry[which(firsttry$names %ni% secondtry$names),1] # Aman is the one that wasn't in secondtry. Aman is not in the KJ Bible (???).

# Names that were possibly mistaken:
## Ar
## Abi
## Ahi
## Asa
## Ai?
## Amal
## Ara
## Arab and Arabia - duplicates
## 

## MERGING ATTEMPTS:
# left = data.frame(id = c(2:7),
#                   y2 = rnorm(6,100,5))
# right = data.frame(id = rep(1:4, each = 2),
#                    z2 = sample(letters,8, replace = TRUE))
# merge(x=left,y=right, by="id", all.x = TRUE)
# 
# left = data.frame(id = letters[1:10],
#                   counts = sample(1:100,10,replace = FALSE))
# right = data.frame(id = letters[c(1,5,2,3,9,8,4,6,7,10)],
#                    counts = c(10,8,6,19,46,45,25,15,27,50))
# merge(x=left,y=right,by="id", all.x = TRUE)
# head(firsttry,5)
# head(secondtry,5)
# merge(x=firsttry,y=secondtry, by="names", all.x = TRUE)
# merge(x=right,y=left,by="id",all.x = TRUE)
# firsttry$order = 1:nrow(firsttry)
# secondtry$order = 1:nrow(secondtry)
# merged = merge(x=firsttry,y=secondtry,by="order",all.x = TRUE)
# order.for.sec = firsttry %>% arrange(names) %>% select(order) %>% head(274)
# secondtry$order = order.for.sec
# merged = merge(x=firsttry,y=secondtry,by="order",all.y = TRUE)

which(firsttry$names == "Aman")
firsttry = firsttry[-241,]
firsttry$order = 1:nrow(firsttry)
order.for.sec = firsttry %>% arrange(names) %>% select(order)
secondtry$order = order.for.sec
sec2 = secondtry[order(secondtry$orden),]

firsttry = firsttry[,-3]
firsttry$counts3 = sec2$counts3
firsttry$names2 = sec2$names

secondtry[order(secondtry$counts3, decreasing = TRUE),]


#############################################################################
######################## ALL THE NAMES IN THE BIBLE #########################

bibnames = "Aarat, high or holy ground
Aaron, a teacher; lofty; mountain of strength, a teacher or lofty
Abba, father
Abaddon, the destroyer; see Apollyon a destroyer, the angel of the bottomless pit (Revelation 🙂
Abagtha, father of the wine-press, a Persian chamberlain, God-given; one of the seven eunuchs in the Persian court of Ahasuerus in charge of the wine
Abana, made of stone; a building, perennial, stony
Abarim, passages; passengers, regions beyond
Abda, a servant; servitude
Abdeel, a vapor; a cloud of God, servant of God
Abdi, my servant
Abdiel, servant of God
Abdon, servant; cloud of judgment, servile
Abednego, servant of light; shining, servant of Nego = Nebo
Abel, a city; mourning, vanity; breath; transitoriness
Abel-beth-maachah, mourning to the house of Maachah, meadow of the house of Maachah, also called ABEL-MAIM
Abel-maim, mourning of waters
Abel-meholah, mourning of sickness, meadow of dancing, or the dancing-meadow
Abel-mizraim, the mourning of Egyptians
Abel-shittim, mourning of thorns, meadow of the acacias
Abez, an egg; muddy
Abi, my father, my father is Jehovah
Abiah, the Lord is my father
Abi-albon, most intelligent father, father of strength, also called ABIEL
Abiasaph, consuming father; gathering, father of gathering, i.e. gathered father of gathering; the gatherer
Abiathar, excellent father; father of the remnant, father of abundance, i.e. liberal, father of abundance, or my father excels
Abib, green fruit; ears of corn, an ear of corn, green fruits
Abidah (or Abida), father of knowledge
Abidan, father of judgment, father of the judge
Abiel, God my father, father (i.e., “possessor”) of God = “pious”
Abiezer (or Abieezer), father of help, father of help, helpful
Abigail, the father’s joy, father, i.e. source, of joy
Abihail, the father of strength, father of, i.e. possessing, strength
Abihu, he is my father, he (God) is my father, father of Him; i.e., “worshipper of God”
Abihud, father of praise; confession, father of renown, famous, father (i.e., “possessor”) of renown
Abijah, the Lord is my father, father (i.e., “possessor or worshipper”) of Jehovah
Abijam, father of the sea, father of the sea; i.e., “seaman”, Abijah or Abijam: my father is Jehovah
Abilene, father of mourning, land of meadows
Abimael, a father sent from God, father of Mael
Abimelech, father of the king
Abinadab, father of a vow, or of willingness father of nobleness; i.e., “noble”
Abinoam, father of pleasantness
Abiram, high father; father of deceit,[00] father of height; i.e., “proud”
Abishag, ignorance of the father
Abishai, the present of my father, father of (i.e., “desirous of”)
Abishalom, father of peace
Abishua, father of salvation, father of welfare; i.e., “fortunate”
Abishur, father of the wall; father of rightness, father of the wall
Abital, the father of the dew; or of the shadow, father of the dew
Abitub, father of goodness,
Abiud, father of praise
Abner, father of light
Abram, high father
Abraham, father of a great multitude
Absalom, father of peace
Accad, a vessel; pitcher; spark
Accho, close; pressed together
Aceldama, field of blood
Achab, brother of the father
Achaia, grief; trouble
Achaicus, a native of Achaia; sorrowing; sad
Achan, or Achar, he that troubles, troubler, valley of trouble
Achaz, one that takes, or possesses
Achbor, a rat; bruising
Achim, preparing; revenging; confirming
Achish, thus it is; how is this
Achmetha, brother of death
Achor, trouble
Achsah, adorned; bursting the veil
Achshaph, poison; tricks
Achzib, liar; lying; one that runs
Adadah, testimony of the assembly
Adah, an assembly, ornament, ornament, beauty
Adaiah, the witness of the Lord
Adaliah, one that draws water; poverty; cloud; death
Adam, earthy; red
Adamah, red earth; of blood
Adami, my man; red; earthy; human
Adar, high; eminent
Adbeel, vapor
Addi, my witness; adorned; prey
Addin, adorned; delicious; voltuous
Addon, basis; foundation; the Lord
Adiel, the witness of the Lord
Adin, Adina, adorned; voltuous; dainty
Adithaim, assemblies; testimonies
Adlai, my witness; my ornament
Admah, earthy; red; bloody
Admatha, a cloud of death; a mortal vapor
Adna, pleasure; delight
Adnah, eternal rest[00]
Adoni-bezek (or Adonibezek), the lightning of the Lord; the Lord of lightning
Adonijah, the Lord is my master
Adonikam, the Lord is raised
Adoniram, my Lord is most high; Lord of might and elevation
Adoni-zedek, justice of the Lord; lord of justice
Adoraim, strength of the sea
Adoram, their beauty; their power
Adrammelech, the cloak, glory, grandeur or power of the king
Adramyttium, the court of death
Adriel, the flock of God
Adullam, their testimony; their prey; their ornament
Adummim, earthy; red; bloody things
Aeneas (or Æneas), praised; praiseworthy
Aenon (or Ænon), a cloud; fountain; his eye
Agabus, a locust; the father’s joy or feast
Agag, roof; per floor
Agar, or Hagar, a stranger; one that fears
Agee, a valley; deepness
Agrippa, one who causes great pain at his birth, king, hero-like
Agur, stranger; gathered together
Ahab, uncle, or father’s brother
Aharah, a smiling brother; a meadow of a sweet savor
Aharhel, another host; the last sorrow; a brother’s sheep
Ahasbai, trusting in me; a grown- brother
Ahasuerus, prince; head; chief
Ahava, essence; being; generation
Ahaz, one that takes or possesses
Ahaziah, seizure; vision of the Lord
Ahi, my brother; my brethren
Ahiah, brother of the Lord
Ahiam, mother’s brother; brother of a nation
Ahian, brother of wine
Ahiezer, brother of assistance
Ahihud, brother of vanity, or of darkness, or of joy, or of praise; witty brother
Ahijah, brother of the Lord
Ahikam, a brother who raises or avenges
Ahilud, a brother born, or begotten
Ahimaaz, a brother of the council
Ahiman, brother of the right hand
Ahimelech, my brother is a king; my king’s brother
Ahimoth, brother of death
Ahinadab, a willing brother; brother of a vow
Ahinoam, beauty of the brother; brother of motion
Ahio, his brother; his brethren
Ahira, brother of iniquity; brother of the shepherd
Ahiram, brother of craft, or of protection
Ahisamach, brother of strength
Ahishahur, brother of the morning or dew; brother of blackness
Ahishar, brother of a prince; brother of a song
Ahithophel, brother of ruin or folly[00]
Ahitub, brother of goodness
Ahlab, made of milk, or of fat; brother of the heart
Ahlai, beseeching; sorrowing; expecting
Ahoah, a live brother; my thorn or thistle
Aholah, his tabernacle; his tent
Aholiab, the tent of the father
Aholibah, my tent, or my tabernacle, in her
Aholibamah, my tabernacle is exalted
Ahumai, a meadow of waters; a brother of waters
Ahuzam, their taking or possessing vision
Ahuzzah, possession; seizing; collecting
Ai, or Hai, mass; heap
Aiah, vulture, raven; an isle; alas, where is it?
Aiath, same as Ai; an hour; eye; fountain
Aijeleth-Shahar (or Aijeleth Shahar), the land of the morning
Ain, same as Aiath
Ajalon, a chain; strength; a stag
Akkub, foot-print; splanting; crookedness; lewdness
Akrabbim, scorpions
Alammelech, God is king
Alemeth, hiding; youth; worlds; on the dead
Alian, high
Alleluia, praise the Lord
Allon, an oak; strong
Allon-bachuth, the oak of weeping
Almodad, measure of God
Almon, hidden
Almon-diblathaim, hidden in a cluster of fig trees
Alpheus, a thousand; learned; chief
Alush, mingling together
Alvah, his rising ; his highness
Amad, people of witness; a prey
Amal, labor; iniquity
Amalek, a people that licks
Aman, mother; fear of them
Amana, integrity; truth; a nurse
Amariah, the Lord says; the integrity of the Lord
Amasa, sparing the people
Amasai, strong
Amashai, the people’s gift
Amashi-ali, same as Amaziah
Ami, mother; fear; people
Amaziah, the strength of the Lord
Aminadab, my people is liberal
Amittai, true; fearing
Ammah, my, or his, people
Ammi, same as Ammah
Ammiel, the people of God
Ammihud, people of praise
Amminadab, my people is liberal[00]
Ammishaddai, the people of the Almighty; the Almighty is with me
Ammizabad, dowry of the people
Ammon, a people; the son of my people
Amnon, faithful and true; tutor
Amok, a valley; a depth
Amon, faithful; true
Amorite, bitter; a rebel; a babbler
Amos, loading; weighty
Amoz, strong; robust
Amplias, large; extensive
Amram, an exalted people; their sheaves; handfuls of corn
Amraphel, one that speaks of secrets
Amzi, strong, mighty
Anab, a grape; a knot
Anah, one who answers; afflicted
Anaharath, dryness, burning, wrath
Anaiah, Jehovah answers
Anak, a collar; ornament
Anamim, a fountain; answer; affliction
Anammelech, answer; poverty of the king
Anani, a cloud; prophecy; divination
Ananias, or Ananiah, the cloud of the Lord
Anathema, separated; set apart
Anathoth, or Anath, answer; song; poverty
Andrew, a strong man, manly
Andronicus, a man excelling others
Anem, or Anen, an answer; their affliction
Aner, answer; song; affliction
Aniam, a people; the strength or sorrow of people
Anim, answerings; singings; afflicted
Anna, gracious; one who gives
Annas, one who answers; humble
Antichrist, an adversary to Christ
Antioch, speedy as a chariot
Antipas, for all, or against all
Antipatris, for, or against the father
Antothijah, answers or songs of the Lord; afflictions
Anub, same as Anab
Apelles, exclusion; separation
Apharsathchites, Apharsites (from a root meaning) dividing or rending
Aphek, Aphekah, strength; a rapid torrent
Aphekah, a city in Judah
Aphik, Aphekah, Aphik ; a rapid torrent
Aphiah, speaking, blowing
Apocalypse, uncovering, revelation
Apocrypha, hidden
Apollonia, perdition, destruction
Apollonius, destroying
Apollos, one who destroys; destroyer
Apollyon, a destroyer,[00] angel of the bottomless pit
Appaim, face; nostrils
Apphia, productive; fruitful
Aquila, an eagle
Ar, awakening; uncovering
Ara, cursing; seeing
Arab, multiplying; sowing sedition; a window; a locust
Arabia, evening; desert; ravens
Arad, a wild ass; a dragon
Arah, the way; a traveler
Aram, highness, magnificence, one that deceives; curse
Aran, an ark; their curse, wild goat
Ararat, the curse of trembling
Araunah, ark; song; joyful cry
Arba, four
Archelaus, the prince of the people
Archippus, a master of horses
Arcturus, a gathering together
Ard, one that commands; he that descends, one that descending, descent
Ardon, ruling; a judgment of malediction
Areli, the light or vision of God
Areopagus, the hill of Mars
Aretas, agreeable, virtuous
Argob, a turf, or fat land
Ariel, altar; light or lion of God
Arimathea, a lion dead to the Lord
Arioch, long; great; tall
Aristarchus, the best prince
Aristobulus, a good counselor, the best counsellor
Armageddon, hill of fruits; mountain of Megiddo
Arnon, rejoicing; sunlight
Aroer, heath; tamarisk
Árpád, the light of redemption
Arphaxad, a healer; a releaser
Artaxerxes, King of Persia; honoured king; great warrior
Artemas, whole, sound
Arumah, high; exalted
Asa, physician; cure
Asahel, creature of God
Asaiah, the Lord hath wrought
Asaph, who gathers together
Asareel, the beatitude of God
Asenath, peril; misfortune
Ashan, smoke
Ashbel, an old fire
Ashdod, effusion; inclination; theft
Asher, happiness
Asherah, a Semitic mother goddess
Ashima, crime; offense[00]
Ashkenaz, a fire that spreads
Ashnah, change
Ashriel, same as Asareel
Ashtaroth, Ashtoreth, flocks; sheep; riches
Ashur, National god of Assyria, an Assyrian city (also who is happy; or walks; or looks, black)
Asia, muddy; boggy
Asiel, the work of God
Askelon, weight; balance; fire of infamy
Asnapper, unhappiness; increase of danger
Asriel, help of God
Assir, prisoner; fettered
Asshurim, liers in want; beholders
Assos, approaching; coming near
Assur, same as Ashur
Assyria, country of Assur or Ashur
Aspim, gatherings
Asyncritus, incomparable
Atad, a thorn
Atarah, a crown
Ataroth, crowns
Ataroth-addar, crowns of power
Ater, left hand; shut
Athach, thy time
Athaiah, the Lord’s time
Athaliah, the time of the Lord
Athlai, my hour or time
Attai, same as Athlai
Attalia, that increases or sends from Attalus
Augustus, increased, augmented venerable
Ava, variation of Eve from Chavvah meaning life, or Ivah, iniquity
Aven, iniquity; force; riches; sorrow
Avim, wicked or perverse men
Avith, wicked, perverse
Azaliah, near to the Lord
Azaniah, hearing the Lord; the Lord’s weapons
Azariah, he that hears the Lord
Azaz, strong one
Azazel, the scape-goat
Azaziah, strength of the Lord
Azekah, strength of walls
Azgad, a strong army; a gang of robbers
Azmaveth, strong death; a he-goat
Azmon, bone of a bone; our strength
Aznoth-tabor, the ears of Tabor; the ears of purity or contrition
Azor, a helper; a court
Azotus, effusion; inclination; theft
Azrael, the Angel of Death
Azriel, help of God
Azrikam, help, revenging
Azubah, forsaken
Azur, he that assists or is assisted
Azzan, their strength
Azzur, he that assists or is assisted
Baal, master; lord, Lord, “owner” or “lord”, also “husband” (as possessor of the wife); possessor, controller;
Baalah, her idol; she that is governed or subdued; a spouse, mistress
Baalath, a rejoicing; our proud lord, see Baal no. the height of the south
Baalath-Beer, subjected pit Baalah of the well,
Baal-berith, idol of the covenant Covenant lord
Baale, same as Baalath
Baal-gad, idol of fortune or felicity, Lord of fortune
Baal-hamon, who rules a crowd, Place of a multitude
Baal-hermon, possessor of destruction or of a thing cursed, Lord of Hermon
Ball-hanan, the Lord is gracious;
Baali, my idol; lord over me, My lord An appellation of JEHOVAH
Baalim, idols; masters; false gods
Baalis, a rejoicing; a proud lord; lord of joy, rules;
Baal-meon, idol or master of the house
Baal-peor, master of the opening
Baal-perazim, god of divisions
Baal-shalisha, the god that presides over three; the third idol
Baal-tamar, master of the palm-tree
Baal-zebub, god of the fly
Baal-zephon, the idol or possession of the north; hidden; secret
Baana, in the answer; in affliction; affliction;
Baanah, son of grief,
Baara, a flame; purging
Baaseiah, in making; in pressing together; Jehovah is bold;
Baasha, he that seeks, or lays waste; boldness, offensive, he who lays waste;
Babel, confusion; mixture, confusion, gate of God
Babylon, same as Babel, Gate Of The Deity, anointment or consecration or confusion or mixing,
Baca, a mulberry-tree
Bahurim, choice; warlike; valiant
Bajith, a house;
Bakbakkar, diligent searcher;
Bakbuk, a flagon, hollow;
Bakbukiah, wasted by Jehovah, effusion of Jehovah
Balaam, the ancient of the people; the destruction of the people; a pilgrim, devouring, lord of the people;
Baladan, one without judgment;
Balak, who lays waste or destroys;;’
Bamah, an eminence or high place
Barabbas, son of shame, confusion
Barachel, that bows before God
Barachias, same as Barachel
Barak, thunder, or in vain
Barjesus, son of Jesus or Joshua son of Jesus, wise
Barjona, son of a Jonah; of a dove
Barnabas, son of the prophet, or of consolation
Barsabas, son of return; son of rest son of Sabas or rest
Bartholomew, a son that suspends the waters
Bartimeus, son of the honorable
Baruch, who is blessed
Barzillai, son of contempt; made of iron
Bashan, in the tooth, in ivory
Bashemath, perfumed; confusion of death; in desolation
Bathsheba, daughter of oath
Bathsuha, daughter of wealth
Bealiah, the god of an idol; in an assembly
Bealoth, cast under
Bebai, void, empty
Becher, first begotten; first fruits
Bechorath, first fruits
Bedad, alone; solitary
Bedaiah, Bedeiah, the only Lord
Bedan, according to judgment
Beeliada, an open idol
Beelzebub, same as Baalzebub
Beer, a water well|well
Beera, a well; declaring
Beerelim, the well of Elim, or of rains
Beeri, my well
Beer-lahai-roi, the well of him that liveth and seeth me
Beeroth, wells; explaining
Beersheba, the well of an oath; the seventh well
Behemoth, beasts
Bekah, half a shekel
Belah, destroying
Belial, wicked, worthless
Belshazzar, master of the treasure
Belteshazzar, who lays up treasures in secret
Ben, a son
Benaiah, son of the Lord
Ben-ammi, son of my people
Beneberak, sons of lightning
Bene-jaakan, sons of sorrow
Benhadad, son of Hadad, or noise
Benhail, son of strength
Benhanan, son of grace
Benjamin, son of the right hand
Benimi, our sons
Beno, his son
Benoni, son of my sorrow, or pain
Benzoheth, son of separation
Beon, in affliction
Beor, burning; foolish; mad, burning or torch, a torch
Bera, a well; declaring
Berachah, blessing; bending the knee
Berachiah, speaking well of the Lord
Beraiah, the choosing of the Lord
Berea, heavy; weighty
Bered, hail
Beri, my son; my corn
Beriah, in fellowship; in envy
Berith, covenant
Bernice, one that brings victory, bearer of victory,
Berodach-baladan, the son of death
Berothai, wells; a cypress
Berothath, of a well
Besai, a despising; dirty
Besodeiah, counsel of the Lord
Besor, glad news; incarnation
Betah, confidence
Beten, belly
Bethabara, the house of confidence
Bethanath, house of affliction
Bethany, house of song; the house of affliction, house of dates, or house of misery, House of Misery
Betharabah, house of depression (in the sense of ‘desert valley’)
Beth-aram, house of height
Beth-aven, the house of vanity; of iniquity of trouble
Beth-azmaveth, house of death’s strength
Beth-baalmeon, an idol of the dwelling-place
Beth-barah, the chosen house
Beth-birei, the house of my Creator, the house of my health
Beth-car, the house of the lamb
Beth-dagon, the house of corn, or of fish
Beth-diblathaim, house of dry figs
Bethel, the Beth (Hebrew)|house of God
Bethemek, house of deepness
Bether, division, or in the trial
Bethesda, house of pity or mercy
Beth-ezal, a neighbor’s house
Beth-gader, a house for a mouse
Beth-gamul, house of recompense, or of the camel
Beth-haccerem, house of the vineyard
Beth-haran, house of grace
Beth-horon, house of wrath
Beth-lebaoth, house of lionesses
Beth-lehem, (Hebrew) house of bread
Beth-marcaboth, house of bitterness wiped out
Beth-meon, house of the dwelling-place
Beth-millo, Kings :
Beth-nimrah, house of leopards
Beth-palet, house of expulsion
Beth-pazzez, house of dividing asunder
Beth-peor, house of gaping, or opening
Bethphage, house of my month, or of early figs
Beth-phelet, same as Beth-palet
Beth-rapha, house of health
Bethsaida, house of fruits, or of food, or of snares
Bethshan, Beth-shean, house of the tooth, or of ivory, or of sleep
Beth-shemesh, Beth (Hebrew)|house of the sun
Bethuel, filiation of God
Beth-zur, house of a rock
Betonim, bellies
Beulah, married
Bezai, eggs
Bezaleel, in the shadow of God
Bezek, lightning; in the chains
Bezer, vine branches
Bichri, first-born; first fruits
Bidkar, in compunction, or sharp pain
Bigthan, in the press; giving meat
Bigvai, in my body
Bildad, old friendship
Bileam, the ancient of the people; the devourer
Bilgah, ancient countenance
Bilhah, timid
Bilshan, in the tongue
Binea, son of the Lord
Binnui, building
Birsha, an evil; a son who beholds
Bishlam, in peace
Bithiah, daughter or worshiper of the Yah
Bithron, divisions
Bithynia, violent precipitation
Bizjothjah, despite
Blastus, that buds or brings forth
Boanerges, son of thunder
Boaz, a pillar of strength
Bocheru, the first born
Bochim, the place of weeping; or of mulberry-trees
Bohan, in them
Boskath, in poverty
Boson, taking away
Bozez, mud; bog
Bozrah, in tribulation or distress
Bukki, void
Bukkiah, the dissipation of the Lord
Bul, old age; perishing
Bunah, building; understanding
Bunni, building me; my understanding
Buz, despised; plundered
Buzi, my contempt
Cabbon, as though understanding
Cabul, displeasing; dirty
Caesar, one cut out, The surname for all Roman emperors described in the New Testament.
Caiphas, he that seeks with diligence, a searcher
Cain, possession, possessed; acquisition, fabrication
Cainan, Kenan, acquisition;
Calah, favorable; opportunity;
Calcol, nourishing
Caleb, a dog; a crow; a basket; bold, impetuous;
Caleb-Ephratah, see Ephratah
Calneh, our consummation
Calno, our consummation; altogether himself
Calvary, the place of a skull
Camon, his resurrection
Cana, zeal; jealousy; possession
Canaan, merchant; trader; or that humbles and subdues
Candace, who possesses contrition
Capernaum, the field of repentance; city of comfort
Caphtor, a sphere, buckle, or hand
Cappadocia, the same as Caphtor
Carcas, the covering of a lamb
Charchemish, a lamb; as taken away; withdrawn
Careah, bald; ice
Carmel, circumcised lamb; harvest; full of ears of corn
Carmi, my vineyard; lamb of the waters
Carpus, fruit; fruitful
Carshena, a lamb; sleeping
Casiphia, money; covetousness
Casluhim, hopes of life
Cedron, black; sad
Cenchrea, millet; small pulse
Cephas, a rock or stone
Cesar, a name applied to those who are born by Caesarean section
Chalcol, who nourishes, consumes, and sustains the whole
Chaldea, as demons, or as robbers
Charran, a singing or calling out
Chebar, force or strength
Chedorlaomer, roundness of a sheaf
Chelal, as night
Chelub, a basket
Chelluh, all
Chelubai, he altogether against me
Chemarims, black ones
Chemosh, handling; stroking; taking away
Chenaanah, broken in pieces
Chenani, my pillar
Chenaniah, preparation, or disposition, or strength, of the Lord
Chephirah, a young lion covered with his mane or a village protected by walls
Cheran, anger
Cherith, cutting; piercing; slaying
Chesed, as a devil, or a destroyer
Chesil, foolishness
Chesulloth, fearfulness
Chidon, a dart
Chiliab, totality; or the perfection of the father
Chilion, finished; complete; perfect
Chilmad, teaching or learning
Chimham, as they; like to them
Chios, open; opening
Chisleu, Cisleu, Casleu, rashness; confidence
Chislon, hope, trust
Chisloth-tabor, fears; purity
Chittem, those that bruise; gold
Chloe, green herb
Chorazin, the secret; here is a mystery
Chozeba, men liers in wait
Christ, anointed;90 The Anointed One;
Christian, The disciples were first called “Christians” at Antioch on the Orontes (Acts :). The name “Christian” appears in the New Testament at Acts :; Peter :. “Christian” occurs times in the New Testament (Acts :; :; Pet. :).9 Believers called0 The term “Christian” of the party of Christ “brethren,” “the faithful,” “elect,” “saints,” “believers,” to the followers of Jesus
Chun, making ready
Chushan-rishathaim, blackness of iniquities
Chuza, the seer or prophet
Cilicia, which rolls or overturns
Cis, same as Kish
Clauda, a lamentable voice
Claudia, Claudius, lame
Clement, mild; good; merciful
Cleophas, the whole glory
Cnidus, age
Colhozeh, every prophet
Colosse, punishment; correction
Coniah, strength of the Lord
Coos, top, summit
Corinth, Greece, which is satisfied; ornament; beauty
Cornelius, of a horn
Cosam, divining
Coz, a thorn
Cozbi, a liar; sliding away
Crescens, growing; increasing
Crete, carnal; fleshly
Crispus, curled
Cush, Cushan, Cushi, Ethiopians; blackness
Cuth, Cuthah, burning
Cyprus, fair; fairness
Cyrene, a wall; coldness; the floor
Cyrenius, who governs
Cyrus, as miserable; as heir, King of Persia; means the son of truth
Dabareh, the word; the thing; a bee; obedient
Dabbasheth, flowing with honey
Daberath, the word; the thing; a bee; obedient (the same as Dabareh)
Dagon, corn; a fish god worship by human (baby) sacrifice burned to the beating of drums
Dalaiah, the poor of the Lord
Dalmanutha, a bucket; a branch
Dalmatia, deceitful lamps; vain brightness
Dalphon, the house of caves
Damaris, a little woman
Damascus, a sack full of blood; the similitude of burning
Dan, judgment; he that judges
Daniel, judgment of God; God my judge
Dannah, judging
Darah, generation; house of the shepherd or of the companion
Darda, home of knowledge; a pearl of wisdom;
Darius, King of Persia; kind man; he that informs himself of a king;
Darkon, of generation; of possession; bearer of scattering;
Dathan, laws or rites; belonging to law;
David, well-beloved, dear; beloved;King.
Debir, an orator; a word; speaker;
Deborah, word; thing; a bee
Decapolis, containing ten cities
Dedan, their breasts; friendship; a judge; low, their friendship;
Dedanim, the descendants of Dedan
Dekar, force; lance bearer, perforation
Delaiah, the poor of the Lord; Jehovah is deliverer;
Delilah, poor; small; head of hair, Samson’s mistress, languishing, She made him sleep upon her knees, and then called the man who was waiting to help her; who “cut off the seven locks of his head,” and so his “strength went from him.” (See SAMSON)[8]
Demas, popular; ruler of people;
Demetrius, belonging to corn, or to Ceres
Derbe, a sting
Deuel, the knowledge of God
Deuteronomy, repetition of the law
Diana, luminous, perfect
Diblaim, cluster of figs
Diblath, paste of dry figs
Dibon, abundance of knowledge
Dibon-gad, great understanding; abundance of sons
Dibri, an orator
Dibzahab, Dizahab, where much gold is
Didymus, a twin; double
Diklah, Dildah, his diminishing
Dilean, that is poor
Dimon, where it is red
Dimonah, dunghill
Dinah, judgment; who judges
Dinhabah, he gives judgment
Dionysius, divinely touched
Diotrephes, nourished by Jupiter
Dishan, a threshing
Dishon, fatness; ashes
Dodai, Dodanim, beloved
Dodavah, love
Dodo, his uncle
Doeg, careful, who acts with uneasiness
Dophkah, a knocking
Dor, generation, habitation
Dorcas, a female roe-deer
Dothan, the law; custom
Drusilla, watered by the dew
Dumali, silence; resemblance
Dura, generation, habitation (same as Dor)
Eagle, a tearer with the beak properly the griffon vulture or great vulture, so called from its tearing its prey with its beak
Earing, ploughing plough or till
Earnest, pledge
East, which is before or in front of a person
Ebal, ancient heaps
Ebed, a servant; laborer
Ebed-melech, the king’s servant
Eben-Ezer, the stone of help
Eber, one that passes; anger
Ebiasaph, a father that gathers or adds
Ebronah, passage over; being angry
Ecclesiastes, a preacher
Ecclesiasticus, or the Wisdom of Sirach Sirach = Joshua, Joshua, saviour, or whose help is Jehovah Jehovah, I am; the eternal living one Jehovah, self-subsisting
Ed, witness
Eden, pleasure; delight
Eder, a flock
Edom, red, earthy; of blood
Edrei, a very great mass, or cloud
Eglah, heifer; chariot; round
Eglaim, drops of the sea
Eglon, heifer; chariot; round(same as Eglah)
Egypt, that troubles or oppresses; anguish
Felix  -happy, happy, prosperous,
Festus  – festive joyful, festal, prosperous
Fortunatus  -lucky, fortunate, prosperous
Gaal – contempt; abomination
Gaash – tempest; commotion
Gabbai – the back
Gabbatha – high; elevated
Gabriel – God is my strength
Gad – a band; a troop
Gadarenes – men of Gadara , i.e. , a place surrounded or walled
Gaddi – my troop; a kid
Gaddiel – goat of God; the Lord my happiness
Gaius – lord; an earthly man
Galal – a roll – a wheel
Galatia – white; the color of milk
Galeed – the heap of witness
Galilee – cylinder; circuit; district; wheel
Gallim – who heap up; who cover
Gallio – who sucks – or lives on milk
Gamaliel – recompense of God; camel of God
Gammadims – dwarfs
Gamul – a recompense
Gareb – a scab
Garmites – men of Garmi, i.e. , bones , or , my cause
Gatam – their lowing; their touch
Gath – a wine-press
Gath – rimmon, the high wine, press
Gaza – strong; a goat
Gazabar – a treasurer
Gazer – a dividing; a sentence
Gazez – a passing over
Gazzam – the fleece of them
Geba – a hill; cup
Gebal – bound; limit
Geber – manly ,strong
Gebim – grasshoppers; height
Gedaliah – God is my greatness
Geder – Gederah ,Gederoth , a wall
Gederothaim – hedges
Gehazi – valley of sight
Geliloth – rolling ,wheel , heap
Gemalli – wares; a camel
Gemariah – accomplishment or perfection of the Lord
Gennesaret – garden of the prince
Genesis – beginning
Genubath – theft; robbery
Gera – pilgrimage , combat; dispute
Gerar – same as Gera
Gergesenes – those who come from pilgrimage or fight
Gerizim – cutters , hatchets
Gershom – a stranger here
Gershon – his banishment; the change of pilgrimage
Geshur – Geshuri – sight of the valley; a walled valley
Gether – the vale of trial or searching
Gethsemane – a very fat or plentiful vale
Geuel – God’s redemption
Gezer – dividing , sentence
Giah – to guide; draw out; produce; a groan or sigh
Gibbar – strong , manly
Gibbethon – a back; a high house
Gibeah – a hill
Gibeon – hill; cup; thing lifted up
Giddel – great
Gideon – he that bruises or breaks; a destroyer
Gideoni – same as Gideon
Gihon – valley of grace
Gilalai – a wheel
Gilboa – revolution of inquiry
Gilead – the heap or mass of testimony
Gilgal – wheel; rolling; heap
Giloh – he that rejoices; he that overturns
Gimzo – that bulrush (the papyrus) , fertile in sycamores a place fertile in sycamores
Ginath – Ginnetho , protection
Girgashite – who arrives from pilgrimage
Gispa – coming hither
Gittah-hepher , digging; a wine-press
Gittaim – a wine,press
Gittites – men of Gath , i.e. , of a wine-press
Goath – his touching; his roaring
Gob – cistern; grasshopper
Gog – roof; covering
Golan – passage; revolution
Golgotha – a heap of skulls; something skull, shaped
Goliath – passage; revolution; heap
Gomer – to finish; complete
Gomorrah – rebellious people
Goshen – approaching; drawing near
Gozan – fleece; pasture; who nourisheth the body
Gudgodah – happiness
Guni – a garden; a covering
Gur – the young of a beast; a whelp
Gur-baal – the governor’s whelp
Haahashtari – a runner
Habaiah – the hiding of the Lord
Habakkuk – he that embraces; a wrestler
Habazinaiah – a hiding of the shield of the Lord
Habor – a partaker; a companion
Hachaliah – who waits for the Lord
Hachilah – my hope is in her
Hachmoni – a wise man
Hadad – joy; noise; clamor
Hadadezer – beauty of assistance
Hadadrimmon – invocation to the god Rimmon
Hadar – power; greatness
Hadarezer – same as Hadadezer
Hadashah – news; a month
Hadassah – a myrtle; joy
Hadattah – new – NEW HAZOR
Hades – see Hell (the grave or place of the dead) – “brought down to hell” (hades) , i.e. , simply to the lowest debasement – descent of Christ into Hell – the death and burial of Jesus – The adobe of departed spirits
Hadlai – loitering; hindering
Hadoram – their beauty; their power
Hadrach – point; joy of tenderness
Hagab – Hagabah – a grasshopper
Hagar – a stranger; one that fears
Haggai – feast; solemnity
Haggeri – Haggi, a stranger
Haggiah – the Lord’s feast
Haggith – rejoicing
Hai – same as Ai ,heap of ruins
Hakkatan – little
Hakkoz – a thorn; summer; an end
Hakupha – a commandment of the mouth
Halah – a moist table
Halak – part
Halhul – grief; looking for grief
Hali – sickness; a beginning; a precious stone
Hallelujah – praise Jah
Halloesh – saying nothing; an enchanter
Ham – son of Noah|Ham , hot; heat; brown
Haman – noise; tumult
Hamath – anger; heat; a wall
Hamath-zobah – the heat , or the wall , of an army
Hammedatha – he that troubles the law
Hammelech – a king; a counselor
Hammoleketh – the queen
Hammon – heat; the sun
Hamonah – his multitude; his uproar
Hamon-gog – the multitude of Gog
Hamor – an ass; clay; dirt
Hamoth – indignation
Hamul – godly; merciful
Hamutal – the shadow of his heat
Hanameel – the grace that comes from God; gift of God
Hanan – full of grace
Hananeel – grace , or gift ,of God
Hanani – my grace; my mercy
Hananiah – grace; mercy; gift of the Lord
Hanes – banishment of grace
Haniel – the gift of God
Hannah – gracious; merciful; graceful; She that gives
Hannathon – the gift of grace
Hanniel – grace or mercy of God
Hanoch – dedicated
Hanun – gracious; merciful
Hapharaim – searching; digging
Hara – a hill; showing forth
Haradah – well of great fear
Haran – mountainous country
Harran – see Charran
Harbonah – his destruction; his sword
Hareph – winter; reproach
Harhas – anger; heat of confidence
Harhaiah – heat,or anger ,of the Lord
Harhur – made warm
Harim – destroyed; dedicated to God
Harnepher – the anger of a bull; increasing heat
Harod – astonishment; fear
Harosheth – a forest; agriculture; workmanship; deafness; silence
Harsha – workmanship; a wood
Harum – high; throwing down
Harumaph – destruction
Haruphite – slender; sharp
Haruz – careful
Hasadiah – the mercy of the Lord
Hashabiah – the estimation of the Lord
Hashabnah – Hashabniah , the silence of the Lord
Hashem – named; a putting to; ‘the name’ [of God]
Hashub – esteemed; numbered
Hashubah – estimation; thought
Hashum – silence; their hasting
Hashupha – spent; made base
Hasrah – wanting
Hatach – he that strikes
Hathath – fear
Hatita – a bending of sin
Hattil – howling for sin
Hattipha – robbery
Hattush – forsaking sin
Hauran – a hole; liberty; whiteness
Havilah – that suffers pain; that brings forth
Havoth-jair – the villages that enlighten
Hazael – that sees God
Hazaiah – seeing the Lord
Hazar-addar – an imprisoned generation
Hazarenan – imprisoned cloud
Hazargaddah – imprisoned band
Hazar-hatticon – middle village; preparation
Hazarmaveth – dwelling of death
Hazar-shual – a wolf’s house
Hazar-susah – or susim , the hay-paunch of a horse
Hazelelponi – sorrow of countenance
Hazeroth – villages; palaces
Hazezon-tamar – drawing near to bitterness
Hazo – seeing; prophesying
Hazor – court; hay
Heber – one that passes; anger
Hebrews – descendants of Heber
Hebron – society; friendship
Hegai – or Hege – meditation; word; groaning; separation
Helam – their army; their trouble
Helbah – Helbon – milk ,fatness
Heldai – Heleb – Heled, the world; rustiness
Helek – part; portion
Helem – dreaming; healing
Heleph – changing; passing over
Helez – armed; set free
Heli – ascending; climbing up
Helkai – same as Helek
Helkath-hazzurim – the field of strong men ,or of rocks
Helon – window; grief
Heman – their trouble; tumult; much; in great number
Hen – grace; quiet; rest
Hena – troubling
Henadad – grace of the beloved
Henoch – same as Enoch
Hepher – a digger
Hephzibah – my delight is in her
Heres – the son; an earthen pot
Heresh – a carpenter
Hermas – Hermes – Mercury; gain; refuge
Hermogenes – begotten of Mercury
Hermon – anathema; devoted to destruction
Herod – son of a hero
Herodion – the song of Juno
Heshbon – invention; industry
Heshmon – a hasty messenger
Heth – trembling; fear
Hethlon – a fearful dwelling
Hezekiah – strength of the Lord
Hezer – Hezir – a bog; converted
Hezrai – an entry or vestibule
Hezron – the dart of joy; the division of the song
Hiddai – a praise; a cry
Hiel – God lives; the life of God
Hierapolis – holy city
Higgaion – meditation; consideration
Hilen – a window; grief
Hilkiah – God is my portion
Hillel – he that praises
Hinnom – there they are; their riches
Hirah – liberty; anger
Hiram – exaltation of life; a destroyer a very noble person
Hittite – one who is broken; who fears
Hivites – wicked; wickedness
Hizkijah – the strength of the Lord
Hobab – favored; beloved
Hobah – love; friendship; secrecy
Hod – praise; confession
Hodaiah – the praise of the Lord
Hodaviah – Hodiah , Hodijah , same as Hodaiah
Hodesh – a table; news
Hoglah – his festival or dance
Hoham – woe to them
Holon – a window; grief
Homam – making an uproar
Hophin – he that covers; my fist
Hophra – biblical name of Pharaoh Apries
Hor – who conceives , or shows; a hill
Horeb – desert; solitude; destruction
Horem – an offering dedicated to God
Hor-hagidgad – the hill of felicity
Hori – a prince; freeborn
Horims – princes; being angry
Hormah – devoted or consecrated to God; utter destruction
Horonaim – angers; ragings
Horonites – men of anger, or of fury , or of liberty
Hosah – trusting
Hosanna – save I pray thee; keep; preserve
Hosea – Hoshea – savior; safety
Hoshaiah – the salvation of the Lord
Hoshama – heard; he obeys
Hotham) – a seal
Hothir – excelling; remaining
Hukkok – engraver; scribe; lawyer
Hul – pain; infirmity
Huldah – the world
Hupham – their chamber; their bank
Huppim – a chamber covered; the sea-shore
Hur – liberty; whiteness; hole
Huram – their liberty; their whiteness; their hole
Huri – being angry; or same as Huram
Hushah – hasting; holding peace
Hushai – their haste; their sensuality; their silence
Hushathite – Hushim , man of haste , or of silence
Huz – counsel; woods; fastened
Huzoth – streets; populous
Huzzab – molten
Hymeneus – nuptial; the god of marriage
Ibhar – election, he that is chosen, he will choose, chooser, God does choose,
Ibleam – ancient people, people decreasing
Ibneiah- Ibniah, the building of the Lord, the understanding of the Lord, son by adoption, God builds -Jehovah does build,
Ibnijah  – whom Jehovah will build up, God builds, Jehovah is builder,
Ibri  -passing over, being angry, being with young, Hebrew, passing over of a Hebrew,
Ibsam  -fragrant
Ibzan – father of a target, father of coldness, splendid, active
Ichabod -where is the glory? or, no glory, inglorious, the glory is not, where is the glory, inglorious,
Iconium -coming,
Idalah -the hand of slander, or of cursing,
Idbash -flowing with honey, the land of destruction, honey,sweet, corpulent,
Iddo -his band, power, praise, God’s friend, affectionate, festal, his power,
Idumea -red, earthy, bloody,
Igal -redeemed, defiled, may God redeem, deliverer, he will vindicate,
Igeal -a redeemer, redeemed, defiled
Igdaliah- the greatness of the Lord, may God be glorified, great is Jehovah,
Iim -heaps of Hebrews, or of angry men
Ije -abarim, heaps of Hebrews, or of passers over
Ijon -look, eye, fountain
Ikkesh -forward, wicked, stubbord, perverse, subtle,
Ilai – exalted, supreme,
Illyricum – joy, rejoicing
Imlah – plenitude, circumcision, full, God does fill, fulfilling, plenitude,
Imla -whom God will fill up, replenisher
Immanuel – God with us, God is with is, Hebrew c. 8th century
Immer -saying, speaking, a lamb talkative, prominent,
Imna – God does restrain, withdrawing,
Imnah – same as Jimnah may God defend, prosperity, he allots,
Imrah – a rebel, waxing bitter, changing, a rebel,stubborn, height of Jehovah,
Imri – speaking, exalting, bitter, a lamb, projecting, eloquent,
India -praise, law
Iphedeiah -redemption of the Lord, may God redeem, Jehovah does deliver, redemption,
Ir- watchman, city, vision, watcher,
Ira -watchman, making bare, pouring out, watcher, watchful, city watch
Irad – wild ass, heap of empire, dragon, fleet,
Iram -the effusion of them, a high heap, watchful,
Iri – fire, light, Jehovah is watcher,
Irijah – the fear of the Lord, may God see, God does see, provide, fear of the Lord,
Ir -shemesh, a city of bondage
Iru, watch,
Isaac – laughter, he laughed, laughing one,
Isaiah -the salvation of the Lord, God’s salvation, Jehovah is helper, salvation is of the Lord,
Iscah – he that anoints, who looks,
Iscariot – a man of murder, a hireling, man of kerioth,
Ishbah – praising, He praises, appeaser,
Ishbak -who is empty, exhausted, free, empty, exhausted,
Ishb -benob, respiration, conversion, taking captive, man sitting in Nob, dweller on the mount -he that predicts,
Ishbosheth -a man of shame,
Ishi -salvation, saving, my help, saving,
Ishiah  -it is the Lord, Jehovah exists, forgiveth,
Ishma- named, marveling, desolation, distinction, elevated,
Ishmael -God that hears, hears,
Ishmaiah- hearing or obeying the Lord, may God hear, Jehovah hears,
Ishmerai -keeper, or keeping, God guards, God keeps,
Ishod -a comely man, famed[18] man of honor, man of splendor,
Ish -pan, hid, broken in two, firm, strong,
Ishtob -good man
Ishua -plainness, equal,
Ishuah -Isuah, equal, self,satisfied,
Ishui- Jesui, equality,
Ishvah -resembles,
Ishvi -quiet,*Ishvah, resembles,
Ishmachiah -cleaving to the Lord, Jehovah supports, may God support,*Ishvah, resembles,
Ismaiah -Jehovah hears,
Ispah – a jasper stone
Israel -who prevails with God, he strives with God, ruling with God
Issachar  -reward, recompense, rewarded,
Isshiah  -there is God,
Isshijah  -there is God,
Isui  -same as Ishuah
Ithai -strong, my sign, a plowshare, God is with me,
Italy -abounding with calves or heifers
Ithamar -island of the palm,tree, palm,coast, palm tree,
Ithiel -sign, or coming of God, God is with me, God is, God is with me,
Ithmah -an orphan, purity, bereavement,
Ithra -excellent,
Ithran -remaining, searching out diligently,
Ithream -excellence of the people, populous, remnant, abundance of the people,
Ittah -kazin, hour, or time, of a prince,
Ittai -with me, plowman, living,
Iturea -guarded, mountainous,
Ivah -iniquity,
Izehar -clearness, oil,
Izhar  -Izehar, oil, bright one, olive oil,
Izrahiah -the Lord ariseth, the clearness of the Lord, may God shine forth, Jehovah is appearing, does arise,
Izri -fasting, tribulation, creative, former,
Izziah -Jeziah, Jehovah exalts,
Jaakan – tribulation,labor he twists ,he shall surround, intelligent
Jaakobah- supplanter, deceiver, the heel
Jaala – ascending, a little doe or goat,wild goat
Jaalam – hidden, young man, heir,whom God hides, concealer, he will be hid
Jaana i- answering, afflicting, making poor, whom Jehovah answers, mourner, Jehovah answers
Jaasau – doing, my doing, whom Jehovah made, fabricator, Jehovah makes,
Jaasiel – God’s work, whom God comforts, made by God, God is maker
Jaasu – created
Jaazaniah – whom the Lord will hear, whom Jehovah hears, Jehovah does hear, may God hear’
Jaazah – Jaazar, Jazer, assistance, helper, Jehovah helps
Jaaziah – Jaaziel, the strength of the Jehovah, sprinkling of the Lord, whom Jehovah expidates – God consoles or determines, may God strengthen,
Jaaziel – God is determining or consoling, may God strengthen,
Jabal  – which glides away, stream, “the father of such as dwell in tents and have cattle” (Genesis 4:20). This description indicates that he led a wandering life. A shepherd a river, moving  – or which glides away, stream,
Jabbok  – evacuation, dissipation, wrestling
Jabesh – dryness, confusion, shame, a dry place, dry,
Jabez  – sorrow, trouble, he makes sorrow or height, sorrow,
Jabin  – Jabneh, whom God observes, discerner, the wise, God discerns or intelligent, he understands
Jabneel  – building of God, built by God
Jachan-wearing out, oppressing, afflicting or troublous, affliction,
Jachin – he that strengthens and makes steadfast, he shall establish, he does establish or founding,established
Jacob  – that supplants, undermines, the heel, supplanter, one who follows on another’s heels , supplanter, he that supplants or follows after, supplanted,
Jada – knowing, wise
Jadau- his hand, his confession, favorite or friend
Jadon  – he will judge, thankful, he that rules or abids,
Jaddua – known, very knowing,
Jael – a goat, agile
Jagur – husbandman, stranger
Jah – the everlasting abbreviation of Jehovah
Jahaleel , praising God, light of God
Jahath – broken in pieces, descending, revival or grasping
Jahaz  -Jahazah,quarrel, dispute
Jahaziah – Jahzeiah, the vision of the Lord, Jehovah reveals, Jehovah sees,
Jahaziel  -seeing God, whom God watches over, beheld by God, God sees or reveals
Jahdai  – guide or he directs
Jahdiel  – the unity, or sharpness, or revenge, of God, union of God or God makes glad
Jahdo  – I alone, his joy, his sharpness of wit, his newness, union,
Jahleel  – waiting for,or beseeching, or hope in, God, God waits or God does grievously afflict
Jahmai  – warm, making warm, Jehovah protects,
Jahzeel  – Jahziel, God hasteth, or divideth, God apportions or distributes,
Jahzerah  – Jehovvah protects or may he lead back,
Jair  – my light, who diffuses light, Jehovah enlightens, arouses or who diffuses light
Jairus  – He will enlighten or diffuse light
Jakan – same as Achan
Jakeh  – pious or hearkening
Jakim  – rising, confirming, establishing
Jalon  – tarrying, murmuring, abiding, lodger
Jambres- poverty, bitter, a rebel
James  – same as Jacob, the Greek form of Jacob, supplanter (to take the place of another, as through force, scheming, strategy, or the like)
Jamin  – right hand,south wind
Jamlech – reigning, asking counsel
Janna – Jannes, who speaks or answers, afflicted, poor
Janoah- Janohah, resting, tarrying, deriving, rest
Janum- sleeping
Japhet – enlarged, fair, persuading
Japheth – same as Japhet
Japhia-  enlightening, appearing
Japhlet- Japhleti, delivered, banished
Japho- fairness,comeliness
Jarah-a wood, honeycomb, watching closely
Jareb -a revenger
Jared – a ruling, commanding, coming down
Jaresiah – the bed of Lord, the Lord hath taken away,poverty, whom Jehovah nourishes
Jarib – fighting, chiding, multiplying, avenging
Jarmuth- fearing, or seeing, or throwing down, death
Jarvah-breathing, or making, a sweet smell
Jashem -Jashen, ancient, sleeping
Jasher -righteous, upright
Jashobeam- the people sitting, or captivity of the people
Jashub- a returning, a controversy, a dwelling place
Jasiel- the strength of God, whom God made
Jason, he that cures, He that will cure, one who will heal
Jathniel- gift of God, whom God gives
Jattir- a remnant, excellent
Javan- deceiver, one who makes sad
Jazeel- strength of God
Jazer- assistance, helper, Jehovah helps
Jaziz- brightness, departing
Jearim- a leap, woods
Jeaterai- whom Jehovah searching out, leads, whom Jehovah leads
Jeberechiah- speaking well of, or kneeling to, Jehovah, whom Jehovah blesses
Jebus- treading under foot, manger
Jebusi- trodden under foot, mangers
Jecamiah -resurrection, or confirmation, or revenge, of the Lord, whom Jehovah gathers
Jecoliah- perfection, or power, of Jehovah, able through Jehovah, The same as Jecholiah: strong through Jehovah
Jeconiah- preparation, or stability, of Jehovah
Jed, God’s friend
Jedaiah – the hand of the Lord, confessing Jehovah
Jedeiah- one Lord, the joy of the Jehovah
Jediael- knowledge, of God
Jedidah- well beloved, amiable
Jedidiah- beloved of the Lord, beloved of Jehovah
Jediel- the knowledge, or renewing, of God
Jeduthun-his law, giving praise
Jeezer- island of help
Jegar -sahadutha, heap of witness
Jehaleleel- Jehalelel, praising God, clearness of God
Jehaziel- same as Jahaziel
Jehdeiah-one Lord, the joy of the Lord,

Jeheiel-God liveth
Jehezekel -strength of God
Jehiah- the Lord liveth, Jehovah lives
Jehiskiah- the strength, the strength, or taking, of the Lord
Jehoadah- passing over, testimony of the Lord, whom Jehovah adorns
Jehoaddan- pleasure, or time, of Jehovah
Jehoahaz- possession of Jehovah
Jehoash- fire of Jehovah, Jehovah,given
Jehohanan- grace,whom Jehovah gave, a name of which John is the contraction.
Jehoiachin- preparation, or strength, of Jehovah
Jehoiada- knowledge of Jehovah
Jehoiakim- avenging, or establishing, or resurrection, of Jehovah
Jehoiarib- fighting, or multiplying, of Jehovah
Jehonadab- Jonadab, free giver, liberality
Jehonathan-altation of Jehovah
Jehoshaphat- the Lord is judge, whom Jehovah judges, Jehovah,judged
Jehosheba- fullness, Jehovah’s oath, Jehovah,swearing
Jehoshua- same as Joshua
Jehovah- self,subsisting, I am, the eternal living one, to be, exist, to be, to become, I am who am- hath sent me, I am who am with you,
Jehovah -jireh, the Lord will provide, Jehovah will see, i.e., will provide
Jehovah -nissi, the Lord my banner, Jehovah my banner
Jehovah -shalom, the Lord send peace, Jehovah send peace
Jehovah -shammah, Jehovah is there, the Lord is there
Jehovah -tsidkenu, Jehovah our righteousness, the Lord our righteousness
Jehozabad -Jehovah dowry, having a dowry Jehovah,given, whom Jehovah gave
Jehu-himself who exists
Jehubbah -hiding, binding
Jehucal -mighty, perfect, wasted
Jehud   -Jehudi, praising, conferring
Jehudijah -the praise of the Lord
Jehush -keeping counsel, fastened
Jekabzeel -the congregation of God
Jekamean -the people shall arise
Jekamiah -establishing, or revenging, of Jehovah, whom Jehovah gathers
Jekuthiel -hope, or congregation, of Jehovah
Jemima -handsome as the day
Jemuel -God’s day, son of God
Jephthah -Whom God sets free
Jephunneh -he that beholds
Jerah -the moon, month, smelling sweet
Jerahmeel -the mercy, or the beloved, of God
Jered -ruling, coming down
Jeremai -my height, throwing forth waters
Jeremiah -exaltation of Jehovah, raised up or appointed by Jehovah, whom Jehovah has appointed
Jeremoth -eminences, one that fears death
Jeriah- fear, or throwing down, of Jehovah
Jerebai -fighting, chiding, multiplying
Jericho -his moon, his month, his sweet smell
Jeriel -fear, or vision of God
Jerijah -same as Jeriah
Jerimoth -he that fears or rejects death
Jerioth -kettles, breaking asunder
Jeroboam -he that opposes the people
Jeroham – high, merciful, beloved
Jerubbaal – let Baal contend with him
Jerubbesheth – let the idol of confusion defend itself
Jeruel – fear, or vision of God
Jerusalem  -vision of peace, the habitation of peace,
Jerusha – banished, possession, inheritance
Jesaiah  – health, or salvation, of Jehovah, salvation of Jehovah
Jeshebeab  – sitting, or captivity, of the father
Jesher – right, singing
Jeshimon – solitude, desolation
Jeshishai -ancient, rejoicing exceedingly
Jeshohaia -Jehovah pressing, the meditation of God
Jeshua -same as Joshua
Jesiah- sprinkling of the Lord
Jesimiel- naming -or astonishment, of God
Jesse -gift, oblation, one who is
Jesui-even,tempered, flat country
Jesus- savior -deliverer, The Greek form of the name Joshua or Jeshua, a contraction of Jehoshua – that is, help of Jehovah or saviour. Latin: Jesus, Iesus, Iesu, Josue. Greek: Ieous from Hebrew Yeshua. Also means safety, victory and who’s help is Jehovah or it may be from the verb “Yasha”, “to save,” and = Jehovah Savior, or simply Savior, a late form of Hebrew “yehosua”, the meaning of which is “YHWH is salvation” or “YHWH saves/has saved.” Online definition of “savior.” Latin term drove out Old English “hæland” which means “healer” as the preferred descriptive term for Jesus.
Jether – he that excels
Jetheth- giving
Jethlah – hanging up, heaping up
Jethro – his excellence, his posterity
Jetur  – order, succession, mountainous
Jeuel – God hath taken away, God heaping up
Jeush – Jeuz, he that is devoured
Jew-same as Judah, a man of Judea,[ then  name derived from the patriarch Judah, at first given to one belonging to the tribe of Judah or to the separate kingdom of Judah ( 2 Kings 16:6 , 25:25 , Jeremiah 32:12 , 38:19 , 40:11 , 41:3 )
Jezaniah- nourishment, or weapons, of Jehovah, whom Jehovah hears
Jezebel-not exalted
Jezer-island of help, power
Jeziah-Jeziel, sprinkling of Jehovah, whom Jehovah expiates
Jezoar- clear, white
Jezrahiah  – Jehovah arises, brightness of Jehovah, produced by Jehovah, a leader of the choir
Jezree l – seed of God
Jibsam- their drought, their confusion
Jidlaph-he that distills water
Jimnah-right hand, numbering, preparing
Jiphtah- opening
Jiphthael- God opening
Joab -paternity, voluntary
Joachim – rising or establishing of Jehovah
Joah – fraternity, brother of Jehovah
Joahaz – apprehending, possessing, seeing
Joanna – grace or gift of Jehovah, whom Jehovah has graciously given, grace or gift of God
Joash- who despairs or burns
Joatham – same as Jotham
Job – he that weeps or cries, persecuted
Jobab – sorrowful, hated
Jochebed – glorious, honorable
Joed – witnessing, robbing, passing over
Joel – he that wills or commands
Joelah- lifting up, profiting, taking away slander
Joezer – he that aids
Jogbehah – an exalting, high
Jogli – passing over , turning back, rejoicing
Joha (or ”Juha”), who enlivens or gives life
Johanan – who is liberal or merciful, whom Jehovah graciously bestows gift or grace of God, Jehovah is or has been gracious.
John- the grace or mercy of the Lord, Jehovah’s gift: the same name as Johanan, a contraction of Jehohanan
Joiarib — chiding, or multiplying, of Jehovah
Jokdeam-crookedness, or burning, of the people
Jokim- that made the sun stand still
Jokmeam – confirmation, or revenge, of the people
Jokneam – possessing, or building up, of the people
Jokshan -an offense, hardness, a knocking
Joktan – small dispute, contention, disgust
Jonadab – who gives liberally
Jonah -or Jonas, a dove, he that oppresses, destroyer
Jonan – a dove, multiplying of the people
Jonathan- given of God
Joppa – beauty, comeliness, Beauty
Jorah – Jorai, showing, casting forth, a cauldron
Joram – to cast, elevated
Jordan – the river of judgment, Some translate it as “the descender,” from the Semitic yrd, “to descend”
Jorim- he that exalts the Lord
Josabad – having a dowry
Josaphat -same as Jehoshaphat
Jose – raised, who pardons
Joseph – increase, addition, remover or increaser, increase, may God add
Joses – same as Jose, exalted
Joshah  – being, forgetting, owing
Joshaviah – the seat, alteration, or captivity of Jehovah
Joshbekesha- it is requiring or beseeching
Joshua – a savior, a deliverer
Josiah – the Lord burns, the fire of Jehovah
Josibiah- the seat, or captivity of Jehovah
Josiphiah – increase of Jehovah, Jehovah’s finishing
Jotham  – the perfection of Jehovah
Jothath  – Jothatha, his goodness
Jozabad – same as Josabad
Jozachar – remembering, of the male sex
Jubal – he that runs, a trumpet
Jucal – mighty, perfect
Judah – the praise of the Lord, confession, praised, celebrated, praise,
Judas – Jude, same as Judah
Judaea – Judea, same as Judah
Judith- same as Judah
Julia – downy, soft and tender hair
Julius – same as Julia
Junia-  youth
Jushabhesed , dwelling,place, change of mercy
Justus- just or upright, just
Juttah- turning away
Kabzeel – The congregation of God
Kadesh – Kadesh-Barnea, holiness, Kadesh,Barnea means holiness of an inconstant son
Kadmiel – God of antiquity, God of rising
Kadmonites – Ancients; chiefs
Kallai – Light, resting by fire; my voice
Kamon – Name of Place in Book of Judges 10:06
Kanah – Of reeds
Kareah – Bald; ice
Karkaa – Floor; dissolving coldness
Karkor – They rested
Karnaim – Horns
Kartah – Calling, Meeting
Kedar – Blackness, Sorrow
Kedemah – Oriental, Ancient, First
Kedemoth – Antiquity,Old age
Kehelahath – A whole, Congregation
Keiiah – She that divides, She that cuts
Keilah – Fortress; enclosed; sling. Let the faint be alienated.
Kelaiah – Voice of the Lord,Gathering together
Kelitah – Same as Kelaiah
Kemuel – God hath raised up, God has established him
Kenah- Buying, Possession
Kenan- Buyer; Owner
Kenaz – This purchase, This lamentation
Kenites – Possession, Purchase, Lamentation
Kenizzites – Possession, Purchase
Keren-happuch – The horn, Child of beauty
Kerioth – The cities; Callings
Keros – Crooked- Crookedness
Keturah – Incense, That makes the incense to fume
Kezia – Superficies- Angle, Cassia
Keziz – End, Extremity
Kibroth -hattaavah,The graves of lust
Kibzaim – Congregation
Kidron – Obscure,Making black, Making sad
Kinah – Same as Kenah
Kir – City, Wall, Meeting
Kir -haraseth,Kirharesh; City of the sun,Wall of burnt brick
Kirioth – Same as Kerioth
Kirjath – City; vocation; meeting
Kirjathaim- The two cities; callings; or meetings
Kirjath -arba- City of four; Fourth city
Kirjath -arim- City of those who watch
Kirjath -baal- City of Baal,City of a ruler
Kirjath -huzoth- City of streets, Populous city
Kirjath -jearim- City of woods
Kirjath -sannah- City of enmity, City of a blackberry bush
Kirjath -sepher- City of letters, City of the book
Kish – Hard- Difficult- Straw; for age
Kishi – Hardness- His gravity, His offense
Kishion – Hardness; soreness
Kishon – Hard; sore
Kithlish – It is a wall; the company of a lioness
Kitron – Making sweet; binding together
Kittim – Breaking; bruising small; gold; coloring
Koa – Hope- Congregation, a line; a rule
Kohath – Congregation, wrinkle; bluntness
Kolaiah – Voice of Jehovah
Korah – Baldness- ice, frost
Kushaiah – Same as Kishi
Laadah, to assemble together; to testify; passing over
Laadan, for pleasure; devouring; judgment
Laban, white; shining; gentle; brittle
Labana, the moon; whiteness; frankincense
Lachish, who walks, or exists, of himself
Lael, to God; to the mighty
Lahad, praising; to confess
Lahairoi, who liveth and seeth me
Lahmam, their bread; their war
Lahmi, my bread; my war
Laish, a lion
Lakum, fortification
Lamech, poor; made low
Laodicea, just people
Lapidoth, enlightened; lamps
Lasea, thick; wise
Lasha, fissure
Lashah, to call; to anoint
Lazarus, assistance of God
Leah, weary; tired
Lebanon, white, incense
Lebaoth, lividness
Lebbeus, a man of heart; praising; confessing
Lebonah, same as Labana
Lecah, progress
Lehabim, flames; inflamed; swords
Lekah, walking; going
Lemuel, God with them, or him
Leor, to me light, light me up
Leshem, a name; putting; a precious stone
Letushim, hammermen; filemen
Leummim, countries; without water
Levi, associated with him joined, to adhere, adhesion
Libnah, white; whiteness
Libni, same as Libnah
Likhi, learned
Lilith, demoness of the night
Libya, the heart of the sea; fat
Linus, net
Lior, same as Leor
Lmri, eloquent
Lo-ammi, For him, my people
Lod, nativity; generation
Lois, better
Lo-ruhamah, for him, mercy
Lot, Lotan, wrapt up; hidden; covered; myrrh; rosin
Lubin, heart of a man; heart of the sea
Lucas, Lucius, luminous; white
Lucifer, bringing light
Lud, Ludim, same as Lod
Luhith, made of boards
Luke, luminous; white, light-giving
Luz, separation; departure; an almond
Lycaonia, she-wolf
Lydda, Lydia, a standing pool
Lysanias, that drives away sorrow
Lysias, dissolving
Lysimachus, scattering the battle
Lystra, that dissolves or disperses
Maachah- pressed down; worn; fastened
Maachathi- broken
Maadai- pleasant; testifying
Maadiah- pleasantness; the testimony of the Lord A priest
Maai- belly; heaping up
Maale-akrabbim- ascent of scorpions
Maarath- den; making empty; watching
Maaseiah- the work of the Lord
Maasiai- the defense- or strength- or trust of the Lord
Maath- wiping away; breaking; fearing; smiting
Maaz- wood; wooden
Macedonia- burning; adoration- extended land
Machbenah- Machbanai- poverty; the smiting of his son
Machi- poor; a smiter- decrease
Machir- selling; knowing
Machnadebai- smiter
Machpelah- double
Madai- a measure; judging; a garment
Madian- judgment; striving; covering; chiding
Madmannah- measure of a gift; preparation of a garment
Madon- a chiding; a garment; his measure
Magbish- excelling; height
Magdala- tower; greatness
Magdalene- a person from Magdala
Magdiel- declaring God; chosen fruit of God
Magog- covering; roof; dissolving- region of Gog
Magpiash- a body thrust hard together
Mahalah- Mahalath- sickness; a company of dancers; a harp
Mahaleleel- praising God
Mahali- infirmity; a harp; pardon
Mahanaim- tents; two fields; two armies
Mahanehdan- tents of judgment
Mahanem- a comforter
Maharai- hasting; a hill; from a hill
Mahath- same as Maath
Mahavites- declaring a message; marrow
Mahaz- an end; ending; growing hope
Mahazioth- seeing a sign; seeing a letter
Maher-shalal-hash-baz- making speed to the spoil; he hastens to the prey
Mahlah- Mahli- Mahlon- same as Mahali
Makas- same as Mahaz
Makheloth- assemblies; congregations
Makkedah- worshiping; burning; raised; crookedness
Malachi- my messenger; my angel
Malcham- Malchom- their king; their counselor
Malchijah- also Malchiah or Melchiah- the Lord my king- or my counselor
Malchiel- God is my king- or counselor
Malchus- my king- kingdom- or counselor
Maleleel- same as Mahaleleel
Mallothi- fullness; circumcision
Malluch- reigning; counseling
Mammon- riches
Mamre- rebellious; bitter; set with trees
Manaen- a comforter; a leader
Manahethites- my lady; my prince of rest
Manasseh- forgetfulness; he that is forgotten- forgetting- who makes to forget””- “God hath made me forget”
Manoah- rest; a present
Maon- house; place of sin
Mara- Marah- sad- bitter
Maralah- sleep; a sacrifice of myrrh; ascension
Maranatha- the Lord is coming
Mareshah- from the beginning; an inheritance
Mark same as Marcus- polite; shining- the evangelist
Maroth- bitterness
Marsena- bitterness of a bramble
Martha- who becomes bitter; provoking a lady
Mary- same as Miriam
Mash- same as Meshech
Mashal- a parable; governing
Masrekah- whistling; hissing
Massa- a burden; prophecy- burden a lifting up- gift
Massah- temptation
Matred- wand of government
Matri- rain; prison
Mattan- Mattana- Mattenai- gifts; rains
Mattaniah- gift- gift of Jehovah- Original name of Zedekiah- king of Judah-
Mattatha- his gift- An ancestor of Jesus
Mattathias- the gift of the Lord
Matthan- same as Mattan- gift
Matthanias- same as Mattaniah
Matthal- gift; he that gives
Matthew- given; a reward- gift of Jehovah- Also called LEVI- gift of God
Mazzaroth- the twelve signs of the zodiac
Meah- a hundred cubits
Mearah- den; cave; making empty
Mebunnai- son; building; understanding
Mecherath- selling; knowledge
Medad- he that measures; water of love
Medan- judgment; process
Medeba- waters of grief; waters springing up
Media- measure; habit; covering
Megiddo- his precious fruit; declaring a message
Megiddon- same as Megiddo
Mehetabel- how good is God
Mehida- a riddle; sharpness of wit
Mehir- a reward
Mehujael- who proclaims God
Mehuman- making an uproar; a multitude
Mejarkon- the waters of Jordan
Mekonah- a foot of a pillar; provision
Melatiah- deliverance of the Lord
Melchi- my king; my counsel
Melchiah- God is my king
Melchi-shua- king of health; magnificent king
Melchizedek- king of justice
Melea- supplying; supplied
Melech- king; counselor
Melita- affording honey
Mellicu- his kingdom; his counselor
Melzar- circumcision of a narrow place- steward
Memphis- abode of the good
Memucan- impoverished; to prepare; certain; true
Menahem- comforter; who conducts them; preparation of heat
Menan- numbered; rewarded; prepared
Mene- who reckons or is counted
Meonenim- charmers- regarders of times
Mephaath- appearance- or force- of waters
Mephibosheth- out of my mouth proceeds reproach dispeller of shame.
Merab- he that fights or disputes
Meraioth- bitterness; rebellious; changing
Merari- bitter; to provoke
Mered- rebellious- ruling
Meremoth- bitterness; myrrh of death
Meres- defluxion; imposthume
Meribah- dispute; quarrel
Meribaal- he that resists Baal; rebellion
Merodach- bitter contrition
Merodach-baladan- bitter contrition- without judgment
Merom- eminences; elevations
Meronothite- my singing; rejoicing; bearing rule
Meroz- secret- leanness
Mesha- burden; salvation
Meshach- that draws with force
Meshech- who is drawn by force
Meshelemiah- peace- or perfection- of the Lord
Meshezaheel- God taking away; the salvation of God
Meshillamith- peaceable; perfect; giving again
Mesobaite- the Lord’s standing-place; a little doe
Mesopotamia- between two rivers
Messiah- anointed
Metheg-ammah- bridle of bondage
Methusael- who demands his death
Methuselah- when he dies it shall be sent
Meunim- dwelling-places; afflicted
Mezahab- gilded
Miamin- the right hand
Mibhar- chosen; youth
Mibsam- smelling sweet
Mibzar- defending; forbidding; taking away
Micah- poor; humble
Micaiah- who is like to God?
Micha- who is like to God?
Michael- who is like to God?
Michaiah- who is perfect?
Michal- who is perfect?
Michmash- he that strikes
Michmethah- the gift or death of a striker
Michri- selling
Michtam- golden psalm
Middin- judgment; striving
Midian- judgment; covering; habit
Migdal-el- tower of God
Migdal-gad- tower compassed about
Migdol- a tower
Migron- fear; farm; throat
Mijamin- right hand
Mikloth- little wants; little voices; looking downward
Minneiah- possession of the Lord
Milalai- circumcision; my talk
Milcah- queen
Milcom- their king
Miletum- red; scarlet
Millo- fullness
Miniamin- right hand
Minni- reckoned; prepared
Minnith- same as Minni
Miriam- rebellion- rebellous- bitterness their rebellion
Mishael- who is asked for or lent
Mishal- parables; governing
Misham- their savior; taking away
Misheal- requiring; lent; pit
Mishma- hearing; obeying
Mishmannah- fatness; taking away provision
Mishraites- spread abroad
Mispar- Mispereth- numbering; showing; increase of tribute
Misrephoth-maim- hot waters
Misti- uncomparable beauty
Mithcah- sweetness; pleasantness
Mithnite- loin; gift; hope
Mithredath- breaking the law
Mitylene- purity; cleansing; press
Mizar- little
Mizpah- Mizpeh- a watch-tower; speculation
Mizraim- tribulations
Mizzah- defluxion from the head
Mnason- a diligent seeker; an exhorter
Moab- of his father
Moladah- birth; generation
Molech- Moloch- king
Molid- nativity; generation; begetter;
Mordecai- contrition; bitter; bruising; dedicated to Mars- a little man or bitter bruising;
Moreh- stretching
Moriah- bitterness of the Lord
Moserah- Moseroth- erudition; discipline
Moses- taken out; drawn forth to draw drawn; drawn forth- taken out of water or a son;
Mozah- unleavened
Muppim- out of the mouth; covering
Mushi- he that touches- that withdraws or takes away
Myra- I flow; pour out; weep
Mysia- criminal; abominable
Naam- fair; pleasant
Naaman- pleasantness
Naamah- can refer to a figure in Genesis- the wife of Solomon or a demon- beautiful; agreeable
Naarah- Naarai- young person
Naaran- juvenile- boyish- juvenile
Naashon- that foretells; that conjectures
Naasson- enchanter
Nabal- fool; senseless
Naboth- words; prophecies
Nachon- ready; sure
Nachor- same as Nahor
Nadab- free and voluntary gift; prince
Nagge- clearness; brightness; light
Nahaliel- inheritance; valley of God
Nahallal- praised; bright
Naham- Nahamani- comforter; leader
Naharai- my nostrils; hot; anger
Nahash- snake; serpent
Nahath- rest; a leader
Nahbi- very secret
Nahor- hoarse; dry; hot
Nahshon- same as Naashon
Nahum- comforter; penitent
Nain- beauty; pleasantness
Naioth- beauties; habitations
Naomi- beautiful; agreeable; sweet; pleasant
Naphish- the soul; he that rests- refreshes himself- or respires
Naphtali- that struggles or fights
Narcissus- astonishment; stupidity
Nason- helper; entry-way
Nathan- given; giving; rewarded
Nathanael- the gift of God
Nathan-melech- the gift of the king- or of counsel
Naum- same as Nahum
Nazareth- separated; crowned; sanctified
Nazarite- one chosen or set apart
Neah- moved; moving
Neapolis- the new city
Neariah- child of the Lord
Nebai- budding; speaking; prophesying
Nebaioth- words; prophecies; buds
Neballat- prophecy; budding
Nebat- that beholds
Nebo- that speaks or prophesies
Nebuchadrezzar- tears and groans of judgment
Nebushasi hahban- speech; prophecy; springing; flowing
Nebuzar-adan- fruits or prophecies of judgment
Necho- lame; beaten
Nedabiah- prince or vow of the Lord
Neginoth- stringed instruments
Nehelamite- dreamer; vale; brook
Nehemiah- consolation; repentance of the Lord
Nehum- comforter; penitent
Nehushta- made of brass
Nehushtan- a trifling thing of brass
Neiel- commotion- or moving- of God
Nekoda- painted; inconstant
Nemuel- the sleeping of God
Nepheg- weak; slacked
Nephish- same as Naphish
Nephishesim- diminished; torn in pieces
Nephthalim- same as Naphtali
Nephthoah- opening; open
Nephusim- same as Nephishesim
Ner- a lamp; new-tilled land
Nereus- same as Ner
Nergal- the great man; the hero
Nergal-sharezer- treasurer of Nergal
Neri- my light
Neriah- light; lamp of the Lord
Nethaneel- same as Nathanael
Nethaniah- the gift of the Lord
Nethinims- given or offered
Neziah- conqueror; strong
Nezib- standing-place
Nibhaz- budding; prophesying- the barker
Nibshan- prophecy; growing of a tooth
Nicanor- a conqueror; victorious
Nicodemus- victory of the people
Nicolas- same as Nicodemus
Nicolaitanes- followers of Nicolas
Nicopolis- the city of victory
Niger- black
Nimrah- Nimrim- leopard; bitterness; rebellion
Nimrod- rebellion (but probably an unknown Assyrian word)
Nimshi- rescued from danger
Nineveh- handsome; agreeable
Nisan- standard; miracle
Nisroch- flight; proof; temptation; delicate
No- stirring up; forbidding
Noadiah- witness- or ornament- of the Lord
Noah- (pronounced No-ach) rest; consolation
Noah- motion (Zelophehad’s daughter)
Nob- discourse; prophecy
Nobah- that barks or yelps
Nod- vagabond; fugitive
Nodab- vowing of his own accord
Noe- same as Noah
Nogah- brightness; clearness
Noha- rest; a guide
Non- posterity; a fish; eternal
Noph- honeycomb; anything that distills or drops
Nophah- fearful; binding
Norah- same as Noah
Nun- same as Non
Nymphas- spouse; bridegroom
Obadiah- servant of the Lord
Obal- inconvenience of old age
Obed- a servant; workman
Obed-edom- servant of Edom
Obil- that weeps; who deserves to be bewailed
Oboth- dragons; fathers; desires
Ocran- a disturber; that disorders
Oded- to sustain- hold or lift up
Og- a cake; bread baked in ashes
Ohad- praising; confessing
Ohel- tent; tabernacle; brightness
Olympas- heavenly
Omar- he that speaks; bitter
Omega- the last letter of the Greek alphabet; long O
Omri- sheaf of corn
On- pain; force; iniquity
Onan- same as On
Onesimus- profitable; useful
Onesiphorus- who brings profit
Ono- grief or strength or iniquity of him
Ophel- a tower; darkness; small white cloud
Ophir- fruitful region
Ophni- wearisomeness; folding together
Ophrah- dust; lead; a fawn
Oreb- a raven
Oren- pine tree
Ornan- that rejoices
Orpah- the neck or skull
Oshea- same as Joshua
Othni- my time; my hour
Othniel- the hour of God
Ozem- that fasts; their eagerness
Ozias- strength from the Lord
Ozni- an ear; my hearkening
Paarai- opening
Padan-aram- cultivated field or table-land
Padon- his redemption; ox-yoke
Pagiel- prevention- or prayer- of God
Pahath-Moab- ruler of Moab
Pai- Pau- howling; sighing- blessing-
Palal- thinking
Palestina- which is covered; watered; or brings and causes ruin
Pallu- marvelous; hidden
Palti- deliverance; flight
Paltiel- deliverance; or banishment- of God
Pamphylia- a nation made up of every tribe
Paphos- which boils- or is very hot
Parah- a cow; increasing
Paran- beauty; glory; ornament
Parbar- a suburb
Parmashta- a yearling bull
Parmenas- that abides- or is permanent
Parnach- a bull striking- or struck
Parosh- a flea; the fruit of a moth
Parshandatha- given by prayer
Paruah- flourishing; that flies away
Pasach- thy broken piece
Pasdammin- portion or diminishing of blood
Paseah- passing over; halting
Pashur- that extends or multiplies the hole; whiteness
Patara- trodden under foot
Pathros- Pathrusim- mouthful of dough; persuasion of ruin
Patmos- mortal
Patrobas- paternal; that pursues the steps of his father
Pau- same as Pai
Paul- small; little
Paulus- same as Paul
Pedahzur- strong or powerful savior; stone of redemption
Pedaiah- redemption of the Lord
Pekah- he that opens; that is at liberty
Pekahiah- it is the Lord that opens
Pekod- noble; rulers
Pelaiah- the Lord’s secret or miracle
Pelaliah- entreating the Lord
Pelatiah- let the Lord deliver; deliverance of the Lord
Peleg- division
Pelethites- judges; destroyers
Pelonite- falling; secret
Peniel- face or vision of God; that sees God
Peninnah- pearl; precious stone; the face
Pentapolis- five cities
Pentateuch- the five books of Moses
Pentecost- fifty count or the counting of fifty days
Penuel- same as Peniel
Peor- hole; opening
Perazim- divisions
Peresh- horseman
Perez- divided
Perez-Uzza- division of Uzza- or of strength
Perga- very earthy
Pergamos- height; elevation
Perida- separation; division
Perizzites- dwelling in villages
Persia- that cuts or divides; a nail; a gryphon; a horseman
Persis- same as Persia
Peruda- same as Perida
Peter- a rock or stone
Pethahiah- the Lord opening; gate of the Lord
Pethuel- mouth of God; persuasion of God
Peulthai- my works
Phalec- same as Peleg
Phallu- Pallu- admirable; hidden
Phalti Palti- deliverance- flight
Phanuel- face or vision of God
Pharaoh- that disperses; that spoils
Pharez- division; rupture
Pharisees- set apart
Pharpar- that produces fruit
Phebe- shining; pure
Phenice- Phoenicia- red; purple
Phichol- the mouth of all- or every tongue
Philadelphia- love of a brother
Philemon- who kisses
Philetus- amiable; beloved
Philip- warlike; a lover of horses lover of horses
Philippi- same as Philip- in the plural
Philistines- those who dwell in villages
Philologus- a lover of letters- or of the word A Christian at Rome to whom Paul sends his salutation. Salute Philologus- and Julia- Nereus- and his sister- and Olympas- and all the saints which are with them.
Phinehas- bold aspect; face of trust or protection
Phlegon- zealous; burning
Phrygia- dry; barren
Phurah- that bears fruit- or grows
Phygellus- fugitive
Phylacteries- things to be especially observed
Pi-beseth- abode of the goddess Bahest or Bast
Pi-hahiroth- the mouth; the pass of Hiroth
Pilate- armed with a dart
Pinon- pearl; gem; that beholds
Piram- a wild ass of them
Pirathon- his dissipation or deprivation; his rupture
Pisgah- hill; eminence; fortress
Pisidia- pitch; pitchy
Pison- changing; extension of the mouth
Pithom- their mouthful; a dilatation of the mouth
Pithon- mouthful; persuasion
Pochereth- cutting of the mouth of warfare
Pontius- marine; belonging to the sea
Pontus- the sea
Poratha- fruitful
Potiphar- bull of Africa; a fat bull
Potipherah- that scatters abroad- or demolishes- the fat
Prisca- ancient
Priscilla- same as Prisca
Prochorus- he that presides over the choirs
Puah- mouth; corner; bush of hair
Publius- common
Pudens- shamefaced
Pul- bean; destruction
Punites- beholding; my face
Punon- precious stone; that beholds
Pur- lot- singular of Purim (lots- as in Cleromancy [casting of lots])
Putiel- God is my fatness
Puteoli- sulphureous wells
Quartus- fourth
Quaternion- a guard of four soldiers- …and delivered him to four quaternions of soldiers to guard him…
Quicksands- The Greater Sytis …fearing lest they should be cast upon the Syrtis…
Quirinius- who governs Syria- often called Cyrenius
Raamah- greatness; thunder; some sort of evil
Raamiah- thunder- or evil- from the Lord
Rabbah- great; powerful; contentious
Rabbi- Rabboni- my master
Rabbith- multitude
Rabboni- lord- teacher
Rabmag- who overthrows or destroys a multitude
Rab-saris- chief of the eunuchs
Rabshakeh- cup-bearer of the prince
Raca- worthless; good-for-nothing
Rachab- same as Rahab
Rachal- to whisper; an embalmer; a village of the tribe of David
Rachel- ewe- God’s ewe
Raddai- ruling; coming down
Ragau- friend; shepherd
Raguel- shepherd- or friend of God
Rahab- proud; quarrelsome (applied to Egypt)
Rahab- large; extended (name of a woman)
Raham- compassion; a friend
Rakem- flower garden
Rakkath- empty; temple of the head
Rakkon- vain; void; mountain of enjoyment
Ram- elevated; sublime
Ramah- same as Ram
Ramath- Ramatha- raised; lofty
Ramathaim-Zophim- the two watch-towers
Ramath-lehi- elevation of the jaw-bone
Ramath-mizpeh- elevation of the watch-tower
Ramiah- exaltation of the Lord
Ramoth- eminences; high places
Raphah- Raphu- relaxation; physic; comfort
Reaiah- vision of the Lord
Reba- the fourth; a square; that lies or stoops down
Rebekah- fettered by beauty
Rechab- square; chariot with team of four horses
Reelaiah- shepherd or companion to the Lord
Regem- that stones or is stoned; purple
Regemmelech- he that stones the king; purple of the king
Rehabiah- breadth- or extent- of the Lord
Rehob- breadth; space; extent
Rehoboam- who sets the people at liberty
Rehoboth- spaces; places
Rehum- merciful; compassionate
Rei- my shepherd; my companion; my friend
Reins- kidneys
Rekem- vain pictures; divers picture
Remaliah- the exaltation of the Lord
Remmon- greatness; elevation; a pomegranate-tree
Remphan- prepared; arrayed
Rephael- the physic or medicine of God
Rephaiah- medicine or refreshment of the Lord
Rehpaim- giants; physicians; relaxed
Rephidim- beds; places of rest
Resen- a bridle or bit
Reu- a friend
Reuben- who sees the son; the vision of the son
Reuel- friend of God
Reumah- lofty; sublime
Rezeph- pavement; burning coal
Rezin- good-will; messenger
Rezon- lean; small; secret; prince
Rhegium- rupture; fracture
Rhesa- will; course
Rhoda- a rose
Rhodoks- to serve
Rhodes- same as Rhoda
Ribai- strife
Riblah- quarrel; greatness to him
Rimmon- exalted; pomegranate
Rinnah- song; rejoicing
Riphath- remedy; medicine; release; pardon
Rissah- watering; distillation; dew
Rithmah- juniper; noise
Rizpah- bed; extension; a coal
Rogelim- a foot or footman
Rohgah- filled or drunk with talk
Romamti-ezer- exaltation of help
Roman- strong; powerful
Rome- strength; power
Rosh- the head; top- or beginning
Reuben- behold a son
Rufus- red
Ruhamah- having obtained mercy
Rumah- exalted; sublime; rejected
Ruth- friend
Sabaoth – rest day
Sabeans – captivity; conversion; old age
Sabtah – a going about or circuiting; old age
Sabtechah – that surrounds; that causes wounding
Sacar – wares; a price
Sadducees – followers of Sadoc, or Zadok
Sadoc – or Zadok – just; righteous
Salah – mission; sending
Salamis – shaken; test; beaten
Salathiel – asked or lent of God
Salcah – thy basket; thy lifting up
Salem – complete or perfect peace
Salim – foxes; fists; path
Sallai – Sallu – an exaltation; a basket
Salma – peace; perfection
Salmon – peaceable; perfect; he that rewards
Salome – same as Salmon – femine version
Samaria – watch -mountain
Samlah – his raiment; his left hand; his astonishment
Samos – full of gravel
Samothracia – an island possessed by the Samians and Thracians
Samson – his son; his service; there the second time
Samuel – lent of God; heard by God; asked of God
Sanballat – bramble,bush; enemy in secret
Sanhedrin – sitting together
Sansannah – bough or bramble of the enemy
Saph – rushes; sea -moss
Saphir – delightful
Sapphira – that relates or tells
Sarah – lady; mother of the multitude
Sarai – my lady; my princess
Sardis – prince of joy
Sardites – removing a dissension
Sarepta – a goldsmith’s shop
Sargon – who takes away protection
Sarid – remaining; hand of a prince
Saron – same as Sharon
Sarsechim – master of the wardrobe
Saruch – branch; layer; lining
Satan – contrary; adversary; enemy; accuser
Saul – demanded; lent; ditch; death
Sceva – disposed; prepared
Seba – a drunkard; that turns
Sebat – twig; scepter; tribe
Sebia – Latin language|Latin form of Zibiah found in the Douay,Rheims Bible|Douay, Rheims
Secacah – shadow; covering; defense
Sechu – defense; bough
Secundus – second
Segub – fortified; raised
Seir – Seirath, hairy; goat; demon; tempest
Sela – a rock
Sela -hammah,lekoth, rock of divisions
Selah – the end; a pause
Seled – affliction; warning
Seleucia – shaken or beaten by the waves
Sem – same as Shem
Semachiah – joined to the Lord
Semaiah – obeying the Lord
Semei – hearing; obeying
Senaah – bramble; enemy
Seneh – same as Senaah
Senir – bed -candle; changing
Sennacherib – bramble of destruction
Seorim – gates; hairs; tempests
Sephar – book; scribe; number
Sepharad – a book descending
Sepharvaim – the two books; the two scribes
Serah – lady of scent; song; the morning star
Seraiah – prince of the Lord
Seraphim – burning; fiery
Sered – dyer’s vat
Sergius – net
Serug – branch; layer; twining
Seth – put; who puts; fixed
Sethur – hid; destroying
Shaalabbim – understanding, or son of a fox
Shaalbim – that beholds the heart
Shaalbonite – a fox’s building
Schaaph – fleeing; thinking
Shaaraim – gates; valuation; hairs
Shaashgaz – he that presses the fleece; that shears the sheep
Shabbethai – my rest
Shachia – protection of the Lord
Shadrach – tender – nipple
Shage – touching softly; multiplying much
Shalem – same as Salem
Shalim – same as Salim
Shalisha – three; the third; prince; captain
Shallum – perfect; agreeable
Shalmai – my garment
Shalman – peaceable; perfect; that rewards
Shalmaneser – peace; tied; chained; perfection; retribution
Shamariah – throne or keeping of the Lord
Shamayim – the heavens
Shamed – destroying; wearing out
Shamer – keeper; thorn; dregs
Shamgar – named a stranger; he is here a stranger
Shamhuth – desolation; destruction
Shamir – Shamer, prison; bush; lees; thorn
Shammah – hear, take notice, listen
Shammai – my name; my desolations
Shammoth – names; desolations
Shammuah – he that is heard; he that is obeyed
Shamsherai – there a singer or conqueror
Shapham – Shaphan, rabbit; wild rat; their lip; their brink
Shaphat – judge
Sharai – my lord; my prince; my song
Sharar – navel; thought; singing
Sharezer – overseer of the treasury – or of the storehouse
Sharon – plain – as in land
Shashai – rejoicing; mercy; linen
Shashak – a bag of linen; the sixth bag
Shaul – Saul – asked; lent; a grave
Shaveh – the plain; that makes equality
Shealtiel – same as Salathiel
Sheariah – gate of the Lord; tempest of the Lord
Shear -jashub, the remnant shall return
Sheba – oath
Shebam – compassing about; old men
Shebaniah – the Lord that converts, or recalls from captivity
Shebarim – breakings; hopes
Sheber – breaking; hope
Shebna – who rests himself; who is now captive
Shebuel – turning, or captivity, or seat – of God
Shecaniah – habitation of the Lord
Shechem – part; portion; back early in the morning
Shedeur – field of light; light of the Almighty
Shehariah – mourning or blackness of the Lord
Shelah – that breaks; that unties; that undresses
Shelemiah – God is my perfection; my happiness; my peace
Sheleph – who draws out
Shelesh – captain; prince
Shelomi – Shelomith – my peace; my happiness; my recompense
Shelumiel – same as Shelemiah
Shem – name; renown
Shema – hearing; obeying
Shemaiah – that hears or obeys the Lord
Shemariah – God is my guard
Shemeber – name of force; name of the strong
Shemer – guardian; thorn
Shemida – name of knowledge; that puts knowledge
Sheminith – eighth (traditionally explained as an eight, stringed instrument – though more likely an octave)
Shemiramoth – the height of the heavens
Shemuel – appointed by God
Shen – tooth; ivory; change
Shenazar – treasurer of a tooth
Shenir – lantern; light that sleeps
Shephatiah – the Lord that judges
Shephi – beholder; honeycomb; garment
Shepho – desert
Shephuphan – serpent
Sherah – flesh; relationship
Sherebiah – singing with the Lord
Sheshach – bag of flax or linen
Sheshai – six; mercy; flax
Sheshan – lily; rose; joy; flax
Sheshbazzar – joy in tribulation; joy of the vintage
Shethar – putrefied; searching
Shethar -boznai – that makes to rot; that seeks those who despise me
Sheva – vanity; elevation; fame; tumult
Shibboleth – Sibboleth – ear of corn; stream or flood
Shibmah – overmuch captivity – or sitting
Shicron – drunkenness; his gift; his wages
Shiggaion – a song of trouble or comfort
Shihon – sound; wall of strength
Shihor -libnah, blackness of Libnah
Shilhi – Shilhim, bough; weapon; armor
Shillem – peace; perfection; retribution
Shiloah – same as Siloah
Shiloh – sent
Shiloh (name of a city), peace; abundance
Shilom – tarrying; peace -maker
Shilshah – three; chief; captain
Shimeah – Shimeath – that hears – or obeys; perdition
Shimei – Shimi, that hears or obeys; my reputation; my fame
Shimeon – same as Simeon
Shimma – same as Shimeah
Shimon – providing well; fatness; oil
Shimrath – hearing; obedient
Shimshai – my son
Shimri – thorn; dregs
Shimrith – Shimron, same as Shimri
Shinab – father of changing
Shinar – watch of him that sleeps
Shiphi – multitude
Shiphrah – handsome; trumpet; that does good
Shisha – of marble; pleasant
Shishak – present of the bag; of the pot; of the thigh
Shitrai – gatherer of money
Shittim – thorns
Shiza – this gift
Shoa – kings; tyrants
Shobab – returned; turned back; a spark
Shobach – your bonds; your chains
Shobai – turning captivity
Shobal – path; ear of corn
Shobek – made void; forsaken
Shochoh – defense; a bough
Shoham – keeping back
Shomer – keeper; dregs
Shophach – pouring out
Shophan – rabbit; hid
Shoshannim – those that shall be changed
Shua – crying; saving
Shuah – ditch; swimming; humiliation
Shual – fox; path; first
Shubael – returning captivity; seat of God
Shuham – talking; thinking; humiliation; budding
Shulamite – peaceable; perfect; that recompenses
Shunem – their change; their sleep
Shuni – changed; sleeping
Shuphim – Shuppim, wearing them out; their shore
Shur – wall; ox; that beholds
Shushan – lily; rose; joy
Shuthelah – plant; verdure; moist; pot
Sia – moving; help
Sibbechai – bough; cottage; of springs
Sibmah – conversion; captivity
Sichem – portion; shoulder
Siddim – the tilled field
Sidon – hunting; fishing; venison
Sigionoth – according to variable songs or tunes
Sihon – rooting out; conclusion
Sihor – black; trouble (the river Nile)
Silas – three, or the third
Silla – exalting
Siloa – Siloam, Siloe, same as Shilhi
Silvanus – who loves the forest
Simeon – that hears or obeys; that is heard
Simon – that hears; that obeys
Sin – a bush – enmity
Sinai – a bush; enmity
Sinim – south country
Sion – noise; tumult
Sippai – threshold; silver cup
Sinon – a breast -plate; deliverance
Sisamai – house; blindness
Sisera – that sees a horse or a swallow
Sitnah – hatred
Sivan – a bush or thorn
Smyrna – myrrh
So – a measure for grain; vail
Socoh – tents; tabernacles
Sodi – my secret
Sodom – their secret; their cement
Solomon – peaceable; perfect; one who recompenses
Sopater – Sosipater – who defends the father
Sophereth – scribe – numbering
Sorek – vine; hissing; a color inclining to yellow
Sosthenes – savior; strong; powerful
Sotai – conclusion in pleading; binding
Spain – rare; precious
Stachys – spike or ear of corn
Stephanas – crown; crowned
Stephen – same as Stephanas
Suah – speaking; entreating; ditch
Succoth – tents; tabernacles
Succoth -benoth – the tents of daughters – or young women; or prostitutes
Sud – my secret
Sur – that withdraws or departs; rebellion
Susanna – lily; rose; joy
Susi – horse; swallow; moth
Sychar – end
Syene – a bush; enmity
Syntyche – that speaks or discourses
Syracuse – that draws violently
Taanach – who humbles thee; who answers thee
Taanachshilo – breaking down a figtree
Tabbath – good; goodness
Tabbaoth – rings
Tabeal – Tabeel, good God
Tabelel – God is good
Taberah – burning
Tabering – to beat with loud strokes
Tabitha – gazelle
Tabor – choice; purity; bruising
Tabrimon – good pomegranate; the navel; the middle
Tadmor – the palmtree; bitterness
Tahan – beseeching; merciful
Tahapenes – secret temptation
Tahath – fear; going down
Tahpenes – standard; flight; temptation
Tahrea – anger; wicked contention
Talithacumi – young woman, arise
Talmai – my furrow; that suspends the waters; heap of waters
Tamah – blotting or wiping out; smiting
Tamar – date palm; datetree erect
Tammuz – abstruse; concealed; consumed
Tanhumeth – consolation; repentance
Taphath – distillation; drop
Tappuah – apple; swelling
Tarah – a hair; a wretch; one banished
Taralah – searching out slander, or strength
Tarea – howling; doing evil
Tarpelites – ravishers; succession of miracles
Tarshish – contemplation; examination
Tarsus – winged; feathered
Tartak – chained; bound; shut up
Tartan – a general (official title)
Tatnai – that gives; the overseer of the gifts and tributes
Tebah – murder; butchery; guarding of the body; a cook
Tebaliah – baptism – or goodness – of the Lord
Tebeth – good – goodness (the tenth month of the Hebrews)
Tehinnah – entreaty; a favor
Tekel – weight
Tekoa – trumpet; that is confirmed
Telabib – a heap of new grain
Telah – moistening; greenness
Telassar – taking away; heaping up
Telem – their dew; their shadow
Telharsa – suspension of the plow
Telmelah – heap of salt
Tema – admiration; perfection; consummation
Teman – Temani – the south; Africa; perfect
Terah – to breathe; scent; blow
Teraphim – images; idols
Tertius – third
Tertullus – third
Tetrarch – governor of a fourth part
Thaddeus – that praises or confesses
Thahash – that makes haste; that keeps silence
Thamah – that blots out; that suppresses
Thamar – fruit – outcome
Tharah – same as Terah
Thebez – muddy; eggs; fine linen or silk
Thelasar – same as Telassar
Theophilus – friend of God
Thessalonica – victory against the Thessalians
Theudas – flowing with water
Thomas – a twin
Thuhash – badger
Thummim – perfection; truth
Thyatira – a perfume; sacrifice of labor
Tibbath – killing; a cook
Tiberias – good vision; the navel
Tiberius – the son of Tiber
Tibni – straw; hay
Tidal – that breaks the yoke; knowledge of elevation
TiglathPileser – that binds or takes away captivity
Tikvah – hope; a little line; congregation
Tilon – murmuring
Timeus – perfect; admirable; honorable
Timnah – forbidding
Timnath – image; figure; enumeration
Timnathheres – or Timnathserah – image of the sun; numbering of the rest
Timon – honorable; worthy
Timotheus – honor of God; valued of God
Tiphsah – passage; leap; step; the passover
Tire – headdress
Tirhakah – inquirer; examiner; dull observer
Tiria – searching out
Tirras – desire
Tirshatha – a governor
Tirza – Pleasantness[
Tirzah – benevolent; complaisant; pleasing
Tishbite – that makes captive
Titus – pleasing
Toah – weapon; dart
Tob – good; goodness
Tobadonijah – my good God; the goodness of the foundation of the Lord
Tobiah – Tobijah, the Lord is good
Toby – Tobias, the Lord is kind
Tochen – middle
Togarmah – which is all bone
Tohu – that lives; that declares
Toi – who wanders
Tola – worm; grub; scarlet
Tophet – a drum; betraying
Topheth – place of burning
Trachonitis – stony
Troas – penetrated
Trophimus – well educated; well brought up
Tryphena – delicious; delicate
Tryphon – masculine of Tryphena
Tryphosa – thrice shining
Tubal – the earth; the world; confusion
TubalCain – worldly possession; possessed of confusion
Tychicus – casual; by chance
Tyrannus – a prince; one that reigns
Tyrus – strength; rock; sharp
Ucal – power, prevalency
Uel – desiring God
Ulai – strength; fool; senseless
Ulam – the porch; the court; their strength; their folly
Ulla – elevation; leaf; young child
Ummah – darkened; covered; his people
Unni – poor; afflicted; that answers
Uphaz – pure gold; gold of Phasis or Pison
Upharsin – divided
Ur – fire, light, a valley
Urbane – courteous
Uri – my light, my fire
Uriah – or Urijah, the Lord is my light or fire
Uriel – God is my light or fire
Urim – lights; fires
Uthai – my iniquity
Uz – counsel; words
Uzai – he
Uzal – wandering
Uzzah – strength; goat
Uzzen-sherah – ear of the flesh
Uzzi – my strength; my kid
Uzziah – Uzzie, – the strength, or kid, of the Lord
Vajezatha – sprinkling the chamber
Vaniah – nourishment, or weapons, of the Lord
Vashni – the second; changed; a tooth
Vashti – that drinks; thread
Vophsi – fragrant; diminution
Yakman – meaning powerful, godly essence, almighty.
Yakob – Yacob, Yacoub, Jacob
Yehoyada – God knew.
Yashua or Eashoa – (also Esho – Eshu and Isho in Assyrian/Aamaic) the Aramaic name of Jesus
Yahweh – Yahwe, Yahawe, Yahave, Yehovah, Jehova or Jehovah, spellings of the pronunciation of YHVH
Yehezkel , variant of Ezekiel
Yoav – (יואב – Joab), “The Lord is father”
Yoel – Joel
Yohanan – Youkhana, Jonathan
Yonan – Younan, Aramaic/Chaldo,Assyrian names for John
Yosef – Yosep, Yosip, Yusuf, Joseph
Yuval – “fresh water stream” , “Water source that feeds into a stream”, Yuval is the first musician and artist mentioned in the Bible.
Zaanaim – removings
Zaanannim – movings; a person asleep
Zaavan – trembling terror
Zabad – dowry; endowed gift
Zabbai – flowing wanderer, pure pure
Zabbud – given, gift
Zabdi – same as Zabad Gift of Jehovah, my gift
Zabdiel – gift of God
Zaccai – pure meat; just – pure
Zacchaeus – pure; clean; just
Zaccur – of the male kind; mindful, mindful
Zachariah – memory of the Lord, remembered by Jehovah, remembered by the Lord
Zacharias – the Lord has remembered, Greek form of Zechariah
Zacher – memento; recollection; commemoration
Zadok – just; justified – just righteous
Zaham – crime; filthiness; impurity fatness
Zair – little; afflicted; in tribulation, little, small
Zalaph – shadow; ringing; shaking wound
Zalmon – his shade; his image shady
Zalmonah – the shade; the sound of the number; his image, shady
Zalmunna – shadow; image; idol forbidden
Zamzummims – projects of crimes; enormous crimes
Zanoah – forgetfulness; desertion marsh
Zaphnath-paaneah – one who discovers hidden things, revealer of a secret the man to whom secrets are revealed
Zarah – east; brightness
Zareathites – wasp (inhabitants)
Zared – strange descent
Zarephath – ambush of the mouth
Zaretan – tribulation; perplexity
Zareth-shahar – splendor of the dawn
Zarhites – rising of light (descendants)
Zartanah – pierce; puncture
Zarthan – pierce; puncture
Zatthu – olive tree
Zattu – [uncertain derivation]; olive tree
Zavan – disquiet
Zaza – belonging to all
Zebadiah – portion of the LORD; the LORD is my portion
Zebah – victim; sacrifice
Zebaim – gazelles
Zebedee – ”abundant; portion, my gift
Zebina – flowing now; selling; buying
Zeboiim – gazelles
Zeboim – deer; goats
Zebudah – endowed; endowing
Zebul – a habitation
Zebulonite – habitation (descendant)
Zebulun – dwelling; habitation
Zebulunites – habitation (descendant)
Zechariah – remembered by the LORD
Zedad – his side; his hunting
Zedekiah – the LORD is my justice; the justice of the LORD
Zeeb – wolf
Zelah – rib; side; halting
Zelek – the shadow or noise of him that licks or laps
Zelophehad – the shade or tingling of fear
Zelotes – zealous
Zelzah – noontide
Zemaraim – wool; pith
Zemarite – Canaanite
Zemira – song; vine; palm
Zenan – coldness; target; weapon
Zenas – living
Zephaniah – the LORD is my secret
Zephath – which beholds; that attends or that covers
Zephathah – watch-tower, associated with modern Zeita|Wadi Zeita
Zephi – observant
Zepho – that sees and observes; that expects or covers
Zephon – watch, tower
Zephonites – watch-tower (descendants)
Zer – perplexity
Zerah – same as Zarah
Zerahiah – the Lord rising; brightness of the LORD
Zered – be exuberant in growth; lined with shrubbery
Zereda – ambush; change of dominion
Zeredathah – pierce; puncture
Zererath – pierce; puncture
Zeresh – misery; strange; dispersed inheritance
Zereth – same as Zer
Zeri – crack; leak; distillation; balm
Zeror – root; that straightens or binds; that keeps tight
Zeruah – leprous; wasp; hornet
Zerubbabel – a stranger at Babylon; dispersion of confusion
Zeruiah – pain or tribulation of the LORD
Zetham – olive grove
Zethan – olive grove
Zethar – he that examines or beholds
Zia – sweat; swelling
Ziba – army; fight; strength
Zibeon – iniquity that dwells
Zibia – gazelle
Zibiah – the Lord dwells; deer; goat
Zichri – that remembers; that is a man
Ziddim – huntings; treasons; destructions
Zidkijah – justice of the Lord
Zidon – hunting; fishing; venison
Zidonians – catching fish; fishery (inhabitants)
Zif – this or that; brightness; comeliness
Ziha – brightness; whiteness; drought
Ziklag – measure pressed down
Zillah – shadow; the tingling of the ear
Zilpah – distillation from the mouth
Zilthai – my shadow; my talk
Zimmah – thought; wickedness
Zimran – song; singer; vine
Zimri – musical
Zin – buckler; coldness
Zina – shining; going back
Zion – monument; raised up; sepulcher
Zior – ship of him that watches
Ziph – this mouth or mouthful; falsehood
Ziphah – flowing
Ziphims – flowing (inhabitants)
Ziphion – watch,tower
Ziphites – flowing (inhabitants)
Ziphron – falsehood of a song; rejoicing
Zippor – bird; sparrow; crown; desert
Zipporah – beauty; trumpet; mourning
Zithri – to hide; demolished
Ziz – flower; branch; a lock of hair
Ziza – same as Zina
Zizah – prominence
Zoan – motion
Zoar – little; small
Zoba – station;
Zobah – an army; warring
Zobebah – canopier
Zohar – white; bright; dryness
Zoheleth – that creeps, slides, or draws
Zoheth – separation; amazing
Zophah – viol; honeycomb
Zophai – honey,comb
Zophar – rising early; crown
Zophim – place for a watchman
Zorah – leprosy; scab; hornet
Zorathites – wasp (inhabitants)
Zoreah – wasp
Zorites – wasp (inhabitants)
Zorobabel – same as Zerubbabel
Zuar – same as Zoar
Zuph – that beholds, observes, watches; roof; covering
Zur – stone; rock; that besieges
Zuriel – rock or strength of God
Zurishaddai – the Almighty is my rock and strength
Zuzims – the posts of a door; splendor; beauty"

bibnames = unlist(strsplit(bibnames, split = "\n")); head(bibnames,10)
tail(bibnames,10)
length(bibnames)
bibnames = gsub(" .*","", bibnames); head(bibnames,10); tail(bibnames,10)
bibnames = gsub("[[:punct:]]","",bibnames); head(bibnames,10); tail(bibnames,10)
bibnames = paste0(bibnames," ")

str_count(theOT,pattern = "Lord")

OTcounts = vector()
for (i in 1:length(bibnames)){
  OTcounts[i] = str_count(theOT, pattern = bibnames[i])
}
as.matrix(head(sort(OTcounts, decreasing = TRUE),20))

OTdf = data.frame(nombres = bibnames,
                  cuenta = OTcounts)

OTranked = OTdf[order(OTdf$cuenta, decreasing = TRUE),]

get.passages.names("On ",theOT)

OTranked[8,]
OTranked$orden = 1:nrow(OTranked)
OTranked = OTranked[-c(1,8,68,135),]
barplot(head(OTranked$cuenta,50), names.arg = head(OTranked$nombres,50),
        las = 2)








