### regex practice

### RegExplain:
# devtools::install_github("gadenbuie/regexplain")

fNephi1 = "1 I, Nephi, having been born of goodly parents, therefore I was taught somewhat in all the learning of my father; and having seen many afflictions in the course of my days, nevertheless, having been highly favored of the Lord in all my days; yea, having had a great knowledge of the goodness and the mysteries of God, therefore I make a record of my proceedings in my days.
2 Yea, I make a record in the language of my father, which consists of the learning of the Jews and the language of the Egyptians.
3 And I know that the record which I make is true; and I make it with mine own hand; and I make it according to my knowledge.
4 For it came to pass in the commencement of the first year of the reign of Zedekiah, king of Judah, (my father, Lehi, having dwelt at Jerusalem in all his days); and in that same year there came many prophets, prophesying unto the people that they must repent, or the great city Jerusalem must be destroyed.
5 Wherefore it came to pass that my father, Lehi, as he went forth prayed unto the Lord, yea, even with all his heart, in behalf of his people.
6 And it came to pass as he prayed unto the Lord, there came a pillar of fire and dwelt upon a rock before him; and he saw and heard much; and because of the things which he saw and heard he did quake and tremble exceedingly.
7 And it came to pass that he returned to his own house at Jerusalem; and he cast himself upon his bed, being overcome with the Spirit and the things which he had seen.
8 And being thus overcome with the Spirit, he was carried away in a vision, even that he saw the heavens open, and he thought he saw God sitting upon his throne, surrounded with numberless concourses of angels in the attitude of singing and praising their God.
9 And it came to pass that he saw One descending out of the midst of heaven, and he beheld that his luster was above that of the sun at noon-day.
10 And he also saw twelve others following him, and their brightness did exceed that of the stars in the firmament.
11 And they came down and went forth upon the face of the earth; and the first came and stood before my father, and gave unto him a book, and bade him that he should read.
12 And it came to pass that as he read, he was filled with the Spirit of the Lord.
13 And he read, saying: Wo, wo, unto Jerusalem, for I have seen thine abominations! Yea, and many things did my father read concerning Jerusalem—that it should be destroyed, and the inhabitants thereof; many should perish by the sword, and many should be carried away captive into Babylon.
14 And it came to pass that when my father had read and seen many great and marvelous things, he did exclaim many things unto the Lord; such as: Great and marvelous are thy works, O Lord God Almighty! Thy throne is high in the heavens, and thy power, and goodness, and mercy are over all the inhabitants of the earth; and, because thou art merciful, thou wilt not suffer those who come unto thee that they shall perish!
15 And after this manner was the language of my father in the praising of his God; for his soul did rejoice, and his whole heart was filled, because of the things which he had seen, yea, which the Lord had shown unto him.
16 And now I, Nephi, do not make a full account of the things which my father hath written, for he hath written many things which he saw in visions and in dreams; and he also hath written many things which he prophesied and spake unto his children, of which I shall not make a full account.
17 But I shall make an account of my proceedings in my days. Behold, I make an abridgment of the record of my father, upon plates which I have made with mine own hands; wherefore, after I have abridged the record of my father then will I make an account of mine own life.
18 Therefore, I would that ye should know, that after the Lord had shown so many marvelous things unto my father, Lehi, yea, concerning the destruction of Jerusalem, behold he went forth among the people, and began to prophesy and to declare unto them concerning the things which he had both seen and heard.
19 And it came to pass that the Jews did mock him because of the things which he testified of them; for he truly testified of their wickedness and their abominations; and he testified that the things which he saw and heard, and also the things which he read in the book, manifested plainly of the coming of a Messiah, and also the redemption of the world.
20 And when the Jews heard these things they were angry with him; yea, even as with the prophets of old, whom they had cast out, and stoned, and slain; and they also sought his life, that they might take it away. But behold, I, Nephi, will show unto you that the tender mercies of the Lord are over all those whom he hath chosen, because of their faith, to make them mighty even unto the power of deliverance."
sub13.20Wo = gsub("(?=Wo)","",fNephi1, perl = TRUE); sub13.20Wo
identical(sub13.20Wo,fNephi1)
v12 = "12 And it came to pass that as he read, he was filled with the Spirit of the Lord."
ftry = gsub("(?=S)[[:space:]]","",v12, perl = TRUE); ftry
abc = "abcdefghijklmnopqrstuvwxyz"
b3 = gsub("(?<=bc)[[:alpha:]]","'",abc, perl = TRUE); b3
B = gsub("b","",abc); B
identical(b,B)
bor = "bor"
or = gsub("(?=o)[[:alpha:]]{1,}","",bor, perl = TRUE); or
jlast = gsub("(?=k)[[:alpha:]]{1,}","",abc, perl = TRUE); jlast # INCLUDES THE K IN WHAT IS REMOVED
jagainlast = gsub("(?<=j)[[:alpha:]]{1,}","",abc, perl = TRUE); jagainlast # DOES SAME THING AS ABOVE, BUT DOESN'T INCLUDE THE LETTER USED (J)
jwrong = gsub("(?<!j)[[:alpha:]]{1}","",abc, perl = TRUE); jwrong
j2wrong = gsub("(?!j)[[:alpha:]]{1}","",abc, perl = TRUE); j2wrong

wtf = gsub("(?=k)[[:alpha:]]{2}","@",abc, perl = TRUE); wtf # includes the 'k' in what is replaced
wtf2 = gsub("(?<=k)[[:alpha:]]{2}","@",abc, perl = TRUE); wtf2 # starts replacing right after the 'k'

# https://stackoverflow.com/questions/12297859/find-and-replace-characters-before
yay = gsub(".*k[[:alpha:]]","",abc); yay
YAYYY = gsub("(?<=k).*","",abc, perl = TRUE); YAYYY
please = regmatches(abc,gregexpr("(?<=k).*",abc, perl = TRUE)); please
yay
please
# append(yay, "k",after = 0) # DOESN'T WORK THE WAY I WANT. DOES VECTORS
old = "abcefg"
gold = sub("(?<=.{3})", "d", old, perl = TRUE); gold
yay
yay2 = sub("(?<=.{0})","k",yay, perl = TRUE); yay2 # YAYYYYYY!!!!!!!!!!!
# Here's the website that taught me this sub() stuff: https://stackoverflow.com/questions/13863599/insert-a-character-at-a-specific-location-in-a-string
old
substr(old,2,4)



################################################################################


meses = c("Jun 17", "Jul 17","Aug 17","Sep 17","Oct 17","Nov 17","Dec 17","Jan 18",
          "Feb 18","Mar 18","Apr 18","May 18","Jun 18","Jul 18","Aug 18","Sep 18",
          "Oct 18","Nov 18","Dec 18")


################################################################################
################################################################################

library(regexplain)
dplyr::starwars$films












