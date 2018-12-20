### The flashcards notepad (apps and decks)

   ### The Flashcard functions ###

# variable lengths
# 3-sided
# 4-sided
# reverse 3
# reverse 4

### flashcards for many-sided cards (best for lists of variable lengths):
flashcards.many = function(yourlist){
  continue = readline(prompt = "Hit any button to continue, enter to quit: ")
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
        show = readline(prompt = "Hit 'a' to show next side: ")
        if (show == "a"){
          print(yourlist[[inum]][j])
        } else{print("you suck")}
      }
      inum.cum = c(inum.cum,inum)
      i = i+1
      continue = readline(prompt = "Hit any button to continue, enter to quit: ") 
    }
  }
  print("Peace out.")
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
# microbiology - SEE ALSO 'PATHOGENS' DECK IN ALPHABETICAL LIST BELOW
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


### ANYTHING FROM A ZOTERO ARTICLE WILL HAVE 'Zotero' AT BEGINNING OF CARD NAME (NAME SHOULD BE PRINCIPAL AUTHOR NAME AND YEAR, MAYBE ALSO TITLE KEY WORD):
## In alphabetical order by list (deck) name:
# ancient.names.3 (a list of ancient people(s) who were/are known by phonetically distinct names)
# BofM.EME.4 (Book of Mormon EME vocab passages)
# BofM.translation.facts.variable
# bythenumbers.3 (topic/question, number, source)
# climate.two.per.lecture (climate change Great Course lectures)
# definitions.variable
# geography.lists.variable
# medical.terminology.3 (term, definition, abbreviation)
###### name.ten.variable (name ten of any kind of thing - like name ten geniuses, name ten top-grossing companies, name ten overpaid CEOs, name ten life-saving vaccines, name ten of the biggest fossil fuel companies, etc.)
# pathogens.variable
# people.important.variable
# PharaohList.3 (a list of 30 famous or interesting pharaohs)
# physics.and.technology.variable (mostly of stuff I learn about in Science and Nature)
# political.facts.variable
# psalms.in.BofM.4
# quotes.variable (author and topic on front, quote on back; could have more than one side for long quotes or words to pay special attention to)
# random.facts.4 (a list of truly random facts--with topic name, two facts about it, and a source)
###### random.lists.variable (a list of random lists; the lists can be however long)
# science.corroboration.variable
# scriptures.variable (lists of scripture refences, with pertinent excerpts, on some topic)
# TreeLifeList.3 (68 of the dichotomous trees in the tree of life)

###### THESE TWO DECKS ARE VERY SIMILAR: NAME.TEN AND RANDOM.LISTS


##########################################################################################


                         ################################
                         ### Great Courses Flashcards ###
                         ################################

Shape.of.Nature = list(one = "filler",
                       topology.vs.geometry = c("Whereas geometry takes a quantitative approach to shape, topology takes a qualitative one. Equivalence of shapes in topology is also different than equivalence in geometry."),
                       isotopy = c("Equivalent shapes in topology."),
                       dimension = c("The amount of information you need to tell somebody in order to communicate a unique position on a given shape."),
                       circles.in.topology = c("To a topologist, a circle is simply a string with the ends connected to one another. You could twist that circle in elaborate ways and knots in 3D space, but yet it remains a 1D shape, because only one piece of info is needed to specify a location on the circle."),
                       knots = c("Simple case: The trefoil.","The trefoil is topologically different than the simple unknot. This can be proved mathematically! You don't have to leave it up to a gut instinct."),
                       Kurt_Reidemeister = c(""))
Modern.Political.Tradition = list(one = "filler",
                                  civil.republicanism.vs.civil.society.vs.communitarianism = c("27:29:58 - This list of thinkers can be confusing; let's separate out the themes. Thinkers concerned for community, civic republicanism, and civil society are related families of thinkers. If you want more precision: If the main glue supposed to bring us together is political activity, then that's civic republicanism. If the main concern is the proliferation of plural non-political association, it's civil society that's the focus. If the concern is the need for a moral sense of limits to individual self interest and an identification with the social or residential community itself, that's communitarianism. If there's a departure from neutrality, and a priority of the right without any concern for moral community, we probably just have a non-neutral liberalism. And if the thinker actually posits a theory of the good, or goods, we can call it perfectionist liberalism. The term communitarian has been given to all of these, confusingly. We must say communitarianism can be of the left or the right, if by that you refer to questions about distributive justice. Moving away from classical liberal positions--which neutralism tried to embody--toward the necessity of the community for the individual, can endorse government intervention in the economy (the traditional left view), or, traditional communal and national values that put restrictions on individuals (the right wing view). But we must acknowlege that any move away from neutralism in principle must increase the possibility of coercion. It also tends to move, as communitarianism tends to move, from Berlin's negative liberty to some positive notion of liberty. So greater community coercion of the individual is more possible for communitarians. Can or should the community tell me how to live? Well, in reality it does, all the time. Parts of it give mixed messages. Should some of those messages have public political force behind them? We already have this, but should we? Communitarians think we should have more, liberals less. The right-ward turn in British politics since 1980 is at least in part the rise of communitarianism. WTF does more political force behind community messages have to do with a 'right-ward' turn in politics???")
                                  )
Understanding.the.Brain = list(one = "filler",
                               Descartes.theory.on.the.soul.and.the.pineal.gland = c("Descartes thought that the pineal had to be the seat of the rational soul, because it was the only unpaired structure he could find. According to his theory, the soul was what mediated between mind and body--the two halves of the dualistic nature of reality. But Descartes didn't think animals had a rational soul--only humans. The problem with this? Other animals besides humans have a pineal gland. He never wrote anything down to show any awareness of this and account for it.","To me, this is a good example of a famous theory that implied interest in basing one's claims on empirical fact, but which failed to account for or fit the facts."),
                               Names.of.important.people.in.neuroscience = c("Thomas Willis","The Oxford Group: Willis, Locke, Hooke, Boyle.","Franz Joseph Gall","Simon LeVay - said 'The mind is just the brain doing its job.'","Hippocrates himself actually somehow knew that our mental faculties came from the brain. WTF???","")
                               )
Foundations.of.Western.Civilization.I = list(one = "filler",
                                             criteria.defining.civlization.extracting.food.from.arid.climates = c("1. irrigation","2. specialization of labor","3. political differentiation","4. arts and crafts"),
                                             if.you.want.to.understand.western.civilization.cover.these.themes = c("1. ecology, geography, climate","2. visible structures and invisible ideologies","3. pagan religious beliefs, Judaism, Christianity, Islam","4. how people lived (quotidian stuff like family structure, past-times, daily activities)","5. philosophical ideas","6. great works of literature","7. art and architecture","8. the difference between celebrity and distinction (how many celebrities today will be in the history books 500 or 1000 years from now (how many will influence the western tradition)?)","9. the distinction between values and virtues (???)","10. the good, the true, and the beautiful: aesthetics","11. the complimentary and competing roles of faith and reason","12. the competing claims of the individual and the community")
                                             )

# Public Health Problems
public.health = list(one = "filler",
                     neurodegenerative.disorders = c("Alzheimer's disease (AD) is the most common neurodegenerative disorder.","Parkinson's is the second most common one, currently affecting 4 million worldwide; WTF: that number is expected to double in the next few decades??? Wow.","Source: Chen-Plotkin et al., 'Finding useful biomarkers for Parkinson’s disease', in Science Translational Medicine, August 2018")
                     )

                         #########################
                         ### School Flashcards ###
                         #########################

Design.Analysis.Experiments = list(one = "filler",
                                   table.of.contents.DAE.book_20 = c("1 principles and techniques",
                                                                     "2 planning experiments",
                                                                     "3 designs with one source of variation",
                                                                     "4 inferences for contrasts and treatment means",
                                                                     "5 checking model assumptions",
                                                                     "6 experiments with two crossed treatment factors",
                                                                     "7 several crossed treatment factors",
                                                                     "8 polynomial regression",
                                                                     "9 analysis of covariance",
                                                                     "10 complete block designs",
                                                                     "11 incomplete block designs",
                                                                     "12 designs with two blocking factors",
                                                                     "13 confounded two-level factorial experiments",
                                                                     "14 confounding in general factorial experiments",
                                                                     "15 fractional factorial experiments",
                                                                     "16 response surface methodology",
                                                                     "17 random effects and variance components",
                                                                     "18 nested models",
                                                                     "19 split-plot designs",
                                                                     "20 computer experiments")
                                   )

Probability.Theory = list(one = "filler",
                          Chapters.1.and.2.summary = c(""))


## Epidemiology I
# by chapter? - No. BY POWER POINT.
# All questions I don't have answers to yet will in their c() have this: ???

#################################### EXAM I MATERIAL ####################################
Epi.Exam.I.subjects = list(one = "filler",
                           lecture_1 = c("Intro and History"),
                           lecture_2 = c("Morbidity"),
                           week.2_l3 = c("Mortality"),
                           week.2_l4 = c("Descriptive Epidemiology"),
                           week.3_l5 = c("Disease outbreaks"),
                           week.4_l6 = c("Surveillance"),
                           week.5_l7 = c("Standardization"),
                           lecture_8 = c("Ecological & Cross-sectional studies"))
Epi.Intro_History = list(one = "filler",
                         public.health.definition_2 = c("Public health is organized community efforts aimed at the prevention of disease and the promotion of health.","Public health is the science and art of protecting and improving the health of communities through education, promotion of healthy lifestyles, and research for disease and injury prevention."),
                         definition.and.6components.of.epidemiology = c("Definition: Epidemiology is the study of the distribution and determinants of health-related states and events in populations, and the application of this study to control health problems.","Components (6): ...","'study' - refers to MEASUREMENT of disease frequency, involving QUANTIFICATION of the occurrence of disease. Examples: surveillance, observation, hypothesis testing, descriptive, analytical and experimental research.","'distribution' - refers to FREQUENCY of disease by person, place, and time. We look for deviations from a uniform distribution (i.e. does the disease cluster?). Example: diabetes prevalence in the US concentrates in the south and midwest.","'determinants' - any FACTOR that brings about a change in a health condition. Types (6): PHYSICAL (safe water, clean air), BIOLOGY AND GENETICS, SOCIAL (culture, beliefs, support), redundantly: cultural, INDIVIDUAL [behavioral] (diet, exercise, smoking), and ECONOMIC (socioeconomic status, access to care).","'health-related states and events' (10) - infectious diseases, chronic diseases, deaths, injuries, disabilities, mental disorders, suicide, substance abuse, behaviors, use of health services.","'populations' - as opposed to individuals; epidemiology is population medicine.","'to control' - the aim is to PROMOTE, PROTECT, and RESTORE health."),
                         easy.key.terms.in.epidemiology_4_a5 = c("Exposure: refers to ...","1) determinants","2) agents","3) interventions","4) conditions","5) policies, or anything that might affect health.","Disease: refers to all health-related states or events","Morbidity: illness","Mortality: death"),
                         really.what.is.epidemiology_2.and.5 = c("Epidemiology is the study of health and disease in populations.","It's the basic science of public health.","WHO gets disease?","What CAUSES disease?","How does disease SPREAD?","What PREVENTS disease?","What works in CONTROLLING disease?"),
                         scope.of.epidemiology_4 = c("Describe (the health status of populations)","Explain (the etiology of disease)","Predict (the occurrence of disease)","Control (the distribution of disease)."),
                         why.consider.the.history.of.epi_2 = c("Scientific understanding of disease and causal factors is a product of historical successes and failures.","Changes in science, technology and disease patterns influence evolution of pubilic health and epidemiology, and so to address changing needs, the science of epidemiology continues to evolve, developing new methodologies to deal with complex problems."),
                         Hippocrates_5_i.e.Discernment.TermsUsed.Humors.Book.9Associations = c("Recognized that disease results from the physical environment rather than supernatural causes.","Used the terms 'epidemic' and 'endemic'.","Believed disease was the result of imbalance in 4 bodily humors: black bile, yellow bile, phlegm, and blood.","Wrote a work called 'On Airs, Waters and Places' in 400 BC.","He associated disease with geography, water conditions, climate, eating habits and housing, and the mode of living including glutony, indolence, or fondness for exercise and labor."),
                         James.Lind_16_i.e.Year.Approach.6Interventions.Recovery.4ApproachCriteria.NavyYear = c("Experiments with scurvy in 1753.","Divided sailors into groups of 2. Same diet in both.","Interventions (6): ...","1) cider","2) vinegar","3) sea water","4) lemon and 2 oranges","5) elixir","6) garlic, mustard seed and horseradish","The men with citrus in their diet were fit for duty in 2-6 days.","Lind's approach included four key criteria: ...","1) he made CLINICAL observations","2) he had an EXPERIMENTAL DESIGN","3) he observed POPULATION changes and their effect on disease","4) he considered SOURCES of disease.","Since 1895, the British navy has required limes/lime juice be included in diet of seamen."),
                         Edward.Jenner = c("In 1796, he did three things I already know about."),
                         John.Snow_4_i.e.Title.WhatUsed.MapYearIllustrates.Recap = c("The father of epidemiology.","He used 'shoe leather epidemiology' to determine that polluted water was the source of cholera.","Snow's map of cholera deaths in Soho in 1854 illustrates the study of DISTRIBUTION of disease.","Snow thought it spread through the water, and used epidemiologic methods to test his hypothesis."),
                         cholera_4 = c("There are 3-5 million cases of cholera a year globally, with ...","... 100,000 deaths globally.","Infection is often mild and may not display any symptoms.","5-10% [more like 5%, right? 5% of 3 million is 150,000] of those infected will develop severe with watery diarrhea, vomiting, and leg cramps with death occurring within hours without treatment (oral rehydration therapy)."),
                         nine.subspecialties.of.epi = c("clinical","reproductive","infectious","environmental","nutritional","pharmaco","social","molecular/genetic","pediatric or geriatric"),
                         ten.examples.of.current.public.health.issues = c("Autoimmune diseases","Chemicals","Climate change","Diabetes","Emerging infectious diseases like Zika and Ebola","Gun violence","Obesity","Tobacco","Unintentional injuries","Vaccines"),
                         do.the.causes.of.many.important.diseases.remain.unexplained = c("Yes."),
                         The.Two.Fundamental.Assumptions.of.Epi = c("Disease does not occur at random.","Disease has causal and preventive factors that can be identifed through systematic investigation."),
                         The.Essence.of.Epi = c("Comparison."),
                         epidemiologic.triangle_with.factors_2Host.4Agent.3Environ = c("For disease to occur there must be an interaction betweeen host, agent, and environment.","Host factors (2): ...","1) biological","2) social","Agent (4): ...","1) biological (bacteria, viruses)","2) chemical (alcohol, toxins)","3) physical (trauma, radiation)","4) nutritional","Environmental factors (3): ...","1) physical (climate, urban/rural, pollution, altitude)","2) social (political, social, economic)","3) herd immunity"),
                         epi.traingle.TB.example = c("Host: poor nutrition; concurrent disease; low immunity","Agent: Mycobacterium tuberculosis","Environment: crowding; poor ventilation; poor sanitation"),
                         three.major.contributors.to.understanding.health.and.disease = c("Basic sciences","Clinical sciences","Population medicine: epidemiology"),
                         clinical.vs.epi_3vs2 = c("Clinical examines the individual.","Clinical describes signs and symptoms in the individual.","Clinical prescribes individual treatment.","Epi does ...","populations; describes groups affected, with time and place trends and other variables affecting distribution","prescribes and evaluates community-wide interventions."),
                         complementarity.of.clinical.and.epi_2and1 = c("Epi depends on clinical for accurate diagnoses.","Clinical observations may suggest need for further epidemiological investigation.","Epidemiological knowledge helps guide selection of diagnostic procedures: Which tests are best? What's the most likely diagnosis given what is known about the patient AND the disease distribution?"),
                         basic.research.vs.epi_2vs2_and.also.dont.forget.thalidomide = c("Basic research illuminates biological basis of etiology and prevention (establishes biological plausibility).","Basic research provides precise control of covariates like genetics, environment and measurement(???).","Epi quantifies the magnitude of the association in humans.","Epi contributes to public health interventions even before biological mechanisms of disease are understood. - e.g. smoking and cancer; toxic shock syndrome"),
                         what.do.we.know.about.the.public.health.relevance.of.lab.findings_4 = c("Differences between vitro and vivo.","Differences in susceptibility across species.","Difficulties extrapolating across dosages, routes of administration, and lifespan.","Difficulties generalizing results from a controlled setting to free-living populations."),
                         research.strategies.ie.study.designs.in.epi_2_bb4 = c("Experimental: clinical and field trials","Observational: descriptive (person, place and time patterns) and analytic (hypothesis testing)","Analytic study designs: ...","1) ecological","2) cross-sectional","3) CASE-CONTROL","4) COHORT (PROSPECTIVE AND RETROSPECTIVE)"),
                         seven.bullet.points.of.epidemiological.reasoning = c("1) Suspect the possible influence of a factor on the occurrence of disease.","2) Modulate suspicion in response to observations.","3) Formulate hypothesis.","4) Test hypothesis using appropriate comparison groups.","5) Systematically collect and analyze data to determine whether association exists.","6) Assess validity of results - chance, bias, confounding.","7) Assess causality."),
                         applications.of.epi_1_7 = c("The application of epidemiology is to prevent disease/injury and promote health via a scientific approach.","1) by conducting surveillance of disease/injury occurrence in populations","2) by studying past (and future???) trends in health/illness","3) by identifying sections of the population at greatest risk","4) by evaluating the effectiveness of programs","5) by studying natural history of disease","6) by identifying CAUSES","6) by investigating outbreaks"),
                         the.epidemiologic.transition.of.the.last.century_1900 = c("1) pneumonia and inlfuenza - 200/100,000","2) TB - 194","3) diarrhea and enteritis - 142","4) heart disease - 137","5) stroke - 106","6) nephritis - 88","7) accidents - 72","8) cancer - 64","9) senility - 50","10) diphtheria - 40"),
                         transition.part.2_2010_COMPARE.TO.NCHS.2017.REPORT.SCREENSHOT = c("1) heart disease - 178","2) cancer - 172","3) chronic lower respiratory diseases - 42","4) stroke - 39","5) accidents - 37","6) Alzheimers - 25","7) diabetes - 20","8) nephritis - 15","9) pneumonia and influenza - 15","10) suicide - 12"),
                         natural.history.of.disease_def.and.use.and.subdivisions.of.phases = c("Definition: The course of disease from its beginning to its final clinical end points.","Uses (2): ...","1) Provides framework for prevention and control","2) identifies where in the disease's natural history effective intervention can occur.","Phases (6): ...","1) preclinical phase: onset","2) clinical phase (4, followed by outcome): ...","a) symptoms","b) seek care","c) diagnosis","d) treatment","then there's the outcome."),
                         summary.of.ppt_5 = c("What is epidemiology?","It's the study of disease distribution and determinants in populations.","What are the fundamental assumptions?","Disease doesn't occur by chance, and causes can be identifed and predicted through systematic study.","The essence of epidemiology is ...","... comparison.","What are the uses of epi? (4 - BUT REMEMBER HOW THERE WERE 7 BEFORE, IN applications.of.epi_1_7 ???)","1) identification of causes","2) monitoring frequency and distribution","3) measuring disease burden in populations","4) evaluating impact of intervention programs","What are the sub-specialties? - Just remember the 9 from the other flashcard."),
                         five.types.of.determinants = c("physical environment (safe water, clean air)","social environment (culture, beliefs, support)","economic (status, access to care)","individual characteristics (diet, exercise, smoking)","biology and genetics"),
                         COMPARE.4USES.TO.7APPLICATIONS.OF.EPI = c("4: Identifying causes, Monitoring frequency and distribution, Measuring disease burden, and Evaluating intervention programs.","7: Surveillance, study Trends, sections of pop at greater Risk, evaluate Programs, study Natural history of disease, id Causes, and investigate Outbreaks."),
                         COMPARE.6COMPONENTS.TO.4KEYTERMS.OF.EPI = c("6 Components: Study (6 examples, one of which is also one of the 7 applications of epi), Distributions, Determinants (6 types - which one is not included in the list of 5 determinants?), Health-related states, Populations, and Control.","4 Key Terms: Exposure (5, one of which is also one of the 6 components), Disease, Morbidity, and Mortality.")
                         )
Epi.Morbidity = list(one = "filler",
                     objectives = c("Define ratio, proportion, rate, and odds as measures of frequency.","Calculatea cumulative incidence, incidence rate and prevalence proportion and understand the difference between these measures.","Identify and calculate commonly used measures of morbidity."),
                     ratio.as.measure.of.freq = c("Division of one quantity by another.","It's the most general term; units and range are variable."),
                     proportion.as.measure.of.freq = c("A ratio in which the numerator is contained in the denominator.","No units.","Range: 0 to 1."),
                     rate.as.measure.of.freq = c("A ratio representing change over time."),
                     odds.as.measure.of.freq = c("Probability of an event DIVIDED BY THE PROABILITY OF A NON-EVENT: P/(1-P).","No units.","Range: 0 to infinity.","Example: 3:1 odds at a horse race. Example 2: If there are 100 smokers and 60 develop a cough, then ... odds of developing a cough are 60:40 or 1.5."),
                     four.things.you.need.to.calculate.a.measure.of.frequency = c("1) count of events","2) population size","3) period of observation","4) constant, K."),
                     cumulative.incidence.CI_4 = c("The cumulative incidence is the risk!","It's the proportion of initially susceptible individuals in a population who become new cases of a disease in a specified period.","The equation is ... (# of new cases of a disease during a given period of time / population AT RISK during that same time)*1000","You could also define CI (risk) as the probability that healthy people will develop disease during a specified time."),
                     incidence.rate.IR_aka = c("Also called incidence density.","It's the number of new cases divded by the person-time of observation."),
                     prevalence.proportion.PP = c("The proportion of the population who have the disease at a given point in time.","The equation's K = 1000. Numerator: # of existing cases of a disease at a given point in time. Denominator: total population."),
                     difference.between_CI.IR.PP = c("???"),
                     commonly.used.measures.of.morbidity = c("???"),
                     the.count_how.to.make.it.descriptive = c("# of cases of a health event","In order to be descriptive, a count must be considered relative to the size of the group."),
                     central.metrological.concern.in.epidemiology = c("Appropriate denominators. (Think in terms of counting and what makes a count descriptive.)"),
                     The.two.categories.of.measurement.for.describing.disease.freq = c("Prevalence.","Incidence."),
                     Prevalent.cases = c("Existing cases (old and new) at a point in time or over a period iof time."),
                     Incident.cases = c("New cases identified over a specified period of time."),
                     Two.kinds.of.prevalence.and.period.prevalence.denominator = c("Point prevalence.","Period prevalence.","The denominator of period prevalence, instead of being total population at a point in time, is the AVERAGE population during a PERIOD of time."),
                     criteria.needed.to.calculate.prevalence_4.plus.1.not.needed = c("1) knowledge of health status of study population.","2) specification of numerator (old and new cases).","3) size of total population.","4) POINT or PERIOD of observation.","We don't need to know time of onset in order to calculate prevalence."),
                     criteria.needed.to.calculate.CI_5.plus.7 = c("1) health status of population","2) time of onset","2) i.e. acute vs chronic (e.g. flu vs cancer)","3) specification of numerator","3) i.e. (A) new cases of disease","3) i.e. (B) excluding existing cases","4) specification of population at risk","4) i.e. (A) denominator should not include people with disease or who are not susceptible--a correction not usually made","4) i.e. (B) over time, the number of people at risk varies; how is this handled? - by using the population size at the mid-point of the time period","5) period of observation","5) i.e. (A) usually 1 year","i.e. (B) if frequency of disease is low, aditional years are included for stability"),
                     IMPORTANT.assumption.for.calculating.risk = c("E.g. - West Nile Virus calculation assumes EVERYONE is at risk of WNV."),
                     when.is.incidence.rate.used_what.does.it.estimate_what.is.the.unit_and.what.is.the.equation = c("It's used when individuals are observed for different lengths of time.","It estimates how quickly people are acquiring the disease.","The unit is 1/time. But isn't it different to say the denominator is person-time rather than just time??","IR or ID = # (of new cases during time period / total person-time at risk during study period)*1000"),
                     more.on.ID_what.it.allows.for.and.denominator.and.example_3 = c("ID allows for individual differentce in time at risk.","The denominator is person-time.","e.g. a rate of .2 cases per 12 person-months = .2 cases per person-year = 20 cases per 100 person-years."),
                     person.time_3 = c("An estimate of the actual time at risk in years, months, or days that all persons contributed to the study.","It accumulates over the duration of the study and for all participants.","A subject is eligible to contribute person-time to the study as long as they remain susceptible (at risk of developing the disease)."),
                     uses.of.prevalence_5 = c("1) NOT a measure of risk","2) used to express 'burden of disease'","3) used by health planners for determining workload","4) used to monitor control programs for chronic conditions","5) if incidence is not available, then prevalence can be used to estimate importance of disease."),
                     uses.of.incidence_3 = c("1) It tells us about change in status from non-diseased to diseased (limited to new cases)","2) etiology - it's a direct indicator of risk of disease","3) for comparing incidence rates in populations."),
                     limitations.of.prevalence.for.etiology_3 =c("1) It favors inclusion of chronic over acute","2) the screening effect","3) it's cross-sectional - the temporality problem."),
                     what.is.the.screening.effect = c("???"),
                     prevalence.depends.on_2 = c("Think P = IxD ...","It depends on incidence, and ...","duration of disease."),
                     P.equals.IxD = c("Incidence is the water dripping out of the faucet into the tub.","Prevalence is the water in the tub.","Recovery/death is the water draining out of the tub."),
                     QUIZ.LEARNING.EXPERIENCE_if.you.implement.a.CURE.to.a.given.disease.are.you.affecting.the.incidence.rate = c("The answer is no--you're only reducing the prevalence proportion. This is because, by CURING the disease, rather than PREVENTING it, you're still going to get the same frequency of people getting the disease, whom you will then cure. People GET the disease as often as before, but since people don't KEEP the disease for as long as previously (due to the cure) you'll find a lower prevalence when you measure it. So I'm guessing only a PREVENTIVE MEASURE could reduce (both) incidence (and prevalence)."),
                     CI.is.the.risk_Is.it.a.rate.or.a.proportion = c("Darwe said that CI is synonymous with incidence proportion; that would make it a proportion."),
                     risk.equals_not_ = c("Risk is CUMULATIVE incidence!","Apparently risk is NOT incidence RATE! Right? WRONG!!! Risk can be the incidence and incidence can be the risk.")
                     )
Epi.Mortality = list(one = "filler",
                     mortality.rate_3 = c("Incidence of death in a population.","Number of deaths occurring in a specified population in a given time period.","You canonly die once so the numerator can only be incident cases (as opposed to both new and old cases in the same individuals?)."),
                     when.is.mortality.rate.a.good.surrogate.for.an.incidence.rate.of.disease = c("When survival is low."),
                     crude.mortality_1 = c("Total death rate in an entire population (generally per 100,000 person-years)."),
                     cause.specific.mortality_2 = c("Rate at which deaths occur for a specific cause.","Number of deaths from specific cause / total population for a given year."),
                     age.specific.mortality = c("Number of deaths for age group / total population in age group for a given year."),
                     two.reasons.for.changes.in.mortality.trends.of.a.disease = c("Artifactual (4 and 3).","Real (4)."),
                     an.artifactual.reason_BOOK.VERSION_2_a4_b3 = c("An error in the numerator due to ...","1) ... errors in diagnosis","2) errors in age","3) changes in coding rules","4) changes in classification.","An error in the denominator due to ...","... 1) errors in counting population","2) errors in classifying by demographic characteristic (e.g. race, age, etc.)","3) differences in percentages of populations at risk."),
                     a.real.reason_4 = c("1) a change in survivorship without change in incidence","2) change in incidence","change in age composition of the populations","4) a combination of the above factors."),
                     changes.in.incidence.of.a.disease.can.occur.because.of.which.three.things = c("Such a change can result from genetic factors","environmental factors","or prevention (i.e. vaccination)."),
                     proportionate.mortality.ratio.PMR_eq.and.2Uses = c("# of deaths from given cause in a certain time preiod / total deaths in same time period ... per 100.","It's useful for identifying causes of death.","It gives the relative importance of a specific cause of death in relation to all deaths."),
                     the.magnitude.of.PMR.depends.on_e.g. = c("... deaths from other causes. - e.g. unintentional injuries among children vs among elderly"),
                     case.fatality.CFR_eq.and.2 = c("# of deaths due to disease X / # of cases of disease X ... *100","Refers to the proportion of fatal cases among those who have the disease.","Provides an index of the deadliness of a particular disease within a specific population."),
                     comparing.CFR.and.mortality.rates_2_a2 = c("e.g. rabies: deaths from rabies is rare in the US.","Cause-specific mortality rate would be LOW, because ...","... small numerator (# deaths due to rabies)","and there's a ***total population denominator***","Case fatality, on the other hand, would be HIGH. ...","... because death is almost certain, making the numerator almost the same as the denominator."),
                     infant.mortality_def.eq.1plus2Indicator = c("Defined as deaths under 1 year.","# of deaths among infants < 1 year in given time period / # live births during same period ... *1,000","Infant mortality is traditionally seen as an important indicator of community health. ...","... unmet health needs","unfavorable environmental factors like economic conditions, nutrition, education, sanitation, etc."),
                     maternal.mortality.rate_2Indicators = c("Considered an indicator of ...","adequacy of obstetric care","general level of socioeconomic development."),
                     maternal.mortality_denominator.and.equation = c("Denominator = only live births, though ideally it would be all pregnancies, but the records are more complete for live births than they are for fetal deaths.","# of deaths related to childbirth / # live births ... *100,000"),
                     NEED.TO.KNOW_why.is.crude.birth.rate.not.rate.nor.proportion.and.why.is.it.more.a.ratio = c("It's the number of births per average population."),
                     NEED.TO.KNOW_why.is.attack.rate.not.a.rate.but.instead.a.porportion = c("It is the number of new cases divided by those exposed. e.g. - household cases in outbreaks."),
                     NEED.TO.KNOW_why.is.fertility.rate.not.rate = c(""),
                     major.public.health.measures.whose.denominators.are.total.pop = c(""),
                     major.public.health.measures.whose.denominators.are.live.births = c("")
                     )
Epi.Descriptive = list(one = "filler",
                       descriptive.epi = c("The study of the distribution of disease in population groups."),
                       two.things.descriptive.epi.does = c("Summarizes data in a systematic fashion.","Determines non-random variation --> clues to etiology."),
                       five.objectives.of.descriptive.epi = c("1) evaluate trends in health and disease","2) provide for planning, provision, and evaluation","3) identify problems to be studied by analytic methods","4) need for changes in policy or research","5) directions, priorities for public health.","In summary: EVAL/TRENDS PROVIDE/P.P.E. ID/PROBS CHANGES/P.R. DIR/PRIORITIES"),
                       EASY.three.components.of.descriptive.epi = c("Person place time."),
                       person_elements.alphabetical.7.plus.other = c("Age","Family variables","Gender","Marital status","Occupation","Race/Ethnic","Social class","Other personal variables."),
                       person.age_3.things = c("Relationship to mortality and morbidity","J-shaped pattern for age-specific death rates","Related to chronic illness morbidity, less consistently with acute illness."),
                       person.gender_3.things = c("Morbidity higher for females","Mortality higher for males","Gender differences for specific diseases"),
                       male.to.female.ratio.of.age.adjusted.death.rates.for.leading.causes.of.death_10 = c("All causes: 1.8","Homicide and legal intervention: 3.88","Chronic obstructive pulmonary disease: 3.13","Suicide: 3.05","Accidents and adverse effects: 2.96","Chronic liver disease and cirrhosis: 2.19","Heart disease: 2.01","Pneumonia and influenza: 1.86","Congenital anomalies: 1.15","Diabetes mellitus: 1.04"),
                       person.race_2.obvious.things.and.a.Q = c("Tradition","Difference in morbidity and mortality by race/ethnicity","Q: What do differences mean?"),
                       person.social.class_2.things_also.intermediate.variables.and.health.outcomes.of.wealth.education.occupation = c("1) Summarizing variable, linking occupation, education, residence, income, lifestyle","2) Associated with morbidity and mortality","Wealth - Intermediate variables ...","Intermediate variables: Access to health care; access to dietary choices.","Health outcomes: Detection and treatment of illnesses; obesity, blood pressure, cholesterol levels.","Education - Intermediate variable ...","Knowledge, attitude and behavior about diet, smoking, alcohol, exercise, sexual practices, illegal drug use, family planning, prenatal care.","Health outcomes: Heart disease, lung cancer, AIDS, low birth weight.","Occupation - Intermediate variable ...","Exposure to hazards, psychological stresses, physical activity","Health outcomes: Cancer, heart disease, accidents, miscarriages, birth defects, other."),
                       person.occupation_3.things = c("Physical conditions, chemicals, noise, stress","Self-selection bias","Associated with specific diseases"),
                       person.marital.status_1.thing.and.an.explanation = c("Lower mortality among married","Explanation: selection factors, psychological and physical support, sexual exposure or hormonal factors for women, misclassification(???)."),
                       person.family_4.things = c("Family size --> social class","Birth order","Maternal age","Parental deprivation"),
                       person.other.variables_4.things = c("Blood type","Personality traits","Specific immunity","Exposures or behaviors"),
                       place_elements.5 = c("1) Natural boundaries","2) Political subdivisions","3) Urban-rural","4) International","Migrant studies"),
                       place.natural.boundaries_3.things = c("Natural barriers, physical environment","More useful than political boundaries in etiology","Climate, temperature, humidity, rainfall, altitude, and water supply."),
                       place.political.subdivisions_3.things = c("Practical and convenient","Reflect administrative boundaries","Denominator for rates"),
                       place.urban.rural_3.things = c("Differences in morbidity and mortality related to specific factors","Urban factors: pollution, crowding, stress, violence, etc.","Rural factors: specific exposures, accidents."),
                       place.international.comparisons_3.things = c("Relative progress in disease control","Clues to etiology","Differences may be artifactual."),
                       place.migrant.studies_3.things = c("Separate genetic from environmental factors","Compares genetically similar populations under different environmental conditions","Selective factors associated with migration."),
                       time_elements.4 = c("Secular trends","Cyclic or seasonal changes","Short-term fluctuation","Time/place clusters"),
                       time.secular.trends_3.things = c("Long-term trends, years or decades","All diseases, especially cancer","May reflect changes in incidence, survival, or artifacts"),
                       time.cyclic.or.seasonal.trends_2.obvious.things = c("Annual, monthly, weekly","Most helpful in explaining infectious diseases"),
                       time.time.and.place.clusters_2.things = c("Clusters --> etiological agent was introduced into environment at a certain time","Denominator, calculating expected values, complex statistical issues"),
                       why.does.epidemiology.deal.with.populations_1.and.2ab = c("Because population-level data is better for generalization of findings beyond group studied","Because population data is needed for statistical inference: ...","a) Estimating incidence, prevalence","b) Low frequency diseases - need large numbers of at risk individuals to get reliable estimates."),
                       does.epidemiologic.or.public.health.research.need.large.data.sets = c("Yes!")
                       )
Epi.Infectious.Outbreaks = list(one = "filler",
                                objectives = c("Describe the fundamental principles of infectious diseases, including terminology.","Describe the process of investigating outbreaks."),
                                features.specific.to.infectious.diseases_6 = c("Infected people become exposures to others","The host can be immune!! [Carriers are not infected, right? WAIT, WHAT ABOUT HERD IMMUNITY???? IF IMMUNE PEOPLE COULD CARRY IT, THEN WHY WOULD HERD IMMUNITY WORK???]","Infectiousness is typically temporary","Infected people may be asymptomatic for various periods of times (including always)","Diseased people (symptomatic) may or may not be infectious to others","Time frame may be urgent."),
                                are.toxins.infectious = c("No; they're the product of an infectious agent."),
                                which.outbreak.was.identified.via.routine.PFGE.typing = c("The Salmonella Newport outbreak of July 2003"),
                                how.was.the.Newport.Salmonella.outbreak.of.2003.identified = c("Via routine PFGE typing"),
                                host.specificity_3_a3_b3_c2a2b3 = c("Some diseases only affect humans, like smallpox, hep B and polio","Some only affect specific animals, like dourine, bovine viral diarrhea, and feline immunodeficiency virus","Some diseases affect multiple species, like foot and mouth disease, and paratuberculosis (don't affect humans); and, zoonotic diseases (DO affect humans) like West Nile, anthrax, and rift valley fever."),
                                what.is.infection.and.carrier.and.contamination_3_a2 = c("Infected means a foreign agent multiplies within the host and the body reacts to its presence. This reaction could be ...","... could be asymptomatic, symptomatic, or a latent infection.","'Colonized/Carrier' refers to the persistence and multiplication of agent on mucosal surface without apparent host reaction.","Contamination = the presence of the agent on surface of body or inanimate object that may serve as a source of infection."),
                                infection.and.disease.timelines_be.able.to.draw.diagram = c("Remember that infectious period and incubation period can overlap!"),
                                minimum.incubation.period = c("time from last exposure to first case"),
                                maximum.incubation.period = c("time from first exposure to last case"),
                                mean.incubation.period = c("time from midpoint of exposure to peak number of cases"),
                                two.measures.of.outbreak.disease_plus.one.big.fact.about.them = c("attack rate","case fatality rate","NONE OF THESE 'RATES' HAVE PERSON-TIME IN THEM"),
                                attack.rate_definition.and.primary.and.secondary = c("# of cases / total # of exposed and at risk persons","Primary: # of cases / # at risk who were exposed to initial infectious source","Secondary: # of cases / # at risk and exposed persons to primary cases [meaning persons exposed to primary cases, right?]"),
                                secondary.attack.rate_what.is.it.a.measure.of = c("The secondary attack rate is a measure of person-to-person transmission."),
                                attack.rate_equation = c("# of cases / population at risk"),
                                risk.ratio_equation = c("risk (attack rate) in exposed / risk (attack rate) in unexposed"),
                                case.fatality.rate.definition = c("# of deaths / # of infected"),
                                vaccine.effectiveness.varies.by.disease_two.examples = c("hep A - greater than 98%","influenza - ranges from 30 to 70% effective"),
                                diseases.obviously.vary.by.transmissibility = c("DUH! Measles is among most transmissible."),
                                herd.immunity = c("Enough people in a population are immune such that it does not spread to susceptible people."),
                                endemic.vs.epidemic_be.able.to.draw.graph = c("Endemics are little bumps and an epidemic is a huge bump!"),
                                what.are.the.four.names.for.violent.upticks.in.disease.incidence.of.different.scales = c("cluster","outbreak","epidemic","pandemic"),
                                two.study.designs.for.outbreak.investigations = c("Retrospective cohort","Case-control"),
                                cohort.study.does.what = c("Exposed and non-exposed individuals followed to determine development of disease."),
                                retrospective.cohort.study.does.what_4 = c("Looks at historical exposure data","It's the technique of choice with an ACUTE outbreak IN A WELL-DEFINED POPULATION","It's a direct assessment of increased risk associated with exposure","The measure of association often used is Risk Ratio (RR)."),
                                case.control.study.does.what_4 = c("People with disease (cases) are compared to people without (controls).","The cases and controls should be comparable (duh).","A case-control study is appropriate with the population at risk is not known.","The measure of association used in a case-control is Odds Ratio (OR)."),
                                DIFFERENCE.BETWEEN.CASE.CONTROL.AND.COHORT = c("Cohort: Exposed and unexposed are compared. RR. Well-defined population.","Case-conrol: Diseased and non-diseased are compared. OR. Population at risk is not known.","However, then later on (in step five - select a study design [to determine who is at risk of becoming ill]) it says that in a COHORT study you compare exposures between ill and non-ill, and in a CASE-CONTROL you compare exposures between cases and controls, which means ill and non-ill. Haha!"),
                                what.is.an.outbreak = c("An epidemic limited to localized increase in the incidence of a disease or health-related event, as in a town, village, or closed institution"),
                                what.is.considered.an.outbreak_2 = c("Two or more cases FROM DIFFERENT HOUSEHOLDS/FAMILIES of potentially infectious disease of unknown etiology.","Cases could have similar clinical syndrome with pending or inconclusive or no lab testing, as in a cluster of rash illnesses, meningitis, or a respiratory illness."),
                                different.shapes.colors.sizes.of.outbreaks_8 = c("foodborne","respiratory tract infections","nosocomial","STI","new diseases e.g. SARS","predictable e.g. seasonal influenza","non-infectious e.g. obesity","many more"),
                                challenges.of.communicable.disease.outbreak.investigations_6_d2 = c("Irregular and dynamic data sources vary dramatically in completeness and accuracy (think hospital records or school health records).","Small sample sizes, because outbreaks can affect small numbers of people, causing restrictions on design, analysis, and conlcusions.","Hampered collection of environmental or biological specimens, because you're arriving after the fact.","Bias created by media publicity, which affects opinions about the source, but it also can help you find cases and implement control measures.","Reluctance of parties (cases and sources) to participate voluntarily.","Conflicting pressures on timing of intervention; public outcry can interfere with the science."),
                                steps.of.an.outbreak.investigation_10 = c("1) establish existence of outbreak","2) confirm dx","3) define a case and count cases","4) orient data to time, place, and person","5) determine who is at risk of becoming ill","6) develop hypotheses and test them","7) compare hypotheses with established facts","8) plan a more systematic study","9) communicate findings and prepare a written report","10) implement control and prevention"),
                                four.tips.on.steps.of.an.outbreak.investigation = c("They may not go in order","several steps may be occurring at once","control/prevention may be implemented soon after beginning","do NOT get over-confident in hypothesis"),
                                step.one.establish.existence.of.outbreak_3_b4_and.key.questions_3_a5_b3 = c("Since most local health departments have ongoing records of communicable diseases you can easily determine if the observed numbers exceed the expected level.","Analyze surveillance data carefully for ...","1) changes in local reporting practices","2) increased interest in certain diseases","3) changing diagnostics","4) so you can determine if the report is valid.","Collect specific information from your source: You may find more possible cases, so GET AS MUCH INFO AS POSSIBLE ON THAT FIRST CALL!","Key questions: Who, When, and What ...","Who: Names of all symptomatic people","ages","genders","race","contact info","When: date of onset","time of onset","still symptomatic?","What: What are their symptoms?"),
                                step.two.confirm.the.diagnosis_3_c2 = c("Determine if clinical specimens have been obtained.","If not, recommend that specimens be collected on acutely ill persons.","The importance of specimen collection: ...","For formulating hypotheses","and for implementing control measures."),
                                step.three.define.a.case.and.count.cases_3_a3_and.levels.of.a.case.definition_3_and.four.more.points_c4_and.ID.common.source_2 = c("Create a case definition comprising: ...","Lab testing","Symptoms","Time","Case definition must be workable, meaning that some cases will be missed, and other non-cases will be unintentionally included.","Case definition must be applied equally and without bias to all persons under investigation.","Three levels of a case definition: ...","Confirmed: Using lab testing","Probable: Using clinical and epidemiological data, and maybe unreliable lab methods","Clinical: Using a clinical dx duh","Four more points on defining a case and counting cases: ...","1) Control and prevention measures depend on knowing source, mode of transmission, and characteristics of ill people.","2) Case finding includes collecting information.","3) Quickly develop a line list or questionnaire (4): ...","a) demographics","b) symptoms","c) lab","d) exposure history","4) Prepare yourself (???); a second interview for person being interviewed may be necessary","ID common source (2): ...","Institution and attack rate info; onset dates and symptoms; specimens collected, tests ordered, and results.","Key question: Observed vs Expected (# of illnesses observed in previous time periods, like same time last year, compared to current event)."),
                                key.questions.step.one.vs.step.three.ID.common.source.key.question = c("In step one, the key questions are Who, When, and What, with 5 sub-questions for Who and 3 for When.","In step three - ID common source, the key question is Observed vs Expected, meaning compare the # of illnesses in this event to the same time last year or other previous time periods."),
                                what.do.control.and.prevention.measures.depend.on_3 = c("1) knowing source","2) knowing mode of transmission","3) and knowing the characteristics of the people who are ill."),
                                step.four.orient.data.to.4Time.4Place.3Person = c("Characterize the collected data - Time: ...","1) Epidemic curve: Point source vs Person-to-Person, Incubation Period, Period of Exposure.","2) This curve is very important if the agent is unknown.","3) The curve can possibly predict how many more cases are likely to occur.","4) The curve is an excellent tool for communication.","Place (4): ...","1) The disease may be associated with a location.","2) So, plot the data on a map dummy","3) A distribution pattern may appear that approximates known sources and routes of potential exposure, mode of exposure, and vehicle of exposure","GIS is an emerging tool (WAIT, YOU MEAN THEY HAVEN'T BEEN USING IT FOR DECADES BY NOW!?!?)","Person - examine the characteristics of cases thusly (3): ...","1) by age, gender, race, occupation, or other","2) if a special attribute emerges, it may provide evidence of who is at risk and what a possible exposure is","3) dates: symptom onset, dx, resolution of symptoms"),
                                step.five.determine.who.is.at.risk.of.becoming.ill_4 = c("1) Compare putative exposures to the illness: This investigation begins with a case definition (YOU MEAN STEP 3?), and epidemiological methods (YOU MEAN STEP SIX?)","2) Case definition: Establish boundaries on who is considered a case using lab results, symptoms, onset, and location (WHY ARE WE REPEATING STEP THREE?? SO WE CAN HAVE THE NICE ROUND NUMBER OF TEN STEPS??).","3) After collecting data, defining a case, and orienting data to time, place, and person it should become evident who is at risk: a) could be community-wide b) or limited to restaurant attendees c) or an occupational exposure.","4) If the outbreak covers a large area it can be difficult to identify who is at risk."),
                                step.five.selecting.study.design_2 = c("Retrospective cohort study: Is ideal for a well-defined population (cohort). For example, GI among wedding reception attendants. In this type of study, each person is surveyed to determine who's ill and compare exposures (food, beverage) between ill and non-ill persons.","Case-control study: Compare exposures between cases and contols; NOTICE THE SIMILARITY BETWEEN THE TWO (R. COHORT AND CASE-CONTROL): R. COHORT DEFINITION = COMPARE EXPOSURES (FOOD, BEVERAGES) BETWEEN ILL AND NON-ILL; WHEREAS C.C. DEFINITION = COMPARE EXPOSURES BETWEEN CASES AND CONTROLS (BETWEEN ILL AND NON-ILL). - SAME DEFINITIONS! YOU HAVE TO GO DEEPER THAN THESE POWER POINTS TAKE YOU TO REALLY UNDERSTAND THE DIFFERENCE. The slide helps a little by saying that 'Controls are similar to cases in potential for exposures, except the controls are not ill; and you get the controls by random phone-dialing, enlisting neighbors, friends, or acquaintances."),
                                step.six.develop.hypothesis.about.exposure.and.test.hypothesis.with.appropriate.statistical.methods_2_a3 = c("Data analysis: ...","a) evaluate risk of illness (measure association between illness and exposures)","b) test hypothesis by analyzing the data, moron","c) the analysis depends on the study design used, DUH!","Besides data analysis, you must also assess clinical, lab, and epidemiological features of the disease."),
                                step.seven.compare.hypothesis.with.established.scientific.knowledge_2 = c("Hypothesis should be consistent with clinical, lab, and epidemiological facts of the investigation.","Q: Do the proposed exposures, mode of spread, and population affected fit well with known facts (the observations?) of the disease?"),
                                step.seven.causation.and.outbreak.investigation_2_a5 = c("Apply the criteria for causation!: ...","a) temporality","b) strength of association (RR or OR)","c) biological plausibility","d) consistency/coherence","e) dose response","The challenge during an outbreak investigation is to balance need to assess causality with the need for public health intervention"),
                                step.eight.plan.a.more.systematic.study_2_b3 = c("The initial investigation is complete! Yay! However, can more be learned from the outbreak?","Improve the sensitivity and specificity of the case definition by getting: ...","a) more accurate # of persons at risk","b) better (defined) case count","c) additional lab tests (serology)"),
                                step.nine.communicate.findings_5_e9 = c("1) Document for action to get detailed recommendations for prevention and control.","2) Record of performance provides input and output measures for program performance evaluations.","3) Document for potential medical/legal issues.","4) Enhance the quality of the investigation, identifying weaknesses and stimulating further inquiry and fact finding.","5) This investigation can now become an instrument for teaching! It can be utilized this way if ...","a) Persons involved in the investigation should complete a report giving a summary of their role.","b) You include ... date of outbreak","c) agent","d) who reported the outbreak","e) each person involved","f) # ill","g) source/vehicle","h) contributing factors","i) control measures."),
                                step.ten.execute.control.and.prevention.measures_2_a4 = c("Recommendations for precautionary measures to prevent further disease are ...","a) made by the investigation team","b) Based on ... etiological agent","c) source and or suspected vehicle","d) population at risk","Difficult decisions are made often with potentially large economic ramifications."),
                                BONUS.QUESTIONS_what.are.the.five.criteria.for.causality = c("1) temporality","2) strength of association (RR or OR)","3) biological plausibility","4) consistency/coherence","5) dose response."),
                                BONUS.QUESTIONS_what.is.the.big.challenge.during.an.outbreak.investigation = c("Balancing the need to assess causality with the need for public health intervention."),
                                BONUS.QUESTIONS_what.is.included.in.the.line.list.or.questionnaire_4 = c("demographics","symptoms","lab","exposure history"),
                                BONUS.QUESTIONS_is.there.any.benefit.to.the.bias.created.by.media.publicity.in.an.outbreak = c("Yes! There is! One benefit is that it can help you find cases and implement control measures."),
                                how.do.we.know.if.someone.has.an.infection = c("By whether or not they have produced antibodies in response to something."),
                                why.is.bubonic.plague.not.a.problem.anymore = c("Because the bubonic part doesn't lead to the pneumonic part anymore. The pneumonic transmission is what leads to rapid spread within a population, and the septicemic follows it, and causes death. But people can get greated with antibiotics as soon as they start to notice they have a swelling in their regional lymph nodes, thus preventing the bubo from ever rupturing and bacteria spreading to the lungs in the first place."),
                                how.many.cases.of.Ebola.would.it.take.to.call.it.an.epidemic = c("In Oklahoma, it would take only 1 case to call it an epidemic. That's because we're not expecting ANY cases of Ebola. Wow.","Call it a cluster, outbreak, or an epidemic. They're all the same. They're not based on numbers. It just depends on how much you want to get the public's attention."),
                                what.is.the.Merriam.Webster.definition.of.a.pandemic.and.why.can.the.Ebola.outbreak.be.called.a.pandemic = c(""),
                                what.is.the.key.term.or.phrase.with.determining.that.we.need.to.do.a.cohort.study.in.an.outbreak = c("Defined population. That's the key term. When we have a defined population, we do a cohort study. But the problem with this definition is that we still need to determine what kinds of criteria can define a population! Duh! What defines the population in the birthday party example? The fact that they were all at the birthday party."),
                                why.9.cases.instead.of.3.cases.on.the.quiz.question.about.what.is.most.suggestive.of.an.outbreak.sharing.a.common.exposure = c("THIS QUESTION IS AWESOME! I GOT IT WRONG BECAUSE I DIDN'T NOTICE THAT IF IT'S ALL WITHIN THE SAME FAMILY, THEN IT'S NOT A FREAKING OUTBREAK DUH! I CAN'T BELIEVE HOW EASY THAT ONE ENDED UP BEING.")
                                )
Epi.Surveillance = list(one = "filler",
                        surveillance.lecture.objective = c("Be able to identify and describe public health surveillance systems in the US."),
                        surveillance.definition_1abcd = c("The continuing scrutiny of all aspects of occurrence and spread of a disease that are pertinent to effective control. Included are the systematic collection and evaluation of: ...","a) morbidity and mortality reports","b) reports of individual cases and field investigations of epidemics","c) isolation and identification of infectious agents by labs","d) data on exposures and risk factors"),
                        what.the.heck.is.public.health.surveillance = c("It is ONGOING, SYSTEMATIC collection, analysis, interpreation, and DISSEMINATION of data regarding a health-related event for use in public health action to reduce morbidity and mortality and to improve health."),
                        measurable.attributes.of.surveillance.systems_9 = c("acceptability","data quality","flexibility","positive predictive value","representativeness","sensitivity","simplicity","stability","timeliness"),
                        surveillance.objectives_6 = c("1) Describe the distribution of health problems - identifying inequalities and disparities","2) Monitor disease trends","3) Evaluate the effectiveness of interventions","4) Triger investigation of etiology and control of disease transmission","5) Educate","6) Establish national priorities"),
                        four.types.of.surveillance.systems = c("Active: Reports are solicited from reporting sources at established intervals OR FOR EMERGENT NEED (FAILED TO PUT THAT LAST PART IN THE FIRST SLIDE).","Passive: Reporting sources give reports of disease at will.","Sentinel: Key report sources are selected to participate in an enhanced disease surveillance system.","Syndromic: Electronic transfer of data fields describing symptomatology or presenting complaints (e.g. ED visit, acute care log, lab test orders)."),
                        active.surveillance.examples_2 = c("Call labs every Monday for disease reports.","Calls initiated as part of an epidemiologic investigation to rapidly detect additional cases."),
                        active.surveillance.strengths_4 = c("rapid","increased sensitivity and specificity","may increase collection of appropriate specimens","increases specimen submission to Public Health Lab for specialized typing or ID"),
                        active.surveillance.limitations_2 = c("resource intensive","difficult to maintain for extended periods of time"),
                        who.does.the.health.department.reach.out.to.in.active.surveillance_4 = c("labs","hospitals","doctors","health care workers"),
                        passive.surveillance.examples_3.dumb.ones = c("Reports faxed/mailed from health care providers.","Reports faxed/mailed from labs.","Phone/email notification."),
                        passive.surveillance.strengths_2 = c("simple","not overly burdensome on the public health system"),
                        passive.surveillance.limitations_2 = c("completeness of reporting","underreporting is likely"),
                        active.vs.passive.surveillance_5 = c("costly vs less so","increased human resource requirements vs less so","data quantity and quality are improved vs less assurance about data q&q","notification more timely vs reports made late if ever","reduced sustainability vs increased sustainability"),
                        augmenting.passive.surveillance.systems_3 = c("Stimulate passive systems through interaction ...","periodic lab audits","periodic review of medical record data - ICD-9 / ICD-10 codes for reportable diseases","death certificate review"),
                        sentinel.surveillance.overview_2_b3 = c("HC providers and testing labs","Based on geography, population, lab testing capacity. Now remember, that's: ...","geography","population","and lab testing capacity."),
                        sentinel.surveillance.goals_3 = c("Identify entry of disease of interest in geographic area or population.","Collect more information about disease-causing agents.","Track geographic progression of disease."),
                        sentinel.surveillance.influenza.goals_4 = c("1) Identify entry of influenza into state/region","2) Identify type of influenza virus - novel or known serotype?","Identify the geographic progression of the virus - where was, is, and might go, it?","4) Identify any change in the type of virus circulating"),
                        sentinel.surveillance.example_3_a2 = c("Emerging drug resistant pathogens like ...","a) Strep pneumoniae","b) MRSA/VRSA","Medical Examiner can be a useful sentinel","Sometimes includes monitoring of non-human hosts like West Nile Virus--a sentinel s. system using birds."),
                        sentinel.surveillance.strengths.and.limitations_3and3 = c("Strengths: ...","valid and reliable","rapid","good for temporal tendencies","Limitations: ...","who does it represent?","labor intensive","incomplete prevalence estimates"),
                        syndrome.indications_3 = c("The presence of a syndrome can indicate exposure to a chemical or biological agent like ...","Bioterror","Industrial pollution","Naturally occurring agents like coliform bacteria."),
                        syndrome.definition = c("A syndrome is a group of symptoms that collectively indicate/characterize a disease, psychological disorder, or other abnormality."),
                        which.surveillance.system.type.is.used.to.monitor.disease.trends = c("Syndromic! (??? - are none of the others used for that?)"),
                        syndromic.surveillance.1Objective.and.1ApplicationNow = c("Objective: Target early manifestations (prodromes) of bioterrorism-related disease processes, with the goal of enabling earlier detection of epidemics and more timely public health responses.","Application now: Awareness of disease situation (???)"),
                        syndromic.surveillance.methods_3 = c("(repeat of description:) ED visit, acute care log, or lab test orders","Periodic analysis to detect temporal or spatial clusters.","If a predetermined threshold is exceeded, an active surveillance and medical record review is triggered."),
                        syndromic.surveillance.strengths.and.limitations_2and3 = c("Strenghts: ...","It may provide early recognition of an ID outbreak or BT event.","It ensures ongoing, systematic surveillance if set up properly.","Limitations: ...","Numerous software packages and many EMR systems","requires compatible electronic formats","may be difficult to define sensitivity to individual or combined measures being used."),
                        national.disease.reporting.NNDSS_3 = c(""),
                        national.disease.reporting.facts_4 = c(""),
                        who.should.be.a.PHIDDO.user = c("PHIDDO = public health investigation and disease detection of Oklahoma system"),
                        OK.reportable.conditions_infectious_tons = c(""),
                        OK.reportable.conditions_noninfectious_13_a7_b4_c_d = c("")
                        )
Epi.Standardization = list(one = "filler",
                           standardization.lecture.objectives = c("Describe the limitations of crude rates and identify alternative measures.","Calculate and interpret specific rates.","Calculate and interpret adjusted rates using direct and indirect standardization."),
                           the.categories.of.measures.of.rates_how.many.are.there.and.what.are.they = c("There are three.","Crude: Summary measures for the total population.","Specific: Measures for population subgroups, restricting the numerator and denominator to specific subgroups (e.g. age, sex, race, etc.)","Adjusted: Summary measures for total population statistically transformed to remove the effect of differences in population composition (such as age), allowing for fair comparisons."),
                           why.adjust.i.e.standardize = c("Primarily to compensate for differential age distributions among comparison populations (to remove the influence of age when comparing rates between two populations)."),
                           the.two.methods.of.standardization = c("direct","indirect"),
                           direct.standardization_what.it.uses.and.what.you.apply = c("Uses the age distribution of an external reference population (the standard).","You apply age-specific rates observed in two or more study populations to the standard population of known age structure."),
                           what.should.the.standard.population.be.for.direct.method.rate.adjusted.comparisons = c("It should be close to ","Could be the combination of the study populations.","You could also just use the overall state or national population."),
                           what.do.you.need.to.use.direct.method.of.adjustment = c("# in each age group of populations being compared","observed # of deaths in each age group","# in each age group of standard population."),
                           direct.adjustment.procedure = c("1) calculate age-specific (AS) rates for each comparison population","2) multiply the AS rates by their corresponding standard population to get the expected # (E) of deaths for each age group","3) sum the E's to get total for all age groups","4) divide expected total by the total standard population"),
                           interpretation.of.directly.standardized.rates_example.with.male.female = c("The interpretation: 'After adjusting for age, males have a higher mortality rate than females.'"),
                           indirect.standardization_when.used.and.what.it.does = c("Used when age-specific rates are unavailable in the study population.","Computes standardized morbidity/mortality ratio (SMR), which is O/E.","You calculate expected numbers by applying rates of the standard population to the study population."),
                           what.do.you.need.to.use.indirect.method.of.adjustment = c("The number in each age group for the comparison populations.","The total number of deaths in the comparison populations.","THE AGE-SPECIFIC (AS) DEATH RATES FOR THE SELECTED STANDARD POPULATION."),
                           indirect.adjustment.procedure = c("Multiply the standard population AS death rates by # in each age group of comparison popultions to get the expected # of deaths (E).","Sum the E's.","Divide O by E to get SMR."),
                           interpretation.of.indirectly.derived.SMR = c("More or less or the same number of deaths occurred than would be expected based on the rates in the reference population."),
                           SMR.is.not.equal.to.the.indirectly.standardized.rates_So.here.is.the.calculation.for.indirectly.standardized.rates = c("You multiply the SMR by the crude mortality rate FOR THE STANDARD POPULATION."),
                           three.facts.about.standardized.rates = c("Changes in the standard affect the magnitude of the rates.","Recalculation of disease rates based on different standards DOES NOT indicate changes in disease incidence or mortality, DUH.","The same standard population should be used when comparing age-adjusted rates."),
                           advantages.and.disadvantages.of.measures_frequency.or.raw.count = c("Advantages: ...","It's the actual number of events","It's useful for determining need for services/programs.","Disadvantage: ...","Influenced by the size of the population."),
                           advantages.and.disadvantages.of.measures_crude.rates = c("Advantages: ...","It's a summary rate.","It's easily calculated","It's the risk of dying in the population for a given period of time.","Disadvantages: ...","It's influenced by population characteristics, which may confound comparisons."),
                           advantages.and.disadvantages.of.measures_specific.rates = c("Calculated for homogeneous subgroups","Comparisons of different populations (??? - not if they have different age characteristcs, right?)","Identifies subgroups at risk","Disadvantage: ...","Cumbersome to compare more than two subgroups of two or more populations of interest."),
                           advantages.and.disadvantages.of.measures_adjusted.rates = c("Advantages: ...","Differences in the adjusted factor between populations is removed","Permist unbiased comparison relative to adjusted factor","Disadvantages: ...","It produces an artificial rate","It does not represent the risk of dying!","Absolute magnitude is dependent on standard population chosen.")
                           )
Epi.advantages.and.disadvantages.of.measures = list(one = "filler",
                                                    advantages.and.disadvantages.of.measures_frequency.or.raw.count_2.and.1 = c("Advantages: ...","It's the actual number of events","It's useful for determining need for services/programs.","Disadvantage: ...","Influenced by the size of the population."),
                                                    advantages.and.disadvantages.of.measures_crude.rates_3.and.1 = c("Advantages: ...","It's a summary rate.","It's easily calculated","It's the risk of dying in the population for a given period of time.","Disadvantages: ...","It's influenced by population characteristics, which may confound comparisons."),
                                                    advantages.and.disadvantages.of.measures_specific.rates_3.and.1 = c("Calculated for homogeneous subgroups","Comparisons of different populations (??? - not if they have different age characteristcs, right?)","Identifies subgroups at risk","Disadvantage: ...","Cumbersome to compare more than two subgroups of two or more populations of interest."),
                                                    advantages.and.disadvantages.of.measures_adjusted.rates_2.and.3 = c("Advantages: ...","Differences in the adjusted factor between populations is removed","Permits unbiased comparison relative to adjusted factor","Disadvantages: ...","It produces an artificial rate","It does not represent the risk of dying!","Absolute magnitude is dependent on standard population chosen.")
                                                    )

Epi.Exam1.MyQuestions = list(one = "filler",
                             why.are.pneumonia.and.influenza.lumped.together.in.mortality.reports = c(),
                             is.it.useful.or.nonsense.to.the.medical.community.to.say.someone.died.of.senility.or.old.age = c(),
                             what.is.the.screening.effect = c(),
                             is.syndromic.surveillance.the.only.one.used.to.monitor.disease.trends_slide24 = c())
Epi.Exam1.Problems.I.Missed.and.Problems.of.Clarity = list(one = "filler",
                                                           what.does.extent.mean.in.relation.to.the.extent.of.disease.and.what.prevalence.and.incidence.measure = c("I asked Dr. Garwe what she meant by 'extent' in the following statement (you're supposed to determine if it's true or false): 'Both incidence and prevalence are measures of the extent of disease in a population.' My answer: ...","My answer was that it's false, because I interpreted 'extent' as more or less how many people had it in the population, and thought of it as analogous to the burden of disease, which is only how prevalence is described in her power point on morbidity. Her power point says that prevalence is regarded as the burden of disease. She actually also answered my question about its meaning by saying extent means burden. This is what makes this question really confusing. Because if that's true, then I think I'm more right than not, since incidence DOES NOT get described in her power points as being a measurement of the burden of disaese. Oh well."),
                                                           can.you.measure.cumulative.incidence.if.not.all.persons.made.it.to.the.end.of.the.survey = c("For example, in this question 8 people started in the survey, 3 got the disease before the end. Can you calculate the cumulative incidence even though these people got the disease before the end of the study, as long as everyone else did finish the study?","My answer was no, but the correct answer was YES! You can! The cumulative incidence is that 3 people of 8 got the disease, or 3/8!! Duh! Super easy!","If a good fraction of non-diseased people had dropped out of the study early for other reasons, then it would be harder to pretend you're reporting an accurate cumulative incidence."))
#################################### EXAM II MATERIAL ####################################
Epi.Exam.II.subjects = list(one = "filler",
                            ecologic = c(""),
                            cross.sectional = c(""),
                            cohort = c(""),
                            case.control = c(""),
                            experimental.studies = c(""),
                            life.tables = c(""),
                            data.display = c(""),
                            measuring.association = c(""))
Epi.Ecological.and.Cross.Sectional = list(one = "filler",
                                          ecologic.studies.objectives = c("You want to be able to describe the ecological study design, understand its usefulness and limitations, and explain the meaning of the term 'ecological fallacy'."),
                                          hierarchy.of.evidence_6 = c("case series","ecological","cross sectional","case control","cohort","randomized controlled trial"),
                                          what.does.selection.of.study.design.depend.on_4 = c("the research question","the amount of info already known about the research question","practical considerations (feasibility)","ethical considerations"),
                                          what.does.an.ecologic.study.do_def.and.example = c("It examines the exposure-disease association among aggregates of people--usually defined by geographic groupings.","For example: patterns of mortality from CHD and per capita cigarette consumption by state in the US."),
                                          unit.of.analysis.of.ecological.study_also.how.unit.is.defined = c("The group!","Groupings are defined by geographic area and time (ecological trend study)."),
                                          measures.used.in.ecological.studies_3 = c("A summary measure of the frequency of exposure for each study unit (e.g. per capita cigarette consumption)","A summary measure of the frequency of outcome for each study unit (e.g. CHD mortality)","Joint distribution of exposure and disease WITHIN INDIVIDUALS residing in each study unit is **unknown**."),
                                          correlation.coefficient = c("This is the measure of association used in ecologic studies, and quantifies the extent of the linear relation between exposure and disease across units of analysis. It reflects the direction and strength of the group-level association between exposure and disease."),
                                          measure.of.association.used.in.ecologic.studies = c("The correlation coefficient!"),
                                          when.do.we.use.the.ecologic.study.design_3 = c("As a first look at a possible association between exposure and disease at the group level - to generate hypotheses.","When only aggregate information is available on the exposure of interest.","When the exposure of interest only varies at the population level (no individual variation within a single population group)."),
                                          example.of.a.question.investigated.by.ecologic.studies = c("Have seat belt laws made a difference in motor vehicle fatality rates? - compare motor vehicle fatality rates from years before and after seat belt laws were passed"),
                                          types.of.data.used.in.ecologic.studies_3 = c("incidence","prevalence","mortality - used most often since it's readily available"),
                                          advantages.of.ecological.studies_5 = c("1) makes use of existing data","2) does not require direct contact with individual subjects","3) can be conducted quickly and inexpensively","4) useful for generating hypotheses regarding disease etiology","5) essential for studying ecologic relationships (well duh. The ecologic study is essential for ecologic relationships. wow.)"),
                                          value.of.ecologic.studies = c("It may suggest promising avenues of research regarding etiologic relationships (e.g. think smoking and lung cancer, fluoridated water and caries, or per capita income and pellagra)."),
                                          ecologic.fallacy_3 = c("occurs when one draws inappropriate conclusions regarding relationships at the individual level based on group level (ecologic) data","error in inference due to failure to distinguish between correlation within a population vs within individuals","may be ascribing to members of a group characteristics they in fact do not possess as individuals."),
                                          limitations.of.ecologic.studies_3 = c("group data, not individual data (cannot infer results to the individual)","info on confounders generally not available (cannot adjust for confounding variables)","they do not demonstrate causal relationships (can only be consistent or not consistent with a causal association)"),
                                          cross.sectional.studies.objectives = c("Describe the cross-sectional approach to studying associations between exposure and disease, describe ways to ascertain exposure status and disease status among study populations, and calculate and interpret measures of association from cross-sectional studies."),
                                          draw.the.diagram.of.studies_IDENTIFYING.EPIDEMIOLOGIC.STUDY.DESIGNS = c("draw it, "),
                                          what.does.a.cross.sectional.study.do_3 = c("aka it's a prevalence study duh","provides a snapshot of the population at a specific point in time","looks for the coexistence of exposure and outcome in individuals at the same point in time"),
                                          features.of.a.cross.sectional.study_7 = c("1) exposure and disease measures made at the INDIVIDUAL level","2) exposure and disease status determined at the same point in time for each participant","3) cases identified are PREVALENCE cases","4) often used for chronic diseases (extended preclinical period and long duration)","5) sero-prevalence studies","6) not useful for rare diseases or those of very short duration","7) often sample the general population--so cross-sectional studies are generalizable"),
                                          uses.of.cross.sectional.studies_3 = c("estimation of the magnitude and distribution of a health problem--measures the prevalence proportion","hypothesis generation","intervention planning"),
                                          design.of.cross.sectional.study = c("Gather data from a defined population so you can get your four possible categories, a-d (you know what I mean)"),
                                          ways.of.assessing.exposure_5 = c("1) questionnaires","2) records","3) lab tests","4) physical measurements","5) other special procedures - e.g. toenail clippings for selenium"),
                                          exposure.issues_4 = c("date of onset of exposure not captured","past exposure not captured","duration of exposure not captured","only exposures that do not change or those that 'track'--e.g. genetic factors, serum levels of persistent pesticides--can be considered as preceding disease"),
                                          measurement.of.disease.for.cross.sectional.studies_2_a3 = c("A) Methods of measurement (3): ...","1) questionnaire / medical records","2) physical exam","3) special procedures e.g. lab tests","B) Defined diagnostic criteria, which may divide cases into DEFINITE, PROBABLE, and POSSIBLE disease categories."),
                                          interpretation.of.prevalence.proportion.ratio.in.cross.sectional.studies = c("The prevalence of existing GI illness, for example, is 2.67 times higher in persons exposed to water contaminated by coliform bacteria than in persons not exposed to contaminated water. (You get this RATIO simply by dividing the exposed prevalence by the unexposed prevalence)"),
                                          advantages.of.cross.sectional.studies_3 = c("If based on a random sample of the general population, results can provide prevalence estimates for the entire population. So, you can estimate population prevalence proportions of disease and exposure.","Provides preliminary examination of disease-exposure association.","Relatively short time needed for study and lower costs."),
                                          limitations.of.cross.sectional.studies_4 = c("temporal sequence not determined (THE EXCEPTION IS IF EXPOSURE IS A PERMANENT FACTOR)","potential for biased inference in prevalent cases (are they like incident cases?) e.g. survival bias","cases in remission or treated may be missed","not appropriate for rare or short duration diseases"),
                                          Job.A.and.Job.B.cross.sectional.study.example.of.this.studys.limitations_2 = c("100 workers start out in Job A (hazardous) and Job B (nonhazardous) each. 20 people in A get illness. 10 of them switch from A to B due to illness. 5 in B also get illness. You end up with 15 out of 110 ill in B, and 10 out of 90 ill in A. That leaves B with a prevalence of 13.6%--more than A's 11%, even though A is the hazardous one.","Why did this happen to a cross sectional study? ...","Because in cross sectional studies, prior exposure status is usually not determined--only their current status is--and people can change exposure status over time, so current status may not reflect past exposures. And, that change in status might even by related to the outcome you are studying, as in this case."),
                                          can.cross.sectional.studies.suggest.associations.between.exposure.and.disease = c("Yes. This is called hypothesis generation."),
                                          can.cross.sectional.studies.establish.causal.relationships_2 = c("No! There is no temporal sequence.","Prevalence cases are a function of both disease incidence (etiology) and disease duration (survival)."),
                                          what.is.the.measure.of.association.for.cross.sectional.studies_is.there.one = c("There doesn't appear to be one right now. I'll have to ask Dr. Garwe.")
                                          )
Epi.Cohort.and.Case.Control = list(one = "filler",
                                   cohort.study.objectives = c("Describe three types of cohort designs; describe appropriate methods for selecting the study population (especially the selection of comparison groups); discuss ways to ascertain risk factor or expsoure status among study populations and techniques to measure the outcome of interest; list advantages and disadvantages."),
                                   what.is.a.cohort = c("a group of people who share a common experience","Persons born the same year are a birth cohort; persons who share a common behavior - like a cohort of smokers; or persons in the same class."),
                                   what.does.a.cohort.study.do = c("Exposed and unexposed individuals are followed (forward in time) to determine the incidence of disease in each group."),
                                   the.design.of.a.cohort.study.begins.with.a... = c("It begins with exposed and not exposed, and then goes on to find out how many of the exposed/unexposed are disaeased / not diseased."),
                                   prospective.cohort.study.design_2 = c("aka concurrent/longitudinal","The investigator collects info on the exposure status of study subjects at the time the study begins and identifies new cases of disease that develop from that time on, until the end of the follow-up interval."),
                                   retrospective.cohort.study.design_2 = c("aka non-concurrent/historical","The investigator determines exposure status from info recorded at some time in the past, and disease status is determined from that point in the past up until the present (i.e. the follow-up period has already occurred)."),
                                   ambi.directional.cohort.study.design = c("aka combined non-concurrent/concurrent","Cohort and exposure status is identified from past records, followed to the present, and then followed into the future. Most often used when additional follow-up time is needed."),
                                   three.approaches.to.selection.of.study.population.in.cohort.studies_each2 = c("A) A general population sample, making the results highly generalizable (like Framingham; Strong Heart Study)","1. You can explore the roles of many exposures","2. It's expensive, labor intensive and may have problems with loss of follow up.","B) Special cohort - a defined population based on membership in a particular subgroup of interest, making results less widely generalizable","1. follow up will be easier","2. examples: Nurses Health Study; US Veterans","C) Selection based on presence of a distinctive exposure - selected because they are known to be exposed to a certain factor, like atomic bomb survivors or people exposed to chemicals on the job. So, not generalizable.","1. This is generally used for occupational cohorts.","2. Cohorts may be stable and easy to follow."),
                                   what.is.the.reference.group.for.the.exposed_3 = c("A) Internal comparison - typically from the same population as the exposed group (e.g. in the Framingham, people with low cholesterol levels)","B) External comparison - outside of the exposed group (e.g. another study's data, or general population data)","C) Combined - both internal and external comparison groups can be used."),
                                   what.are.the.characteristics.of.a.cohort.study_2 = c("Can be population based or non-population based (???)","Can be open cohort (persons enter and leave over the course of follow-up) or closed cohort (begin with a fixed study group; persons may leave but no new members enter)."),
                                   assessment.of.exposure.in.cohort.studies_5 = c("A) Sources of exposure data (4): ...","interview","questionnaires","existing records","physical exam","B) Definition of exposure: what will constitute exposure?","C) Timing of exposure - onset","D) Quantifying exposure: frequency (continuous or intermittent?), intensity, and duration","E) Changes in exposure status - how to deal with them?"),
                                   measurement.of.disease.in.cohort.studies_5 = c("determine disease-free status at the START of the study","outcomes must be clearly defined and measurable--who is an incident case?","how will you determine whether or not disease has occurred?","procedures should be well-defined and the same for both exposed unexposed groups","best if assessment of disease incidence is done blinded to exposure status!!!"),
                                   outcome.measures.in.cohort.studies_cumulative.incidence_3 = c("CI: all cases known to have occurred in the baseline cohort during the follow up time divided by the study population at baseline, per unit of time. So, the numerator is # of incident cases, denominator is # of people in study population.","It has to be a closed cohort.","CI is a ***RISK*** measure."),
                                   outcome.measures.in.cohort.sutides_incidence.density_3 = c("ID: all cases known to have occurred in the baseline cohort during the follow up time, divided by the amount of 'at risk' experience (usually in units of person-time) contributed by all members of the cohort. So, the numerator is # of incident cases, denominator is a blend of # of people and their time at risk.","It can be open or closed cohort.","ID is a ***RATE*** measure."),
                                   CIsub.E = c("incidence of disease in exposed (think of two-way table 2nd grader calculation)"),
                                   CIsub.noE = c("incidence of disease in unexposed (think of two-way table 2nd grader calculation)"),
                                   CIsub.T = c("overall disease incidence (think of two-way table 2nd grader calculation)"),
                                   RR.for.cohort.study.outcome.measures = c("ratio of the two incidence measures, with CIsub.E on top, CIsub.noE on bottom"),
                                   interpretation.of.RR.in.cohort.studies_what.part.to.not.forget = c("***Over an 18-year follow up period,*** a Framingham male with HBP was 3.32 times more likely to develop CVD than a Framingham male without HBP. You could say the RISK of developing CVD was 3.32 times greater in males with HBP than in males without HBP ***over an 18-year period***.","So, the part to not forget is the ***over an 18-year period*** part."),
                                   measure.of.association.in.cohort.studies.with.person.time_3 = c("Incidence Rate Ratio (IRR)","IRR = incidence rate in exposed divided by incidence rate in unexposed, so, calculating this means ...","... taking the # of diseased in exposed, divided by PT in exposed, and dividing that by the same quotient (rate in this case) in unexposed"),
                                   intrepretation.of.IRR.in.cohort.studies = c("If the IRR is 1.5 then the incidence rate of the disease is 1.5 times higher in exposed individuals compared to unexposed individuals."),
                                   attributable.risk.in.the.exposed_cohort.studies = c("AR: How much of the disease in people who are exposed is due to the exposure? This is a very sensible question to ask. If people without the exposure also get sick, then you could imagine that some of the sickness in the exposed group might not actually be due to the exposure. The simple way to account for this is simply to subtract off from the CIsub.E the CIsub.noE--that is, the difference between the two proportions."),
                                   attributable.risk.percentage_cohort.studies = c("AR%: What percentage of the disease in the exposed is due to the exposure? Simply take your AR and divide by CIsub.E, then multiply by 100, duh"),
                                   interpretation.of.AR.and.ARpercent = c("128 cases of CVD per 1000 persons with HBP can be attributed to their HBP. AR%: about 70% of CVD among HBP people is because of their HBP."),
                                   assumptions.of.AR_cohort.studies = c("The association is CAUSAL (since you are making attribution).","All other factors in exposed and unexposed are equally distributed."),
                                   what.is.the.public.health.importance.of.a.given.exposure = c("Its importance is a function of how much it increases the risk of disease (size of RR or OR), how common the disease is, and how common the exposure is."),
                                   population.attributable.risk_cohort.studies = c("There isn't just the question of how many of the exposed, diseased people got their disease from the exposure. A slightly different question is how many of all the diseased people got their disease from the exposure. This sounds like it could be complicated, but thankfully you just subtract off the exact same thing as in the AR calculation (the UˆD/U), but this time from the CIsub.T.",""),
                                   population.attributable.risk.percentage_cohort.studes = c("In addition to the question, 'How much of the disease in the population is due to exposure?', you can do a percent: 'What percentage of the disease in the population is due to the exposure?'","Simply take the PAR and divide it by CIsub.T (same as the principle followed with the AR% and AR, where AR% is AR/CIsub.E)"),
                                   interpretation.of.PAR.and.PARpercent = c("35 CVD cases per 1000 in the population are due to HBP. 39% of CVD in the population is due to HBP."),
                                   advantages.of.cohort.studies_5 = c("1) more certain of the temporal relationship between exposure and disease","2) multiple effects of a single exposure can be assessed","3) bias in ascertainment of exposure is minimized (i.e. cannot be biased by knowledge of outcome)","4) can more easily asssess changes in risk factor status","5) direct measurement of INCIDENCE of disease."),
                                   limitations.of.cohort.studies_4 = c("1) should initially include individuals free of disease, but disease process may have already begun and not yet be detected","2) expense and time required may limit feasibility","3) loss to follow up","4) not suitable for rare diseases (that would require too large a cohort)"),
                                   summary.of.cohort.studies_4 = c("The PRIMARY measures of association from a cohort study are RR or IRR. You 'can also calculate an OR'.","What is compared, and between whom, in a cohort study? ...","... Disease incidence is compared, between exposed and unexposed.","Comparisons for the exposed group's experience can be internal, external, or both.","The two measures of incidence that are calculated in cohort studies: ...","... CI and ID."),
                                   QUESTIONS.ABOUT.COHORT.STUDIES = c("Why are AR/AR% and PAR/PAR% not the primary measures of association in a cohort study?"))
Epi.Case.Control.Only = list(one = "filler",
                             case.control.objectives = c("describe the case-control approach to studying exposure-disease associations, the appropriate methods for selecting case and control groups, the purpose of matching cases and controls and list the two matching methods; understand the methods used for gathering and quantifying exposure information, and list advantages and disadvantages of case control design"),
                             selection.in.case.control_2basics_see.selection.of.controls.for.more = c("on the basis of disease status, WITHOUT REFERENCE TO (KNOWLEDGE OF) THEIR EXPOSURE HX","A case-control study begins with people who have the disease (cases) and compares them to people who don't have the disease (controls)."),
                             comparison.in.case.control = c("You compare the odds of a **PAST** exposure to a suspected risk factor between cases and controls."),
                             case.contro.rubella.cataracts.example = c("cases should be children with cataracts; controls should be children without cataracts; for each child we would determine whether or not their mother was exposed to rubella during pregnancy"),
                             issues.of.case.selection_3underlined = c("Diagnostic critera - clearly defined, objective, standardized criteria (the objective is to produce a uniform, ***HOMOGENEOUS*** group of cases)","***CRITERIA FOR ELIGIBILITY*** - clearly defined reasons to include/exclude cases (e.g. by age, gender, potential for exposure, applied equally to cases and controls)","e.g. A study of recent OC use and MI would exclude males and post-menopausal and surgically sterilized women because they have no risk of ***RECENT*** exposure. Including them would bias the results towards the null."),
                             incident.vs.prevalent.cases_which.preferred.and_3why = c("Incident cases are preferred, because ...","... this reduces potential for I/P bias (P = IxD). The dxs are more likely to be uniform, using same criteria; recall of exposure may be better."),
                             case.and.control.selection.blinding = c("cases and controls are selected without reference to (without knowledge of) their exposure history"),
                             sources.of.cases_advantages.and.disadvantages_2_plus.other = c("hospital-based - advantages: easier, cheaper","hospital-based - disadvantages: 1) potential for biased sample of cases 2) referral patterns 3) only suitable for diseases that are usually hospitalized","community or population sample - advantages: representative case group","community or population sample - disadvantages: 1) costly 2) time-consuming 3) difficult to do without registry","other sources of cases: registries which are not population-based; large pre-paid insurance plans; retirement communities"),
                             selection.of.controls_10 = c("controls provide a comparison group (duh) and are intended to represent the frequency of exposure in the population from which the cases arose (duh)","controls are selected from the same source population as the cases were chosen (duh!)","controls are free of disease under study (duh!)","controls are usually similar to cases with regard to past POTENTIAL for exposure, during the same period of risk under study","select controls in the same manner as selection of cases duh","eligibility criteria - any exclusion "),
                             some.stipulations.on.types.of.controls_2 = c("The guiding principle for the valid selection of cases and controls is that they represent the same base or source population. **Had the control become disease, they would have been eligible to become a case.**"),
                             community.and.general.population.controls_3.plus.advantages.and.disadvantages = c("controls are selected from a random sample of the general population","random-digit dialing commonly used","appropriate if cases are population-based","advantages: highly representative, good for population frequency of exposure, appropriate for population based cases","disadvantages: costly, probles with refusal and phone coverage, increase in cell phone usage (WTF is that a disadvantage for???)"),
                             hospital.based.controls_4.plus.advantages.and.disadvantages = c("the controls are people seeking medical care at the same institution as cases for conditions unrelated to disease under study","exclude persons with diseases known/suspected to be related to exposure under study","the illness of the controls should have the same referral patterns to the health care facility as that of cases","may use multiple dxs (???)","advantages: captive population, clearly defined, economical, less recall bias","disadvantages: potential for selection bias, less generalizable"),
                             neighborhood.controls_plus.advantages.and.disadvantages = c("selected from same neigborhood, via phone number or canvassing","advantages: provides controls of similar socioeconomic status and environment as cases","disadvantages: overmatching possible, low response rates"),
                             multiple.types.of.controls_plus.advantages.and.disadvantages_plus.advantages.and.disadvantages_plus.example = c("used to assess potential bias","advantages: replicate","disadvantages: costly; if results differ, you need to be able to explain why"),
                             representativeness.vs.comparability_defs.and.adv.disadv.for.each = c("representativeness = the generalizability of the study","advantages: increasing generalizability decreases potential for selection bias","disadvantages: may be more difficult time consuming, and resource intensive","comparability = internal validity: cases and controls have equal proability of past exposure if there is not association between exposure and disease","advantages: easier to detect smaller differences, reduces chances of unmeasured confounding","disadvantages: may have select group of cases; potential for Berkson's bias (hospital only)"),
                             assessment.of.exposure.in.case.control.studies_2_plus.2underlined = c("EXPOSURE IS PRIOR TO DISEASE ***ONSET*** (OR REFERENCE DATE FOR CONTROLS)","techniques (???): info on prior exposure in cases and controls may be ascertained through personal interview, medical records, employment records, pharm or lab records, or direct measurement","TIMING OF EXPOSURE: when did it occur in relation to disease onset or index date?","QUANTIFICATION OF EXPOSURE: for dose response analysis (includes amount, frequency, and duration of exposure)"),
                             measure.of.association.in.case.control.studies = c("OR: a ratio of two odds--the odds that the cases were exposed, and the odds that the controls were exposed."),
                             interpretation.of.OR_cigs_brain.tumors = c("The odds of having smoked cigs among cases of lung cancer are 14 times greater than the odds of having smoked cigs among controls.","The odds of having been born high birth weight are 2.57 times higher in children with brain tumors than in children without brain tumors."),
                             what.about.risk.ratio.in.case.control.studies_3 = c("You cannont, in most circumstances, use case-control data to calculate incidence and thus, you cannot directly calculate risk ratios from case-control data.","Study groups are chosen on the basis of presence of disease, not exposure.","Odds ratios are good approximations of the risk ratio if your study is properly designed."),
                             two.methods.for.matching.of.cases.and.controls.plus.definition = c("Matching is the selection of controls so that they are similar to cases for characteristics that might be confounding variables (such as age, gender, race, SE status, etc.)","1) individual (pair) matching: selecting one or more controls for each individual case (if a white male 67 years old got an MI, get one or two white males in same age range who didn't get MIs)","2) frequency (group) matching: the control group is selected so that its distribution is similar to that of cases for potentially confounding variables such as age, gender, race, etc."),
                             explanation.of.matched.pair.analysis = c("The odds of having been born high birth weight are 2.57 times greater in children with brain tumors than in children without brain tumors. This is for the situation with the 2x2 table (cells a,b,c,d) of cases on the left side, controls on the top, with >8lbs in top left corner, <8lbs lower and to the right, with a=8, b=18, c=7, d=38. A and D are uninformative and not used in the calculation. B would need to be divided by C (18/7 = 2.57) to get an OR of how much higher the odds of being born high weight was for tumor babies."),
                             advantages.of.case.control.design = c(""),
                             disadvantages.of.case.control.design = c(""),
                             summary.of.case.control.studies_4 = c(""),
                             QUESTIONS.ABOUT.CASE.CONTROL.STUDIES = c("In a case-control study of breast cancer and pesticide exposure cases are chosen from tumor registries of all major hospitals in Klahoma Country. What is the 'source population' for these cases? ... Uh ... The hospital maybe?? Should controls be picked via hospital records of people with the 'same referral patterns to the health care facility as that of cases'?"))
Epi.Experimental.Studies.and.Life.Tables = list(one = "filler",
                                                experimental.studies.objectives = c("define different types of experimental studies, describe key elements of a clinical trial, distinguish between clinical significance and statistical significance, escribe biases that may affect trials, and calculate and interpret measures of tx effect in clinical trials"),
                                                types.or.terms.of.experimental.studies = c("experimental study, clinical trial, community intervention trial, quasi-experimental study (exposure is controlled by the investigator, but there is no randomization)"),
                                                experimental.vs.observational.studies_4 = c(""),
                                                community.intervention.trial_3_plus.example = c(""),
                                                clinical.trials.are.useful.for.evaluating_6 = c(""),
                                                types.of.clinical.trials_3 = c(""),
                                                what.is.a.clinical.trial_3 = c(""),
                                                real.examples.of.clinical.trials = c(""),
                                                elements.of.a.clinical.trial_6 = c(""),
                                                non.concurrent.control.group.is.subject.to.what.and.its.related.to.what_4 = c(""),
                                                advantages.of.random.allocation.to.tx = c(""),
                                                does.stratification.occur.before.or.after.randomization = c("Before duh.","Stratification is used when there is an important known prognostic factor like tumor stage that an be measured before randomization; this ensures balance in tx groups."),
                                                masking.or.blinding = c("Masking/blinding is a method of concealing knowledge of tx assignment to reduce bias in reporting or measuring outcome."),
                                                single.double.triple.blind = c(""),
                                                type.I.and.II.errors = c("type I (alpha) is a false positive conclusion, rejecting a true null, and in Garwe's words, concluding that a new tx is better when it really isn't.","type II error (beta) is a false negative, failing to reject a false null, and in Garwe's words concluding a new tx is not better when it really is."),
                                                true.negative.and.true.positive = c("1 - beta = the power of the test, or the probability of getting a true positive; 1 - alpha = the probability of getting a true negative"),
                                                what.you.need.to.calculate.sample.size_3 = c("alpha","beta","the rate of events in the control group","the size of the absolute difference in outcomes (absolute difference = the rate of events in the control group - the rate of events in the experimental group"),
                                                sample.size.relations_2 = c("if alpha, rate of events, and absolute difference are constant: as the sample increases, the power increases","if alpha, power, and rate of events are constant: as the size of the absolute difference increases, the sample size can decrease"),
                                                CER.and.EER = c("CER = control event rate = # of patients in control group who had an event divided by the total # of patients in the control group","EER = experimental event rate = same as CER equation but experimental group instead of control group"),
                                                measures.of.treatment.effects.in.experimental.studies = c("When the experimental tx reduces the probability of a bad outcome (e.g. death), how do you measure the amount of effect it has? ...","ARR and RRR (absolute risk reduction and relative risk reduction)"),
                                                ARR.equation.and.RRR.equation = c("ARR = | EER - CER | ... this will tell you that, for example, there were 15 ***per 100*** fewer deaths in the experimental group compared to the control group","RRR = | EER - CER |/CER ... this will tell you, for example, that there was a 60% relative reduction in death in the experimental group compared to the control group"),
                                                number.need.to.treat.NNT = c("This is another approach for expressing the results of randomized trials that is useful for clinical medicine. It's an estimate of the number of patients who would need to be treated in order to prevent ONE adverse outcome.","NNT = 1/| CER - EER | ... essentially it's the same as 1/ARR (cuz the absolute value eliminates the difference) ... So you could say, for example, if the NNT was 1/.15 or 6.7, that you would need to treat 7 people in order to prevent one bad outcome."),
                                                clinical.significance.vs.statistical.significance_how.do.you.assess.them = c(""),
                                                error.in.clinical.trials = c("bias","chance"),
                                                types.of.bias.in.clinical.trials = c("contamination","co-intervention","diagnostic suspicion bias","interpretation bias"))
Epi.ppt.on.Measuring.Association.Between.D.and.E = list(one = "filler",
                                                        cohort.study.vs.case.control_measures.of.association = c("Cohort: RR, IRR, and **DISEASE** OR","Case-control: **EXPOSURE** OR"),
                                                        cross.sectional.measure.of.association = c("PPR (ratio of prevalence proportions)"),
                                                        RR.equation_AND.WHAT.THE.HECK.IS.RISK.YOU.BETTER.KNOW.THIS.BY.NOW = c("risk in exposed divided by risk in unexposed ... or, P(D|E)/P(D|NE).","RISK = CUMULATIVE INCIDENCE"),
                                                        IR = c("IR = # of cases divided by person-time"),
                                                        difference.between.risk.and.IR_3 = c("Risk: # at risk is in the denominator","IR: time is in the denominator","IRR is a ratio of two rates, whereas RR is a ratio of two risks."),
                                                        what.is.relative.risk = c("It depends on who you ask!","It could be risk ratio (RR), or it could be incidence rate ratio (IRR); so it's an umbrella term for either risk or rate ratio. When deciding which to apply the term to, you should consider the study design and type of data collected, and contextualize the term accordingly."),
                                                        Risk.is.synonymous.with = c("CI, and Incidence Proportion (IP?)"),
                                                        Odds.Ratio.can.be.calculated.for.which.studies_the.no.brainer.and.the.WTFs_plus.explanations = c("For case-control, duh.","Oddly, it can also be calculated for the cohort (not too weird) and the cross-sectional (WTF??) studies.","Explanation: in case-controls, OR is the only option, and it's exposure OR. In cohort, OR would be disease OR (incidence), whereas in cross-sectional, OR would also be disease OR (but prevalence, not incidence)."),
                                                        OR.interpretation.in.cohort.and.cross.sectional_plus.further.terminology.confusion = c("Cohort: OR is the odds of **DEVELOPING** disease in exposed vs unexposed.","Cross-sectional: OR is the odds of **HAVING** disease in exposed vs unexposed.","You know how one of the slides in this ppt says disease OR is applicable to both cohort and cross-sectional? Well, now you should know that the term 'disease OR' is more applicable to cohort, and 'prevalence OR' is what you'd say if you're using OR in a cross-sectional study."),
                                                        when.is.the.OR.obtained.in.a.case.control.study.a.good.approximation.of.the.RR.in.the.population_3 = c("1) when cases are representative of all cases in the population.","2) when controls are representative of all controls in the population","3) when the disease is rare"),
                                                        PPR_plus.comparison.to.RR.VERY.IMPORTANT.INFO = c("Used in cross-sectional studies, it's the ratio of the disease prevalence proportion in exposed to the disease prevalence proportion in the unexposed.","IT IS MATHEMATICALLY IDENTICAL TO THE RR, BUT THE INTERPRETATION IS VERY DIFFERENT!"),
                                                        beyond.estimating.association_RR.IRR.OR.PPR_if.we.want.to.estimate.potential.for.prevention.then.we.measure = c("AR, AR%, PAR, PAR%")
                                                        )
Epi.ppt.on.data.sources.and.rules.of.data.display = list(one = "filler",
                                                         objectives.of.data.sources.and.rules.of.data.display = c("1) identify sources of health data, 2) discuss strengths and limitations of secondary data for epi studies, 3) describe guidelines for tables and charts/graphs, 4) contrast weak and effective data presentation, and 5) use appropriate graphical methods to display data."),
                                                         primary.and.secondary.data_defs = c("Primary: data collected by the researcher","Secondary: existing data collected during other studies or at other institutions, or for state or national surveillance purposes."),
                                                         strengths.of.primary.data_3 = c(""),
                                                         limitations.of.primary.data_5 = c(""),
                                                         strengths.of.secondary.data_5  = c(""),
                                                         limitations.of.secondary.data_6 = c(""),
                                                         other.issues.with.secondary.data_5 = c(""),
                                                         data.source.considerations_3_a4_b3obvious_c4 = c(""),
                                                         data.quality.and.access.things.to.think.about_5 = c(""),
                                                         linkage.of.data.sources_2 = c("Consolidating multiple data sources for the same individual - e.g. linking birth and death files to study childhood diseases.","Probabilistic matching software: uses statistical distributions of key variables (last name, day/month/year of birth) to determine probability of matches. WHAT THE HECK IS THIS????"),
                                                         types.of.data.used.in.public.health.and.epi.studies_6 = c("demographic data","vital statistics data","surveillance data","health status and behavioral data","socioeconomic data","utilization data"),
                                                         demographic.data.i.e.census_4 = c("decennial survey","American Community Survey","CENSUS DATA IS THE MAIN SOURCE OF DENOMINATOR DATA","www.census.gov or OK Dept of Commerce"),
                                                         vital.statistics.data_3 = c(""),
                                                         vital.statistics.data.problems_5 = c(""),
                                                         health.status.data_7 = c(""),
                                                         medical.records_2_a4 = c("A) clinical: ...","1) admission dxs","2) lab reports","3) discharge dxs","4) medication records","B) demographics, socio-cultural, economic, administrative, attitude"),
                                                         features.of.medical.records.data_2 = c("It contributes info on morbidity, mortality, and contributes to other health status studies","There's a lack of uniformity in recording info (data may not be complete)"),
                                                         behavioral.risk.data_6 = c(""),
                                                         other.Oklahoma.sources_3 = c(""),
                                                         social.and.business.statistics_5 = c(""),
                                                         health.resources.data_5 = c("There are numbers of ...","health professionals","hospital beds","outpatient facilities","long term care facilities","hospice services"),
                                                         utilization.data_7 = c("")
                                                         )
Epi.rules.of.data.display.only = list(one = "filler",
                                      data.display.should.be = c("self-explanatory and stand alone"),
                                      choose.your.table.or.chart.based.on_3_or.5 = c("1) purpose, 2) audience, 3) type of data","amount of data to be presented","level of detail needed","relationship within the data you wish to communicate","type of presentation","type of audience"),
                                      tables_data.display_4 = c(""),
                                      charts.and.graphs = c("Use bars, lines, or dots to show trends, patterns, and relationships across data set.","Should be used when general pattern is more important than exact data values.","Useful in oral and poster presentations, for non-technical audience but also manuscipts and technical reports."),
                                      pie.charts = c("display parts to a whole","not easy for the audience to copmare angles, so heights of bars is easier","pie charts can be deceiving (in what ways???)"),
                                      bar.charts = c("for comparing quantities of categorical data [BUT PIE CHARTS ARE ALSO FOR CATEGORICAL DATA]"),
                                      line.graphs = c("series of data points connected by straight lines","used to show change over some type of order (usually time)"),
                                      scatter.plots = c("used to show correlations, trends, and outliers"),
                                      guidelines.for.charts.and.graphs = c("clear and concise title, scale should start at zero, any break in scale should be labeled, avoid clutter and 3D effects, and use legend or proper labels on bars/lines")
                                      )
#################################### EXAM III MATERIAL ####################################
Epi.Exam.III.subjects = list(one = "filler",
                             association_causation = c(""),
                             bias.in.study.designs = c(""),
                             confounding_effect.modification = c(""),
                             critical.appraisal.of.studies = c(""),
                             ethics = c(""),
                             evaluation.of.interventions = c(""),
                             screening = c(""))
Epi.association_causation = list(one = "filler",
                                 what.is.an.association = c("correlation, relationship, statistical dependence - two events occur more frequently together than one would expect by change"),
                                 four.types.of.association = c(""),
                                 properties.of.a.causal.factor_3 = c("statistical dependence (must be associated with outcome)","time order - (must precede outcome)","direction (a change in exposure must lead to a change in outcome)"),
                                 necessary.cause = c(""),
                                 sufficient.cause = c(""),
                                 process.for.assessing.causality = c("It's a two-stage process.","1) determine if the observed result is valid by ruling out bias, confounding and random error","If association is real, use Hill's criteria to evaluate strength of evidence for causality."),
                                 Hills.criteria.list_9 = c("temporal sequence","strength of association","consistency upon repitition","dose response (biol. gradient)","biological plausibility","specificity","coherence of explanation","experimental evidence","analogy","Sir Austin Bradford Hill, Proceedings of the Royal Society Med. (1965)"),
                                 Hills.criteria.details = c("temporality - this is easiest to establish in a prospective cohort study","strength - ecological? what else?","replication - may not be possible due to differing exposure prevalence and differential susceptibility of populations","dose response - helps, but not necessary when there are threshold effects between exposure and outcome","biological plausibility - depends upon (limited by) **current knowledge** of disease etiology","specificity = exposure is associated with one disease or the disease is associated with only one factor; weakest of criteria; absence of specificity cannot rule out causality","coherence - kind of like consilience: epidemiological, lab, clinical, and other data tend to paint a consistent picture","experimental evidence (holding all other variables constant, if a change in the factor leads to a change in outcome then the relationship 'is causal') - not a guideline, but a way of testing a hypothesis; most epi studies are observational","analogy (a weak form of evidence) - an association is more likely to be causal if a similar relationship has been observed with another exposure/disease pair (e.g. think AIDS and Hep B, and HIV transmission)")
                                 )



###########################################################################################

## list of lists: a deck with all the decks in it:
deck.of.decks = list(ancient.names.3,
                     BofM.translation.facts.variable,
                     bythenumbers.3,
                     climate.two.per.lecture,
                     #definitions.variable,
                     geography.lists.variable,
                     medical.terminology.3,
                     #name.ten.variable,
                     pathogens.variable,
                     people.important.variable,
                     PharaohList.3,
                     physics.and.technology.variable,
                     political.facts.variable,
                     psalms.in.BofM.4,
                     quotes.variable,
                     random.facts.4,
                     random.lists.variable,
                     science.corroboration.variable,
                     scriptures.variable,
                     TreeLifeList.3)

length(deck.of.decks)
deck.of.decks[1] # gives the entire deck
deck.of.decks[[1]] # gives the exact same thing, minus the [[1]] business before each card
deck.of.decks[[3]][2] # gives every side of just the second card in the third deck
deck.of.decks[[3]][[2]] # gives every side of just the second card in the third deck, minus the front side
deck.of.decks[[3]][[2]][1] # gives the SECOND side (the one right after the front) of the second card
deck.of.decks[[3]][[2]][2] # gives the THIRD side of the second card
names(deck.of.decks[[3]][2]) # HOLY FREAK I FIGURED IT OUT: gives the FIRST side of the second card of the third deck



# Ancient peoples known by two phonetically dissimilar names:
ancient.names.3 = list(one = "filler",
                       Phrygians = c("Mushki","The Mushki were an Iron Age people of Anatolia who appear in sources from Assyria but not from the Hittites. Several authors have connected them with the Moschoi of Greek sources and the Georgian tribe of the Meskhi. Josephus Flavius identified the Moschoi with the Biblical Meshech. ***Assyrian sources identify the Western Mushki with the Phrygians,*** but Greek sources clearly distinguish between the Phrygians and the Moschoi. The Encyclopedia of Indo-European Culture notes that the Armenians according to Diakonoff, are then an amalgam of the Hurrian (and Urartians), Luvians and the Proto-Armenian Mushki (or Armeno-Phrygians) who carried their IE language eastwards across Anatolia.According to Greek mythographers,[9] the first Phrygian Midas had been king of the Moschi (Mushki), also known as Bryges (Brigi) in the western part of archaic Thrace.Assyrian sources from the 8th century BC speak of a king Mita of the Mushki, identified with king Midas of Phrygia. The Phrygians were an ancient Indo-European people, initially dwelling in the southern Balkans – according to Herodotus – under the name of Bryges (Briges), changing it to Phryges after their final migration to Anatolia, via the Hellespont. - Wikipedia 'Mushki' and 'Phrygians'")
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
  z = BofM.EME.4[[i]][3]
  BofM.EME.4[[i]][3] = BofM.EME.4[[i]][1]
  BofM.EME.4[[i]][1] = z
}
BofM.EME.4 # THIS IS THE ***REAL*** BMLIST!!

# BMList[3] # gives you everything
# BMList[[3]] # gives you just what's "in" scripture reference #3
# BMList[[3]][2] # gives you just the second sub-item in contents of scripture reference #3


BofM.translation.facts.variable = list(one = "filler",
                                       stream.of.dictation_Isaiah.chapters = c("'Milton Blackman, Stan Larson, and Royal Skousen have written on ... textual evidence for a dictated manuscript, citing homophonic miscues, or errors of the ear (e.g. 'no' corrected to read 'know'), scribal anticipation errors (cross-outs that appear before intervening dictation), and the general lack of revisions in the text.[89]'","'Those extensive portions that largely parallel the King James Version of Isaiah 2-14 and 48-54 and Matthew 5-7 seem, even to at least one Mormon scholar, prima facie evidence that Joseph simply used an open copy of the King James Bible, at least for those portions.[90] If so, that would contravene both the testimony of eyewitnesses and textual hints that Joseph was dictating extemporaneously. (For example, as Royal Skousen has shown, Joseph's thematic divisions of Isaiah are inconsistent with the KJV chapter groupings. Also, Cowdery's spellings in these sections mimic the spelling he used in other portions of the Book of Mormon, not King James spelling, showing the relevant passages were the product of dictation rather than scribal copying.[91]'","source: From Darkness Unto Light (?)")
                                       )


## bythenumbers.3 
# topic, number, source
bythenumbers.3 = list(one = "filler",
                      antibody.drug.industry.revenue = c("Antibodies made by the pharmaceutical industry rake in about $100 billion a year.","source: Nature News - 25 May 2018 - Heidi Ledford")
                      )


## climate random facts (variable)
# topic name, facts, sources
climate.random.variable = list(one = "filler",
                               Younger.Dryas = c("The Younger Dryas lasted from 11,000 to 9,000 years BP.","Glaciers began to melt quickly following the maximum glacial advance about 18,000 years BP. Global warming ended the most recent Ice Age beginning around 14,000 years BP.","A brief warm period called the Allerod occurred roughly 11,500 years BP, followed the cooling trend of the Younger Dryas in which glaciers began to readvance.","source: geologycafe.com"),
                               climate.since.Younger.Dryas = c("After the Younger Dryas, the Earth became dramatically warmer. The period between about 8,000 and 7,000 years BP is called the Hypisthermal/Altithermal, or Climatic Optimum. More on the Climatic Optimum to follow ...","The Climatic Optimum was the warmest period within the Holocene. Global temperatures may have been as much as 4 C higher than today. Africa was wetter, and the midwest US was more desert-like.","Since the Climatic Optimum, global temperatures have fallen haltingly: ...","A minor warm period called the Little Climatic Optimum began around 300 BC and lasted for roughly 1500 years. Then a cool period called the Little Ice Age began in the 1300s and ended in the 1840s. Earth has been warming ever since. What would normally be happening next (and right now) ...","... is that the Earth should, based on the patterns of previous interglacial stages and Milankovitch Cycles, be heading towards another ice age after the Climatic Optimum.")
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
                     The.IMF = c(),
                     biomarker = c("Any measurable indicator of some biological state or condition.","Examples: ...","It can also be a substance whose detection indicates a particular disease state, for example, the presence of an antibody may indicate an infection.","Other biomarkers can be based on measures of the electrical activity of the brain (using Electroencephalography (so-called Quantitative electroencephalography (qEEG)) or Magnetoencephalography), volumetric measures of certain brain regions (using Magnetic resonance imaging), saliva testing of natural metabolites, such as saliva nitrite, a surrogate marker for nitric oxide or analysis of urinary exosomes that carry biomarkers of disease.","One example of a commonly used biomarker in medicine is prostate-specific antigen (PSA). This marker can be measured as a proxy of prostate size with rapid changes potentially indicating cancer.","Biomarker type categories in personalized medicine: ...","... Biomarkers used for personalized medicine are typically categorized as either prognostic or predictive. ...","... Prognostic biomarkers indicate outcome regardless of treatment. Predictive biomarkers are used for estimating likelihood of benefitting from a specific therapy.","Biomarkers and precision oncology--cases of typical use: ...","... Biomarkers for precision oncology are typically utilized in the molecular diagnostics of chronic myeloid leukemia, colon, breast, and lung cancer, and in melanoma.","In cell biology: ...","... In cell biology, a biomarker is a molecule that allows the detection and isolation of a particular cell type (for example, the protein Oct-4 is used as a biomarker to identify embryonic stem cells).","In genetics: ...","... In genetics, a biomarker (identified as genetic marker) is a DNA sequence that causes disease or is associated with susceptibility to disease. They can be used to create genetic maps of whatever organism is being studied.","In geology and astrobiology: ...","A biomarker can be any kind of molecule indicating the existence, past or present, of living organisms. In the fields of geology and astrobiology, biomarkers, versus geomarkers, are also known as biosignatures. The term biomarker is also used to describe biological involvement in the generation of petroleum.","History of usage: ...","The widespread use of the term 'biomarker' dates back to as early as 1980. The term 'biological marker' was introduced in 1950s. In 1998 the NIH defined a biomarker as ...","... a characteristic that is objectively measured and evaluated as an indicator of normal biological processes, pathogenic processes, or pharmacologic responses to a therapeutic intervention. To recap, that's 1) normal processes, 2) pathogenic processes, and 3) pharmacologic responses to therapy. THE END."),
                     patriotism = c(),
                     jingoism = c(),
                     nationalism = c(),
                     fascism = c(),
                     totalitarianism = c(),
                     aristocracy = c(),
                     plutocracy = c(),
                     meritocracy = c(),
                     republic = c(),
                     democracy = c(),
                     constitutional.democracy = c(),
                     rule.of.law = c(),
                     capitalism = c(),
                     laissez.faire = c(),
                     state.capitalism = c(),
                     welfare.capitalism = c(),
                     socialism = c(),
                     communism = c(),
                     conservativism = c("","Reagan vs Trump - definitions of conservatism: ...","Reagan: "),
                     liberalism = c(),
                     classical.liberalism = c(),
                     libertarianism = c(),
                     secularism = c(),
                     pluralism_vs_multiculturalism_vs_cultural.relativism = c(),
                     collectivism_vs_inividualism = c(),
                     monophyly_vs_paraphyly_vs_polyphyly = c(),
                     seal_sealing = c(),
                     monotheism_henotheism_monolatry_polytheism_pantheism_panentheism_trinitarianism_unitarianism = c(),
                     deism_atheism_agnosticism_antitheism = c(),
                     empiricism = c(),
                     rationalism_vs_empiricism = c(),
                     The.Enlightenment = c(),
                     The.Rennaissance = c(),
                     political.correctness = c(),
                     propaganda = c(),
                     obscurantism = c(),
                     elitism = c("(i.e. recall James Anderson's claim that his grandmother's Christian faith towered over his own, and that it would be elitist to think that she didn't have much faith after all because she didn't have the sophisticated scholarly knowledge of the Bible that he now does.")
                     )


geography.lists.variable = list(one = "filler",
                                largest.cities.South.America.ten = c("Sao Paulo - 12 M","Lima - 9 M","Bogota - 8 M","Rio de Janeiro - 6 M","Santiago - 5.5 M","Caracas - 3 M","Buenos Aires - 3 M","Salvador - 3 M","Brasilia - 3 M","Fortaleza - 2.5 M","So, it goes Sao Paulo, Lima, Bogota, Rio, Santiago, Caracas, Buenos Aires, Salvador, Brasilia, Fortaleza; which is (country-wise) ...","Brazil, Peru, Colombia, Brazil, Chile, Venezuela, Argentina, Brazil, Brazil, Brazil. So Brazil has 5 of the top 10, 2 of the top 5, and also the biggest city, and also is the only repeat in the top 10.")
                                )


# Try to do 5 of these a day, and get 1000 medical terms in only 200 days! I'll be done in February of 2019!
# link: https://www.health.harvard.edu/a-through-c
medical.terminology.3 = list(one = "filler",
                             five.alpha.reductase = c("Enzyme that converts testosterone into dihydrotestosterone. This hormone can cause the prostate gland to grow abormally.","E that c t into d. This h c c the p g to g a."),
                             ablation = c("A form of treatment that uses electrical energy, heat, cold, alcohol or other modalities to destroy a small section of damaged tissue.","A f of tx that u e e, h, c, a, or other m to d a s s of d t."),
                             accommodation.eyes = c("The eye's ability to focus on objects close up.","E's a t f o o c u."),
                             ACE.inhibitor = c("Angiotensin-converting enzyme inhibitor, a drug used to treat hypertension and congestive heart failure.","A-c e i, a d u to tx h and CHF."),
                             acetaldehyde = c("The main breakdown product of alcohol metabolism; accumulation of it in the bloodstream may produce flushing (feeling of heat in the face or chest) and vomiting.","T m b p of a m; a of it in the b m p f (a f of h in the f or c) and v."),
                             achlorhydria = c("A condition in which the stomach produces little or no acid. This can affect digestion, cause stomach pain, and keep the body from absorbing vitamins and nutrients.","A c in which the s p l or n a. This c a d, c s p, and k the b f a v and n.")
                             )


# name.ten flashcard deck:
name.ten.variable = list(one = "filler",
                         ten.geniuses = c("(This one is good for showing people like Jordan Peterson how stupid it is to think of Trump as a stable genius). I would think it important to pick ten geniuses who best represent either important contributors to current fields of knowledge, or who have unmistakably exuded unusual thinking abilities in some field.","Albert Einstein","Richard Feynman","Alan Turing","Isaac Newton","James Clerk Maxwell","Emmy Noether","William Shakespeare","Leo Tolstoy","Mozart","Socrates"),
                         ten.overpaid.CEOs = c(),
                         ten.historians = c(),
                         ten.philosophers = c(),
                         ten.innovators = c(),
                         ten.inventers = c(),
                         ten.Muslim.majority.countries = c(),
                         ten.Christian.majority.countries = c(),
                         ten.countries.that.have.remained.economically.stagnant.over.the.past.40.years = c(),
                         ten.top.grossing.tech.companies = c(),
                         ten.top.grossing.pharmaceutical.giants = c(),
                         ten.top.grossing.car.companies = c(),
                         ten.biggest.fossil.fuel.companies = c(),
                         ten.most.socialist.countries = c(),
                         ten.most.capitalist.countries = c(),
                         ten.best.evidences.for.evolution = c(),
                         ten.common.creationist.arguments = c("Think Gish-galloping."),
                         ten.global.warming.evidences = c(),
                         ten.global.warming.denial.arguments = c(),
                         ten.capitalist.victories.by.industry = c("For example, has capitalism been a boon to modern medicine?",""),
                         ten.black.intellectuals.scholars.or.scientists = c(),
                         ten.economically.important.minerals = c(),
                         ten.BofM.evidences = c(),
                         ten.examples.of.confounding.factors = c("Duration of treatment for postmenopausal osteoporosis: People who did worse at the end of the study might not have had a poorer outcome because they didn't take alendronate; rather, it might have been because they didn't start out as well in the first place relative to other patients.")
                         )


pathogens.variable = list(one = "filler",
                          survival.on.surfaces = c("MRSA","MRSA: 7 days to 7 months","VRE","VRE: 5 days to 4 months","Acinetobacter","Acinetobacter: 3 days to 5 months","C difficile","C difficile: 5 months","Norovirus","Norovirus: 12 to 28 days","HIV","HIV: minutes to hours","HBV","HBV: 7 days","HCV","HCV: 16 hours to 4 days")
                          )


# important people (INCLUDES 'names_with_ideas' when I need to group people by idea/movement/school of thought/etc.)
people.important.variable = list(one = "filler",
                                 Otto_Warburg = c("1883-1970; son of physicist Emil Warburg (friend of Einstein's), German Jewish (Emil's parents were Orthodox Jews, though Emil had converted to Protestantism) Nobel (1931, for his 'discovery of the nature and mode of action of the respiratory enzyme') laureate","Warburg Hypothesis: To quote Warburg himself, 'Cancer, above all other diseases, has countless secondary causes. But, even for cancer, there is only one prime cause. Summarized in a few words, the prime cause of cancer is the replacement of the respiration of oxygen in normal body cells by a fermentation of sugar.' Warburg thought that cancer is caused by cells generating ATP mainly by anaerobic breakdown of glucose (i.e. fermentation/anaerobic respiration), in contrast to healthy cells, which undergo oxidative breakdown of pyruvate. Because pyruvate is oxidized in the mitochondria, Warburg thought cancer should be interpreted as a mitochondrial dysfunction."),
                                 Walter_Lippmann = c(""),
                                 Benedict_Anderson = c("Died 2015, was a political scientist and historian, best known for his 1983 book Imagined Communities, which explored the orignis of nationalism. A polyglot with an interest in Southeast Asia, Anderson was a distinguished Professor Emeritus of international studies and government and asian studies at Cornell.","Anderson's work on the Cornell Paper that debunked the official story of Indonesia's 30 September Movement and the subsequent anti-Communist purges of 1965-66 led to his expulsion from that country."),
                                 Mark_Hertsgaard = c(""),
                                 Hector_Gramajo = c("(Died 2004, was a general in the Guatemalan Army who served as Defense Minister from 1987 to 1990, during part of the Guatemalan Civil War (1960-1996).","He ran unsuccessfully in 1995 as the presidential candidate for the coalition between the FUN and PID parties.","Some of Gramajo's friends from his graduating class at Escuela Politecnica would become leaders of Guatemala's marxist-inspired guerrilla insurgency.","During his term as Defense Minister, he played a pivotal role in upholding the elected government from at least two coup attempts led by right-wing hardliners in May 1988 and May 1989.","After finishing his term as Defense Minister, Gramajo entered the graduate program at Harvard's JFK School of Government, where he got a degree in public administration in 1991. In 1991, he also served as commencement speaker at the US Army's School of the Americas, in Fort Benning Georgia.","Also in 1991, while getting up to deliver his commencement address (timing/setting not mentioned by Wikipedia) Gramajo was served in a combined suit by the Center for Constitutional Rights, representing 8 Kanjobal Indians who survived the destruction of their village in the eary 1980s, and the American nun Dianna Ortiz, who had been abducted and tortured in Guatemala in 1989. They accused Gramajo of having command responsibility for the abuses that took place. In 1995 Gramajo was judged civilly liable and ordered to pay a total of $47 million in damages. He did not contest the lawsuit or pay the damages. In 1995, the US revoked his entry visa, barring him from the US.","Gramajo died in 2004 at his ranch in Guatemala. He and his son were attacked by a swarm of africanized bees and died from their many stings."),
                                 names_with_ideas_American.decline = c("Walter Russell Mead's 2018 July 4th article mentions three people with specific articles or books predicting imminent/present American decline. Who are they?","Andrew Hacker - political scientist who published 'The End of the American Era' in 1970.","Charles Kupchan - used the same title for a 2002 book.","Stephen Walt used it again for an influential artile in the National Interest.","Mead also adds Paul Kennedy, Fareed Zakaria and Henry Kissinger as three more 'acute foreign-policy thinkers' who have perceived signs of American decline."),
                                 Walter_Williams = c("b. 1936, an African American economist, commentator, and academic. He is the John M. Olin Distinguished Professor of Economics at George Mason University, as well as a syndicated columnist and author known for his classical liberal and libertarian conservative views.","Wrote Liberty versus the Tyranny of Socialism: Controversial Essays; Race and Economics: How Much Can Be Blamed on Discrimination?"),
                                 Frans_de_Waal = c("Primatologist who wrote a book called Chimpanzee Politics, in which he documents complex rivalries and coalitions that govern chimp communities. De Waal's thesis is that human politics, in all its brutality and ugliness, is 'part of an evolutionary heritage we share with our close relatives'. People like Newt Gingrich agree.")
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


physics.and.technology.variable = list(one = "filler",
                                       ferroelectric.materials = c("Ferroelectric materials have spontaneous electric polarization that can be switched with an external electric field. Applications ...","... Applications: Ferrolectrics thus have applications as capacitors, sensors, and data-storage devices."),
                                       ferroelectric.perovskite.oxides = c("Since the discovery of high-performance BaTiO3 and LiNbO3, ferroelectric perovskite oxides have dominated industrial applications. However ... (cost) ...","... However, these ceramics are expensive to produce as high-quality thin films, and they often contain ...","... they often contain heavy metals like lead 2+, which raises health and environmental concerns. So, what's the alternative? ...","Replacing heavy metals with organic components in the perovskite lattice was considered an alternative, but ...","... but hybrid organic-inorganic perovskites have not shown copmarable ferroelectricity and stability."),
                                       Ye.et.al._organic.ferroelectrics_Science.2018.July.13 = c("Because of the cost, and health and environmental concerns of inorganic ferroelectric materials, organic alternatives have been sought. However, hybrid organic-inorganic perovskites have not shown comparable ferroelectricity and stability. Ye et al. have designed a new class of ferroelectric metal-free perovskites that can compete with ferroelectric oxides (BaTiO3 and LiNbO3). The general chemical formula of perovskites ...","Perovskite general chemical formula: ABX3, where A and B are large and small metal cations, respectively, and X is a bitopic anion, like oxygen 2-, for example. X coordinates to B. About structural topology ...","... The structural topology of the perovskite is important. This topology can be maintained if the A- and X-site ions are replaced with organic molecules within certain size ranges. Finally, recap on the advantages of the new metal-free perovskites designed by Ye et al. ...","These new metal-free perovskite ferroelectrics are simple to make, low cost, and lightweight.")
                                       )


## Political stuff:
# Front side: topic; back sides: some facts with sources
political.facts.variable = list(one = "filler",
                                Israel.2005.disenggmnt.Gaza.part1 = c("aka 'Gaza expulsion', it was the withdrawal of the Israeli army from and dismantling of all 21 Israeli settlements in the Gaza Strip in 2005. Despite the disengagement, the Gaza Strip is sill considered to be under military occuption by Israel by ...","... the UN, international human rights organizations and most legal scholars. - source: Andrew Sanger, 'The Contemporary Law of Blockade and the Gaza Freedom Flotilla', in _Yearbook of International Humanitarian Law 2010_ (2011) ...","... What are the reasons listed by Sanger (2011) for rejecting the Israeli claim of disengagement? ...","Reason 1: Israel's own Disengagement Plan provides for Israel monitoring the external land perimeter of the Gaza Strip. ...","... Reason 2: Israel maintains exclusive authority in Gaza air space and maintains security activity off the Gaza coast. ...","... Reason 3: Israel reserves the right to reenter Gaza at will and maintains a military presence on the Egyptian-Gaza border. ...","Reason 4: Israel continues to control six of Gaza's seven land crossings as well as its maritime borders, and controls the movement of goods and people in and out of the territory. ...","Reason 5: Israeli Defence Force regularly enters parts of the territory, deploys missile attacks, drones and sonic bombs into Gaza. ...","... Reason 6: Israel has declared a no-go buffer zone stretching deep into Gaza: if Gazans enter this zone they can be shot on sight. ...","... Reason 7: Gaza is also dependent upon Israel for water, electricity, telecommunications and other utilities, currency, issuing IDs, and permits to enter and leave the territory. Israel also maintains sole control over the Palestinian Population Registry (through which they classify who is a Palestinian, who is a Gazan or West Banker)."),
                                Israel.2005.disnggmnt.Gaza.part2.aftermath = c("The disengagement/expulsion took place in August and finished in September. Many Israelis resisted and some protesters pelted Israeli military/security with rocks and barricaded themselves. Some told their children to walk with their hands raised in reference to the Holocaust. Right-wingers fiercely opposed Ariel Sharon's action to disengage, including Benjamin Netanyahu, who threated to resign then took it back. But Sharon did win unusual support from the left for the Gaza withdrawal move. So why did Sharon do it? Did he go back on his campaign rhetoric in a sudden inexplicable abandonement of his former professed interests? Apparently it was all about demographics: Israeli democratic majority. Palestinian population growth was outpacing that of Israelis in Gaza.","What happened with the greenhouses? ...","... The following people (7) strongly believed that Israelis left behind all their greenhouses in perfect condition out of an act of selfless charity and good will, only to have them all demolished by rabid Palestinians who wanted to destroy Israeli gifts as much as Israeli people: ...","... Ezra Levant, writing in the Toronto Sun in July 2014. ...","... Charles Krauthammer, writing in the Washington Post in July 2014. ...","... Richard Chesnoff, writing in the Huffington Post in July 2014. ...","J J Goldberg, writing in The Atlantic in July 2014. ...","... Alan Dershowitz, writing in the Jerusalem Post in July 2014. ...","... Lee Smith, writing in Tablet (Israeli magazine), in November 2014. ...","... and Yair Rosenberg, writing in Tablet in July 2014.","What was the reality of the greenhouse debacle? ...","... In reality, the Israeli greenhouse owners were supposed to receive extra compensation for leaving the greenhouses behind. Non-profits like the Economic Cooperation Foundation raised millions in donations. But many Palestinians objected to Israeli settlers being given extra compensation, and the laws of many donor countries and international institutions prevent giving aid to a relatively wealthy country like Israel. About half of the 1,000 acres of greenhouses were destroyed by the owners themselves after giving up waiting for the money they were promised. - source: Steven Erlanger, writing in the New York Times in July 2005 (writing at a time before Palestinians could have destroyed them?? Maybe not?) ...","... The greenhouses that were left were looted by Palestinians. Palestinian Authority security forces attempted to stop them but didn't have the manpower. Then, the Palestinian Economic Development Company invested $20 million and by October the industry was back on its feet. ...","... However, the harvest intended for export via Israel to Europe was lost due to Israeli restrictions on the Karni crossing, which was closed more often than not, leading to losses in excess of $120,000 per day. Economic consultants estimated that the closures cost the whole agricultural sector in Gaza $450,000 a day in lost revenue. 25 truckloads of produce a day through that crossing were needed to render the project viable, but they were lucky to get 3. - sources: ...","... (source for Palestinian looting and Israeli cause of harvest losses: Justin Schwegel, writing in Mondoweiss in August 2014)")
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


# quotes (author and topic on front, quote on back, citation info behind that (could be on same side); could break quote up into multipe parts if long):
quotes.variable = c(one = "filler",
                    Boyd.K.Packer.on.inborn.tendencies.gay = c("Original orally communicated talk as delivered over the pulpit in general conference: 'Some suppose that they were preset and cannot overcome what they feel are inborn tendencies toward the impure and unnatural. Not so! Why would our Heavenly Father do that to anyone? Remember he is our Heavenly Father.' - General Conference October 2010","Subsequent clarification: 'Some suppose that they were preset and cannot overcome what they feel are inborn temptation toward the impure and unnatural. Not so! Remember, God is our Heavenly Father.'","The key difference here is that in the clarification, he didn't imply that God would not have created anyone with same-gender attraction, suggesting that he wanted to take back the claim that God would not create people with inborn same-gender attraction (I know same-gender attraction isn't specifically mentioned in the quote, but that's what no one seems to be debating, as the relevance to same-gender attraction seems rather obvious and not worth debating.)","Some puzzling commentary by Gregory Smith: 'President Packer has an extensive publication record on homosexuality--and, as we will now see, the edited version of his conference talk matches precisely what he has always taught. Far from backpedaling, the edited version is a smooth continuation of principles that he has tuaght for over thirty years.' - G. Smith, 'Shattered Glass: The Traditions of Same-Sex Marriage Advocates Encounter Boyd K Packer'","This is not backpedaling, because it's consistent with his earlier material? What makes something backpedaling is not whether or not the clarification matches an earlier, consistent position, but arises from how the clarification compares to the original message being clarified. If it reduces the boldness or brashness of what was originally said, then that is what makes something backpedaling."),
                    Howard.W.Hunter.on.the.Spirit = c("From Preach My Gospel: 'President Howard W Hunter offered this counsel: 'Let me offer a word of caution. ... I think if we are not careful ..., we may begin to try to counterfeit the true influence of the Spirit of the Lord by unworthy and manipulative means. I get concerned when it appears that strong emotion or free-flowing tears are equated with the presence of the Spirit. Certainly the Spirit of the Lord can bring strong emotional feelings, including tears, but that outward manifestation ought not to be confused with the presence of the Spirit itself.' (The Teachings of Howard W Hunter, 184). The Spirit of the Lord always edifies.'"),
                    David.O.McKay.on.evolution = c("Science, dominated by the spirit of religion, is the key to progress and the hope of the future. For example, evolution's beautiful theory of the creation of the world offers many perplexing problems to the inquiring mind. Inevitably, a teacher who denies divine agency in creation, who insists there is no intelligent purpose in it, will infest the student with the thought that all may be chance. I say, that no youth should be so led without a counterbalancing thought ... In the Brigham Young University and every other Church school the teacher can say God is at the helm. - BYU Address, 10 October 1952")
                    )


## Random facts to remember (with 4 components, usually the last one is a source. So, 
# usually a topic name, two facts about it, and a source):
random.facts.4 = list(one = "filler",
                      humans.v.volcanoes_CO2_sources = c("Humans emit ~24 billion tons of CO2 a year","Volcanoes emit ~200 million tons CO2. That means volcanoes emit .83% what humans emit","This is according to the USGS. Also, the federally funded Carbon Dioxide Information Analysis Center says CO2 has gone up year after year, regardless of whether there have been major volcanic eruptions. - from a Scientific American article from probably 2009"),
                      layy.v.lehi_F.M.Cross_plus.sources = c("The connection of the name Lei (classical Arabic Layy, meaning 'bend, twist') with Lehi is based on a linguistic blunder. ... I would not support layy = lehi anymore than I would confuse Lee with Locke.","lyy and lhy cannot be confused in Semitic. The 'h' is a strong laryngeal spirant in Semitic, like the German ch in Buch or Scottish ch in loch.","I read this F M Cross quote from BAR in Jeffrey Chadwick's 2009 Khirbet Beit Lei and the Book of Mormon: An Archaeologist's Evaluation."),
                      Beit.Lei.archaeology_when.was.it.not = c("Was there any ancient Israelite or Jewish settlement at Khirbet Beit Lei during the time of biblical Samson (ca. 1100 BC; Iron Age I) or during the time of the Book of Mormon prophet Lehi (ca. 600 BC; Iron Age II)?","Answer: No. The archaeological survey of Khirbet Beit Lei carried out by Yehuda Dagan in the 1970s revealed no evidence of any Iron Age I or Iron Age II settlement at the site. This was confirmed by the archaeological excavations of Joseph Patrich and Yoram Tsafrir in the 1980s and by the current excavations being carried out by Oren Gutfeld and Yakov Kalman. Not even a single sherd of Iron Age I or II pottery has been found at Khirbet Beit Lei, nor any architectural component from those periods. There was no city, nor town, nor village, nor private estate at Khirbet Beit Lei during the time of Samson or of Lehi.","So could there have been any kind of settlement at Khirbet Beit Lei around 600 BC that could be called a city of Lehi or Beit Lehi, or was there any community at the site around 600 BC in which Lehi might have prophesied? Answer: No city or town; no community at all. No one lived at the site in 600 BC."),
                      Beit.Lei.archaeology_when.was.it.then = c("When was the village at Khirbet Beit Lei an active community?","Answer: All of the archaeologists (mentioned in another flashcard) affirm that the site was first utilized as an oil-pressing complex during the Hellenistic and early Roman periods (ca. 300 BC to AD 70). Underground oil presses and dovecotes have been excavated at the site by Gutfeld and Kalman. Later, during the Byzantine period (fourth to sixth centuries AD), the site was used as a Christian monastic complex with an elaborately decorated chapel, which was excavated by Patrich and Tsafrir. The site seems to have been abandoned thereafter until it was resettled and built up as an Arab village during the Mameluke period (thirteenth to fifteenth centuries AD). The ruins of houses and public buildings from this era have been cleared by Gutfeld and Kalman; pottery remains from the Hellenistic, Roman, Byzantine, and Mameluke periods have been recovered from all over the site.","Was Khirbet Beit Lei ever a Jewish site? Answer: Possibly. According to Gutfeld, the site seems to have been taken over by Jewish forces during the time of John Hyrcanus, around 100 BC. The site may have been used by Jews of Judea from that time until the Roman war against Judea, which culminated in AD 70."),
                      Otto_Warburg = c("1883-1970; son of physicist Emil Warburg (friend of Einstein's), German Jewish (Emil's parents were Orthodox Jews, though Emil had converted to Protestantism) Nobel (1931, for his 'discovery of the nature and mode of action of the respiratory enzyme') laureate","Warburg Hypothesis: To quote Warburg himself, 'Cancer, above all other diseases, has countless secondary causes. But, even for cancer, there is only one prime cause. Summarized in a few words, the prime cause of cancer is the replacement of the respiration of oxygen in normal body cells by a fermentation of sugar.' Warburg thought that cancer is caused by cells generating ATP mainly by anaerobic breakdown of glucose (i.e. fermentation/anaerobic respiration), in contrast to healthy cells, which undergo oxidative breakdown of pyruvate. Because pyruvate is oxidized in the mitochondria, Warburg thought cancer should be interpreted as a mitochondrial dysfunction."),
                      Ediacaran.biota = c("Ediacaran organisms were first discvoered in 1946 in South Australia's Ediacara Hills.","As of 2018, researchers have identified about 200 different types in Ediacaran-age rocks across the world.","Source: Science - 'These half-billion-year-old creatures were animals—but unlike any known today' 8 August 2018")
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
                             Zotero_ice.cores.Roman.lead.pollution.May18_PNAS = c("The paper, 'Lead pollution recorded in Greenland ice indicates European emissions tracked plagues, wars, and imperial expansion during antiquity', is by Joseph McConnell (leading expert in ice core analysis; at the Desert Research Institute (DRI) in Reno), and Andrew Wilson (classical archaeologist at Oxford; Head of School of Archaeology there), and others","Lead pollution in Arctic ice reflects midlatitude emissions from ancient lead-silver mining and smelting.","Though measurements have been extrapolated to infer performance of ancient economies before, including that of the Roman Empire, past studies were based on sparse sampling and inaccurate dating. This study shows that annual European lead emissions closely varied with historical events, like imperial expansion, wars, and major plagues.","Emissions rose coeval with Phoenician expansion, and reached a maximum under the Roman Empire.","Their results indicate sustained economic growth during the first two centuries of the Roman Empire, terminated by the 2nd century Antonine plague.","Materials/Methods (M/M): They analyzed parts of the NGRIP2 ice core--originally collected in 1998--using the DRI's continuous melter system.","M/M cont.: The DRI ice-core analytical system includes two high-resolution inductively coupled plasma mass spectrometers (HR-ICP-MS) operating in parallel. These MSs, along with a lot of other instruments, are capable of simultaneous measurements of ~30 elements, isotopes, and chemical species.","M/M cont.: All measurements were exactly coregistered in depth.","M/M cont.: The part of the NGRIP2 core they examined was between depths 160 m and 580 m. They got 21,000 low (~9 samples / a --meaning 9 samples per year!) and 48,000 medium (~19 samples / a) slit resolution measurements of lead. This afforded them an effective depth resolution for the HR-ICP-MS measurements of .015 m, equivalent to ~12 samples/a during the Roman era.","M/M cont.: Lead concentration detection limits (defined as 3xs the SD of the blank??) were .01 and .18 pg/g--well below the average NGRIP2 lead concentrations of 1.4 and 3 pg/g during background (1100-1000 BC) and Roman periods. Concentrations ranged from .4 pg/g to more than 20 pg/g during these periods.","M/M cont.: It was *assumed* that total lead comprised three components: 1) crustal lead from windblown dust, 2) volcanic lead largely from quiescent emissions, and 3) pollution lead. Deriving the crustal and voclanic lead levels is an interesting, not too lengthy story. See article for full explanation.","Independent ice core chronology for NGRIP2: Because the NGRIP2 measurements did not extend to the surface, they used the distinct sulfur concentration maximum associated with the well-known Samalas volcanic eruption of 1257 as a signal to synchronize the NGRIP2 with the NEEM_2011_S1 volcanic record. The remaining 162 to 582 m NGRIP2 was independently dated through annual layer counting."),
                             Korean.War_Prager = c("The Korean War, starting just 5 years after WWII ended, was the first major clash between democratic and communist forces.","On June 25th, Soviet-backed communist North Korea crossed the 38th parallel and invaded its US-backed, anti-communist South Korean neighbor. [So they started it.] Within weeks the communists had absorbed nearly the entire country.","The US was confused over whether it should or even could respond. (The US had slashed its military budget at the end of WWII.) The Soviet Union sensed America's lack of resolve and encouraged the North's aggression.","Then Truman rushed troops to Pusan to save the last sliver of unconquered territory.","General Douglas MacArthur pushed further north into North Korean territory in order to unite the entire peninsula [so it was a noble aim].","By the 1980s, South Korea had developed into an economic powerhouse.","Was the Korean war worth the cost (35,000 American lives is the only cost mentioned in the video. The loss of Korean lives is never once brought up.)?","Answer: The natural dividend of saving the South was the evolution of today's democratic and prosperous South Korea. It brought 50 million South Koreans undreamed of freedom and affluence, and it brought the world top flight products. South Korea is a model global citizen and a strong ally of the US, standing in stark contrast to [the hole that is] North Korea. Had it not been for the US, the monstruous Pyongyang regime would now rule all of Korea."," The US's intervention was an effort to save South Koreans. It was also a message that the free world under US leadership would no longer tolerate communist military takeovers of free nations. This deterrent prevented the Soviets from trying similar tactics on Japan, Taiwan, and western Europe.","Finally, the Korean War awakened the US to the dangers of disarmament and isolationism. It led to the bipartisan policy of containment of global communism.","Though not a full victory (didn't unite the peninsula), the Korean War was a victory nonetheless, and not just a military victory, but a moral one as well (it kept half the Korean people free). Want further evidence that it was a moral victory? ...","... evidence: Korea did not have a single material resource that would have benefited America."),
                             Korean.War_basic.facts = c("The Korean War started 25 June 1950, ended 27 July 1953."),
                             Mount.St.Helens.eruption.6 = c("table of contents: 1) VEI 2) amount of material ejected 3) size of crater 4) thermal energy 5) death toll 6) homes destroyed.","In 1980, the Mount St. Helens eruption ranked 5 on the Volcanic Explosivity Index with ...","... more than 1 km^3 of ejected material (estimated to have ejected ~1 cubic mile of material).","This left Mount St. Helens 400 meters shorter, with a crater 1-2 miles wide and 640 meters deep.","It released 24 megatons of thermal energy, which is equivalent to ...","... 1,600 Hiroshimas.","57 people were killed.","200 homes were destroyed."),
                             public.health_health.care.policy_important.facts = c("Inspired by remembering that Will VDB was deriding promises made about the potential health benefits stemming from Human Genome Project. What are the benefits made possible or facilitated by the HGP thus far? How do US health care policies get in the way of (or facilitate) realizing benefits from recent research? What do I need to know to sketch a basic outline of the relative performance/success of the approaches of different national governments to health care (a la T. R. Reid)? What do I need to know about public health in various countries of interest to this last question in order to understand the effects of different health care policies?")
                             )


## A list of old or well-known claims or ideas in science that receive important corroboration some time after the initial introduction of the idea and its general acceptance:
# source; however many key points
science.corroboration.variable = list(one = "filler",
                                      planets.are.born.of.disks.of.gas.and.dust.that.coalesce.around.young.stars = c("Science news - 'In a first, astronomers witness the birth of a planet from gas and dust' (2 July 2018)","In the first convincing observation of its kind, astronomers have directly imaged a newborn planet still forming around its star. The planet, hotter than any in our solar system, supports what astronomers have long believed: that such bodies are born of the disks of gas and dust that coalesce around young stars.","Astronomer Kevin Hang at U of Bern says, 'After decades of speculation, it’s nice to actually see one. It’s very comforting.'",""))


# Scripture references
scriptures.variable = list(one = "filler",
                           drunkenness.Bible.4 = c("Isaiah 5:22","I5:22 - Woe unto them that are mighty to drink wine","1 Corinthians 6:10","1C6:10 - nor ... drunkards shall inherit the kingdom of God","Galatians 5:21","G5:21 - drunkenness ... do such things shall not inherit the kingdom","Ephesians 5:18","E5:18 - be not drunk with wine, wherein is excess")
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
# patient presentations
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
                                      

###################

## microbiology lists:
# pathogens (microbe, disease, pathophysiology / mechanism, dx, tx)

pathogens = list(one = "filler",
                 E.coli.lettuce.2018 = c("The E coli romaine lettuce outbreak in spring and summer of 2018 killed 5 and sickened 210. It's deadliness was due to the fact that it caused hemolytic uremic syndrome.","It's virulence is due to the Shiga toxin, which destroys RBCs. This in turn blocks the kidneys' filtering system, leading to hemolytic uremic syndrome.","dx: ","tx: ")
                 )


length(deck.of.decks)
deck.of.decks[2] # gives the entire deck
deck.of.decks[[2]] # gives the exact same thing, minus the [[1]] business before each card
deck.of.decks[[4]][2] # gives every side of just the second card in the fourth deck
deck.of.decks[[4]][[2]] # gives every side of just the second card in the fourth deck, minus the front side
deck.of.decks[[4]][[2]][1] # gives the SECOND side (the one right after the front) of the second card
deck.of.decks[[4]][[2]][2] # gives the THIRD side of the second card
names(deck.of.decks[[4]][2]) # HOLY FREAK I FIGURED IT OUT: gives the FIRST side of the second card of the fourth deck
length(deck.of.decks[[4]])
names(deck.of.decks[[4]])
length(random.lists.variable[[2]])
length(deck.of.decks[[15]][[2]]) # WORKS! GIVES SAME ANSWER AS LINE ABOVE. DOUBLE BRACKETS FOR BOTH.
deck.of.decks[[15]]["Korean.War_basic.facts"] # No way to use list names to index them. Can only use card names with list numbers as demonstrated here

cardtot = 0
for (i in 1:length(deck.of.decks)){
  Y = length(deck.of.decks[[i]])
  X = sprintf("%i %7.0f",i,length(deck.of.decks[[i]]))
  cardtot = cardtot + Y
  print(X)
}
realcardtotal = cardtot - length(deck.of.decks)
mydf = data.frame(matrix(NA, nrow = realcardtotal, ncol = 2))
mydf[1,] = c(2,2)
mydf[2,] = sample(100,2)
for (i in 3:10){
  mydf[i,] = sample(100,2)
}
mydf[214,] = c(1,2)

##########################################################################
# THIS IS MONEY (FIGURING OUT HOW TO FIND IDENTICAL ROWS FROM TWO DIFFERENT DATA FRAMES):
Amydf = data.frame(matrix(NA, nrow = 10, ncol = 2))
Bmydf = data.frame(matrix(NA, nrow = 10, ncol = 2))
for (i in 1:10){
  Amydf[i,] = sample(10,2)
  Bmydf[i,] = sample(10,2)
}
myanswer = Amydf[10,] %in% Bmydf[3,]
class(myanswer)
sum(myanswer)
# THIS IS THE MONEY PART:
for (i in 1:10){
  for (j in 1:10){
    if (sum(Amydf[i,] %in% Bmydf[j,]) == 2){ # THE SECRET WAS FIGURING OUT TO SET ***THE SUM*** OF THE %in% OPERATION EQUAL TO 2, CUZ THAT'S TWO TRUES!
      print(c(i,j,Amydf[i,] %in% Bmydf[j,])) 
    }else{print("")}
  }
}
##########################################################################

for (i in 1:length(random.lists.variable)){
  X = sprintf("%i %7.0f",i,length(random.lists.variable[[i]]))
  print(X)
}
  
deckofdecks = function(lols){
  cardtotal = 0
  for (i in 1:length(lols)){
    
  }
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  sampdecks = 1:length(lols)
  dnum = 1:length(lols) # Because every deck's first card needs to be in the data frame to begin with
  cnum = 1
  dnum.cnum = data.frame(col1 = c(dnum), col2 = c(cnum))
  while (continue != ""){
    dnum = sample(sampdecks, 1)
    cnum = sample(2:length(lols[[dnum]])) # starting at 2 you avoid the filler card altogether
    print(names(lols[[dnum]][cnum]))
    for (i in 1:length(lols[[dnum]][[cnum]])){
      show = readline(prompt = "Hit 'l' to show next side: ")
      if (show == "l"){
        print(lols[[dnum]][[cnum]][i])
      } else{print("you suck.")}
    }
    
    dnum.cnum = data.frame(col1 = c(dnum), col2 = c(cnum))
  }
}

sampdecks = 1:length(deck.of.decks)
dnum = 1:length(deck.of.decks) # Because every deck's first card needs to be in the data frame to begin with
cnum = 1
dnum.cnum = data.frame(col1 = c(dnum), col2 = c(cnum)) # IT WORKS!


df1 = data.frame(col1 = sample(10,5), col2 = sample(10,5))
df2 = data.frame(col1 = 1, col2 = 3)
library(plyr)
match_df(df1,df2)
df3 = data.frame(col1 = 1, col2 = 2)
answer = match_df(df1,df3)
class(answer)
answer$col1 = as.numeric(answer$col1); answer$col1 > 0
answer$col2
identical(integer(0),answer$col1)
df4 = data.frame(col1 = 8, col2 = 7)
answer2 = match_df(df1,df4)
answer2$col1 = as.numeric(answer2$col1); answer2$col1 > 0
answer2$col2

### flashcards for many-sided cards (best for lists of variable lengths):
deckofdecks = function(yourlist){
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









