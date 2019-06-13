# Advanced R


cat("Guess a number between 0 and 100.\n")
cat("Guess a number between 0 and 100.") # What's the difference?

num = round(runif(1)*100, digits = 0)
cat("Congratulations!",num,"is right.\n")
sprintf("Congratulations! %s is right.",num) # Ohhh .... sprintf doesn't get rid 
# of the quotes, or the \n

# From the internet:
## https://medium.freecodecamp.org/best-programming-languages-to-learn-in-2018-ultimate-guide-bfc93e615b35
readinteger <- function() {
  n <- readline(prompt="Enter an integer: ")
  if (!grepl("^[0-9]+$",n)) {
    return(readinteger())
  }
  return(as.integer(n))
}

num <- round(runif(1) * 100, digits = 0)
guess <- -1

cat("Guess a number between 0 and 100.\n")

while(guess != num) {
  guess <- readinteger()
  if (guess == num) {
    cat("Congratulations,", num, "is right.\n")
  } else if (guess < num) {
    cat("It's bigger!\n")
  } else if(guess > num) {
    cat("It's smaller!\n")
  }
}




###############################################################################

# Samuel Ackerman's website at sites.temple.edu:

# Checkers: This is a program to play an automatic, randmized, semi-intelligent 
# game of checkers.
# Moves are chosen by a simple preference algorithm to analyze the available moves 
# in terms of the
# immediate result (e.g. a capture or becoming a king) and do not take into account 
# the possible 
# reactions to the move by the opposing player.  For instance, move choice does not 
# incorporate the
# ability to avoid a future capture or other global position strategy.
# To play the game: in R, run the program checkers.txt via the source() command, 
# after having saved
# the program.  Then run the checkers() function in R. The successive moves will be 
# printed to the
# screen.
# Copyright 2012 Samuel Ackerman

# rm(list=ls())

choose.random<-function(vec) {
  #function to sample from a vector, problem is applying sample func to unit vector, will sample from 1:n
  if (length (vec) >1) {  chosen<-sample( vec, 1) }
  else { chosen <-vec  }
  chosen
}


is.king<-function(players=xs) {
  v<-c()	
  str<-strsplit( as.vector(players), split="")
  for (i in 1:length(str)) {
    v[i]<- (str[[i]][2]=="K")
  }
  v
}


direction<-function(team, dir, capture, row) {  
  dir<-as.vector(dir)
  step<-rep(0, length(dir))
  if (row==T) {
    step[ dir %in% c("UR","UL")]  <-  1*(1+ (capture==T)) 
    step[ dir %in% c("LR","LL")]  <- -1*(1+ (capture==T)) 
  }	else if (row==F) {
    step[ dir %in% c("UL","LL")]  <-  1*(1+ (capture==T)) 
    step[ dir %in% c("UR","LR")]  <- -1*(1+ (capture==T)) 
  }
  step[ team=="O" ] <- step[ team=="O"]*-1
  step
}	   


make.king<-function(player="X1") {
  str<-unlist(strsplit(player, split=""))
  player<-paste(c(str[1],"K", str[2:length(str)]), collapse="")
  player
}  


rot90<-function(mat=as.matrix(1))  {
  n<-dim(mat)[1]
  stopifnot(n==dim(mat)[2])
  t(mat[n:1,])
}


inbounds<-function(v) {
  v<-as.vector(v)
  v >=1 & v<=8 & ! is.na(v)
}	


team.vector<-function(vec) {	 
  substr(vec,1,1)
}  


#the middle point in between two coordinates on a diagonal
middlepoints<-function(row1,col1,row2,col2) {
  rows<-cbind(row1,row2)
  cols<-cbind(col1,col2)
  n<-min(nrow(rows), nrow(cols))
  rows<-rows[1:n, ]
  cols<-cols[1:n, ]
  mids<-as.data.frame(cbind(apply(rows,1,mean), apply(cols,1,mean)))
  colnames(mids)<-c("row","col")
  mids
}


matrix.elements<-function(mat,row, col) {
  n<-min(length(col),length(row))
  col<-col[1:n]
  row<-row[1:n]
  elements<-c()
  if (n>0) {
    
    for (j in 1:n) {
      
      if(is.na(row[j]) | is.na(col[j])) {  elements[j]<-NA 
      } else { elements[j]<-mat[ row[j], col[j]] }
    }
  }
  elements
}


onestepmoves<-function(b=board, players=xs) {
  name<-c("player","row","col","dir","torow","tocol")
  moves<-data.frame(t(rep(NA, length(name))))
  colnames(moves) <-name
  kings<-players[ is.king(players) ]
  not.king<-players[ is.king(players)==F ]	
  if ( length(kings) >0 ) {
    dir<-c("UL","UR","LL","LR")
    kingmat<-expand.grid(kings, 0, 0, dir, 0, 0)
    colnames(kingmat)<-name
    moves<-rbind(moves, kingmat)
  }
  
  if (length(not.king) > 0) {
    dir<-c("UL","UR")
    regmat<-expand.grid(not.king, 0, 0, dir, 0, 0)
    colnames(regmat)<-name
    moves<-rbind(moves, regmat)
  }	
  moves<-na.omit(moves)
  for (i in players) {
    #all 1 step moves
    loc<-which(b==i, arr.ind=T)
    moves$row[ moves$player== i ]<-loc[1]
    moves$col[ moves$player== i ]<-loc[2]
  }
  moves$torow <- moves$row + direction(team=team.vector( moves$player), dir=moves$dir, capture=F, row=T)
  moves$tocol <- moves$col + direction(team=team.vector( moves$player), dir=moves$dir, capture=F, row=F)
  #remove squares that are out of bounds
  moves<-moves[ inbounds(moves$torow) & inbounds(moves$tocol) ,]
  moves$king<-0
  moves$king[ team.vector(moves$player)=="X" & !is.king(moves$player) & moves$torow==8 ] <-1
  moves$king[ team.vector(moves$player)=="O" & !is.king(moves$player) & moves$torow==1 ] <-1	
  #remove squares where the square is not empty	
  moves<-moves[ matrix.elements(mat=b, row=moves$torow, col=moves$tocol ) ==".", ]
  moves   
}


capturemoves<-function(b=board, players=xs) {
  captures<-as.data.frame(players) 
  captures$torow0<-NA
  captures$tocol0<-NA
  captures$numcaptured<-0
  captures$king<-0
  colnames(captures)<-c("player","torow0","tocol0","numcaptured", "king")
  for (p in captures$player) {
    loc<-which(b==p, arr.ind=T)
    captures$torow0[ captures$player==p ] <-loc[1]
    captures$tocol0[ captures$player==p ] <-loc[2]
  }
  for (t in 1:8) {
    captures<-captures[rep(1:nrow(captures), 2+(2*is.king(captures$player) )) ,]
    dir<-rep("",nrow(captures))
    
    #the king determination needs to be set at the beginning; once a player becomes a king, his turn
    #ends before he has the chance to go backwards, which was impossible before he became a king
    king.vec<-is.king( captures$player)
    
    dir[ king.vec==FALSE ]<-c("UL","UR")
    dir[ king.vec==TRUE ]<-c("UL","UR","LL","LR")
    
    moves<-as.data.frame(dir) 
    moves[, paste(c("torow","tocol"), t, sep="")]<-NA
    moves[, paste("skipped",t, sep="")]<-""
    
    colnames(moves)<-paste(c("dir","torow","tocol","skipped"),t, sep="")
    rownames(moves)<-rownames(captures)	  
  
    moves[ , paste("torow",t,sep="")] <- captures[, paste("torow",t-1,sep="")] +
      direction(team=team.vector(captures$player), dir=as.vector(moves[, paste("dir",t,sep="")]), capture=T, row=T)
    moves[ , paste("tocol",t,sep="")] <- captures[, paste("tocol",t-1,sep="")] +
      direction(team=team.vector(captures$player), dir=as.vector(moves[, paste("dir",t,sep="")]), capture=T, row=F)
    
    #merge the two and then start testing
    captures<-cbind(captures, moves)
    
    outofbounds<-((inbounds(as.vector( captures[, paste("tocol",t,sep="")]))==FALSE) | 
                    (inbounds(as.vector( captures[, paste("torow",t,sep="")]))==FALSE))
    
    captures[ outofbounds==TRUE, paste(c("torow","tocol"), t, sep="")]<-NA
    
    #eliminate rows with other inconsistencies	       
    
    #now test remaining rows for validity         	       
    if (nrow(moves[ outofbounds==FALSE, ]) > 0) {
      skipped.squares<-middlepoints(row1=captures[ , paste("torow",t-1,sep="")], col1=captures[ , paste("tocol",t- 
                                                                                                          1,sep="")],        								    row2=captures[ , paste("torow",t,sep="")],  
                                    col2=captures[ , paste("tocol",t,sep="")])
      
      skipped.contents<-matrix.elements(mat=b, row=skipped.squares$row, col=skipped.squares$col)
      
      captures[ , paste("skipped",t,sep="") ]<- skipped.contents
      
      destination.contents<-matrix.elements(mat=b, row=captures[ , paste("torow",t,sep="") ], 
                                            col=captures[ , paste("tocol",t,sep="")])	
      #can't capture same one more than once (important for kings)
      captures[ outofbounds==FALSE & ((skipped.contents==".") | (destination.contents !=".") |
                                        ((team.vector( destination.contents) == team.vector(captures$player)) & destination.contents !=".") |
                                        ((team.vector( skipped.contents) == team.vector(captures$player)) & skipped.contents !=".")),  
                paste(c("torow","tocol"), t, sep="")]<- NA
      nonunique<-rep(F, nrow(captures))	
      if (t>1) { 
        for (j in 1:(t-1)) {
          
          nonunique[ (captures[, paste("skipped",j,sep="")] == captures[, paste("skipped",t,sep="")])
                     & (! is.na(captures[, paste("skipped",t,sep="")]))]   <-T
        }
      }
      
      captures[nonunique==T, paste(c("torow","tocol"),t,sep="")]<-NA
    }			
    captures$numcaptured<-captures$numcaptured + as.numeric(! is.na(captures[, paste("torow",t,sep="")])) 
    captures$king[ ! is.king(captures$player) & (team.vector(captures$player)=="X") & (captures[, paste 
                                                                                                ("torow",t,sep="")]==8) ]<-1
    captures$king[ ! is.king(captures$player) & (team.vector(captures$player)=="O") & (captures[, paste 
                                                                                                ("torow",t,sep="")]==1) ]<-1
    #delete any rows with less successful outcomes  
    
    if ( nrow ( captures[ ! is.na( captures[, paste("torow",t,sep="") ]), ]) >0 ) {
      
      #if any moves for t result in kings, remove any rows that end earlier
      
      if (sum ( captures$king [ ! is.na( captures[, paste("torow",t,sep="") ])]) >0) {
        
        captures<- captures[ ! is.na( captures[, paste("torow",t,sep="")]), ]
        
      }
      
      else {
        #if no kings here, delete any previous rows that end earlier with no king
        
        captures<-captures[ -which( is.na(captures[, paste("torow",t,sep="")]) &  
                                      captures$king==0) , ]
      }
    }
    if (nrow(captures)==0  ) {
      
      break
      
    }
    
    else if (max(captures$numcaptured)==0) {
      
      captures<-captures[ captures$numcaptured >0, ]
      break
      
    }#if no valid moves for this iteration
     else if (sum( ! is.na( captures[, paste("torow",t, sep="")])) ==0) {
      
      captures<-captures[ , - which(colnames(captures) %in% paste(c("dir","torow","tocol","skipped"),t ,  
                                                                  sep="")) ]
      break
    }     
  }#end of 1:t
  captures<-unique(captures)
}       	   


choose.action<-function(cap.matrix=captures, move.matrix=onestep, tokenlist=tokens) {
  #strategy: favor maximal captures with king, then maximal captures without king ,..., move.matrix to make king, then  
  move.matrix
  can.onestep<-(nrow(move.matrix)>0)
  can.capture<-(nrow(cap.matrix)>0)    
  #chosen move
  chosen.move<-list()
  if (can.onestep) {
    bestmove.forkings<-( move.matrix$torow >=2 & move.matrix$torow <=7 & move.matrix$tocol>=2 & move.matrix$tocol<=7)
    #if you have only kings, try to move them towards center so they don't get stuck
    if (sum(bestmove.forkings)>0 & sum(is.king(move.matrix$player)) ==nrow(move.matrix)) { 
      if (sum(bestmove.forkings)==1) { chosen.onestep<- which( bestmove.forkings==T)  }
      else { chosen.onestep <- sample (which(bestmove.forkings==T), 1)  } 				
    } 
    #if you can make a king, do it
    else if (sum(move.matrix$king) >1 ) {   chosen.onestep<- sample( which(move.matrix$king ==1), 1)  }
    else if (sum(move.matrix$king) ==1) { chosen.onestep<-which(move.matrix$king==1)  }	
    #otherwise just choose
    else { 
      chosen.onestep<-sample(1:nrow(move.matrix), 1)
    }
    chosen.move$action<-"onestep"
    chosen.move$move<- chosen.onestep
  }  
  if (can.capture) {
    #test if can win the game in this move (problem because sometimes can win game but choose to make king instead)
    maxcap<-max(cap.matrix$numcaptured)
    curr.team<-team.vector(cap.matrix$player[ 1 ])
    opposite.team<-as.vector(unlist(tokenlist[ names(tokenlist) != curr.team ]))
    can.win<- (maxcap==length(opposite.team))
    #print(cap.matrix)
    if (sum(as.numeric(cap.matrix$king))==0) { maxcap.withking<-0 }
    else {maxcap.withking<- max( cap.matrix$numcaptured[ cap.matrix$king==1 ]) }
    have.maxcap<-(cap.matrix$numcaptured == maxcap)
    #if there is a max capture move that also has a king 
    #first check if can win the game 
    if (can.win) {
      selected<-which( have.maxcap==T) 
      chosen.capture<-choose.random( selected )
      chosen.move$action<-"capture"
      chosen.move$move<-chosen.capture
    }
    else if ( maxcap.withking==maxcap) { 
      selected<-which( have.maxcap==T & cap.matrix$king==1) 
      chosen.capture<-choose.random(selected)       	
    }
    #if there is a king move with 2 or more cap.matrix, do that, otherwise do maxcap
    else if ( maxcap.withking >=2 ) { 
      
      selected<-which(cap.matrix$king==1 & cap.matrix$numcaptured==maxcap.withking)
      chosen.capture<-choose.random(selected)
    }
    #otherwise take one of the max cap.matrix
    else { chosen.capture<-sample ( (1:nrow(cap.matrix))[ have.maxcap ], 1)  }
    chosen.move$action<-"capture"
    chosen.move$move<-chosen.capture
  }
  #if can do both then choose
  if (can.capture & can.onestep) {
    if (can.win==F) {	        
      #if can king with one step
      if (sum(move.matrix$king) >0) {
        #if can also capture with king:
        if (maxcap.withking >0)  {
          chosen.move$action<-"capture"
          chosen.move$move<-chosen.capture
        }#if no cap.matrix with king     
          else {
            if (maxcap >=2) {
            chosen.move$action<-"capture"
            chosen.move$move<-chosen.capture  
            } else {
            chosen.move$action<-"onestep"
            chosen.move$move<- chosen.onestep
              }
          }
      }
      #if can't king with move.matrix
      else {
        chosen.move$action<-"capture"
        chosen.move$move<-chosen.capture  	               
      }
    }	  
  }#end of resoving
  chosen.move
}


execute.onestep<-function(b=board, move=onestep[move.decision$move, ], tokenlist=tokens) {
  player<-as.character(move$player)
  #replace current spot with dot
  b[ move$row, move$col ] <-"."
  
  #if becomes a king, update info
  if (move$king & ! is.king(player)) {
    playerlist<- tokenlist[[ team.vector( player ) ]]
    newking<-as.character(make.king(player))
    playerlist[ which(playerlist==player) ]<-newking
    tokenlist[[ team.vector( newking ) ]]<-playerlist
    player<-newking
  } 
  b[ move$torow, move$tocol ] <- player
  Sys.sleep(1)
  write.table(format(b,justify="centre"),quote=F,col.names=F, row.names=F, file="")
  flush.console()
  result<-list()
  result$board<-b
  result$tokens<-tokenlist
  result
}	


execute.capture<-function(b=board, move=captures[move.decision$move, ], tokenlist=tokens) {
  player<-as.character(move$player)
  opposite.team<-team.vector(  move$skipped1 )
  opposing.players<-tokenlist[[ opposite.team ]]
  numcaptured<-move$numcaptured
  for (i in 1:min(8, move$numcaptured) ) {
    skipped<-move[, paste("skipped",i,sep="")]
    loc<-which(b==skipped, arr.ind=T)
    b[ loc[1], loc[2] ]<-"."
    opposing.players<-opposing.players[ opposing.players != skipped ]
    
    #if becomes a king
    if (i==move$numcaptured & move$king==1 & ! is.king(player)) {
      player<-as.character(make.king(player))
      playerlist<- tokenlist[[ team.vector( player ) ]]
      playerlist[ which(playerlist==move$player) ]<-player
      tokenlist[[ team.vector( player ) ]] <-playerlist
      
    }
    #place moved player on the board and move from original spot 
    b[ move[, paste("torow",i-1,sep="")],  move[, paste("tocol",i-1,sep="")]] <-"."  
    b[ move[, paste("torow",i,sep="")],  move[, paste("tocol",i,sep="")]] <-player
    Sys.sleep(1)  
    if (i>1) { cat("\n") }
    write.table(format(b,justify="centre"), quote=F, row.names=F, col.names=F, file="")
    flush.console()
  }
  tokenlist[[ opposite.team ]] <-opposing.players
  result<-list()
  result$board<-b
  result$tokens<-tokenlist
  result
}	


checkers<- function() {
  n<-8
  board<-matrix(".", ncol=n, nrow=n)
  xs<-paste("X",1:12, sep="")
  os<-paste("O",1:12, sep="")
  
  #initialize the board
  board[1, seq(2,8, by=2)]<-xs[1:4]
  board[2, seq(1,7, by=2)]<-xs[5:8]
  board[3, seq(2,8, by=2)]<-xs[9:12]
  board[6, seq(1,7, by=2)]<-os[1:4]
  board[7, seq(2,8, by=2)]<-os[5:8]
  board[8, seq(1,7, by=2)]<-os[9:12]
  
  #list of avatokens
  tokens<-list()
  tokens[["X"]]<-xs
  tokens[["O"]]<-os
  order<-rep(c("X","O"), 10000)
  write.table(format(board, justify="centre"), file="", quote=F, col.names=F, row.names=F)
  for (p in order) {
    cat("\n",paste("Player:",p),"\n")
    onestep<-onestepmoves(b=board, players=tokens[[p]])
    captures<-capturemoves(b=board, players=tokens[[p]])   
    move.decision<-choose.action(cap.matrix=captures, move.matrix=onestep, tokenlist=tokens)
    if (nrow(captures) >0 | nrow(onestep) >0) {
      if (move.decision$action=="capture") {
        move.result<-execute.capture(b=board, move=as.data.frame(captures[ move.decision$move, ]), tokenlist=tokens )
        board<-move.result$board
        tokens<-move.result$tokens
      } else if (move.decision$action=="onestep") {
        move.result<-execute.onestep(b=board, move=as.data.frame(onestep[ move.decision$move, ]), tokenlist=tokens )
        board<-move.result$board
        tokens<-move.result$tokens
      }
    } else if (nrow(onestep)==0 & nrow(captures)==0)   {
      print(paste("Game is over, ",p, " cannot move.  ",p," loses the game.", sep=""))
      break
    }  
    if (length(tokens[["O"]])==0 | length(tokens[["X"]])==0)  {
      print(paste(p, "wins the game!"))
      break
    }
  }
  #board
}


















