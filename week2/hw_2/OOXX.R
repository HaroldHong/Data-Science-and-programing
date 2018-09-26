
player <- 0
winner <- 0
choice <- 0
row <- 0
col <- 0
line <- 0
i <- 0
board<-array('\u0020',dim=c(3,3))
board

while(i<9 & winner == 0)
{
  cat(board[0,0],"|",board[0,1],"|","|",board[0,2],'\n')
  cat("-----------\n")
  cat(board[1,0],"|",board[1,1],"|","|",board[1,2],'\n')
  cat("-----------\n")
  cat(board[2,0],"|",board[2,1],"|","|",board[2,2],'\n')
  cat("-----------\n")
  player = i %% 2+1 
  
  cat("Now is player", player,"'s term!")
  choice <- readline()
  choice <- as.numeric(choice)
  as.numeric(choice)
  row <- choice / 3
  col <- choice %% 3
  while(choice < 0 | choice > 9 | board[row,col] =='X'|board[row,col] =='O')
  {
    cat("Now is player", player,"'s term!")
    choice <- readline()
    mode(choice)
    choice <- as.numeric(choice)
    row <- choice/3
    col <- choice %% 3
  }
  if(player == 1)
  {
    board[row,col] <- 'X'
  }else{
    board[row,col] <- 'O'
  }
  if((board[0,0] == board[1,1] & board[0,0] == board[2,2])|(board[0,2] == board[1,1] & board[0,2] == board[2,0]))
  {
    winner = player
  }else{
    while(line < 3)
    {
      if((board[0,line] == board[1,line] & board[0,line] == board[2,line])|(board[line,0] == board[line,1] & board[line,0]== board[line,2])){
        winner = player
      }
      line = line + 1
    }
    
  }
  i = i + 1
}

cat(board[0,0],"|",board[0,1],"|","|",board[0,2],'\n')
cat("-----------\n")
cat(board[1,0],"|",board[1,1],"|","|",board[1,2],'\n')
cat("-----------\n")
cat(board[2,0],"|",board[2,1],"|","|",board[2,2],'\n')
cat("-----------\n")
if(winner == 0){
  print("平局")
}else{
  cat("player",winner, "win!")
}