########################################################### 
# 加分作業：
# 當要完成的目標變複雜後，學習如何將複雜的問題拆解成一個一個小問題來解決
# 練習 R function 的使用

# OOXX 遊戲練習
# 1. 設計一個兩人的OOXX遊戲。
# 2. 遊戲玩家分為A、B。A 先手，使用的符號為'O'; B 後手，使用的符號為'X'
# 3. 遊戲一開始，請輸出以下遊戲提示，並且停留等待玩家A輸入
record <- c(0,0,0,0,0,0,0,0,0)
print("Round 0")
print("Now is player A's term!")
print("Player A input(1~9) :")
game.A <- 0
game.A <- scan("")


#    Round 0
#    Now is player A's term!
#    Player A input(1~9) : 

# 4. 玩家們可以輸入的數字範圍為 1~9，依序對應九宮格的九格位置。
#    如果輸入錯誤，請抓錯！輸出以下遊戲提示。

#    Invalid input! Please re-enter! 
#    Round 0
#    Now is player A's term!
#    Player A input(1~9) : 


while((game.A>10) | (game.A <1) ){
  print("Invalid input! Please re-enter! ")
  print("Round 0")
  print("Now is player A's term!")
  print("Player A input(1~9) :")
  game.A <- scan("")
  
}
# 5. 待玩家正確輸入完後，請輸出以下遊戲提示(當時的遊戲圖形狀況)，並且等待切換到另外一位玩家等待輸入。
#    * 提醒，記得增加'Round'次數，以及切換使用者

#    O| | 
#    _____
#     | | 
#    _____
#     | | 
#    **************
#    Round 1
#    Now is player B's term!
#    Player B input(1~9) : 
print("开始\n")
record[game.A] <- 1
for(i in 1:game.A ){
  if(i%%3 != 0){
  cat(" |")
    write.table(' |',"D:\\OOXX.txt",eol = "",row.names = F,col.names = F)
  }else{
    cat("\n","_____","\n")
    write.table("\n_____\n","OOXX.txt",eol = "",row.names = F,col.names = F)
  }
}
if(game.A%%3 == 0){
  cat("O\n","_____","\n")
  write.table("\n_____\n","OOXX.txt",eol = "",row.names = F,col.names = F)
}else{
  cat("O|")
  write.table("O|","OOXX.txt",eol = "",row.names = F,col.names = F)
  
}
for(i in game.A+1:9){
  if(i%%3 != 0){
    cat(" |")
    write.table(" |","OOXX.txt",eol = "",row.names = F,col.names = F)
    
    
  }else{
    cat("\n","_____","\n")
    write.table("\n_____\n","OOXX.txt",eol = "",row.names = F,col.names = F)
  }
}
print("Round 1")
print("Now is player B's term!")
game.B <- 0
game.B <- scan("")

while((game.B>10) | (game.B <1) ){
  print("Invalid input! Please re-enter! ")
  print("Round 1")
  print("Now is player B's term!")
  print("Player B input(1~9) :")
  game.B <- scan("")
  
}
# 6. 當玩家輸入的位置之前已經有'O'或'X'時，請輸出以下遊戲提示。

#    This position is already occupied!
#    Round 1
#    Now is player B's term!
#    Player B input(1~9) : 

while(record[game.B] != 0){
  print("This position is already occupied!")
  print("Round 1")
  print("Now is player B's term!")
  print("Player B input(1~9) :")
  game.B <- scan("")
}
# 7. 當使用者輸入'exit'時，結束遊戲並印出以下遊戲提示 

#    Bye-Bye!!

if(game.A == 'exit')
{
  print("Bye-Bye!!")
  break;
}

# 8. 判斷遊戲結束！當三個直排、橫排、或者斜排時，請輸出以下遊戲提示(當時的遊戲圖形狀況)，並且輸出勝利的玩家。

#    O|X|O
#    _____
#    X|O|X
#    _____
#    O| | 
#    **************
#    Player A wins!!! 
#
if()

# 9. 當空格皆被填滿且無玩家獲勝時，請輸出以下遊戲提示(當時的遊戲圖形狀況)以及和局遊戲提示。

#   O|O|X
#   _____
#   X|X|O
#   _____
#   O|X|O
#   **************
#   End in a draw!!! 