
 first <- sample(1:9,1)
 firstSet <- setdiff({1:9},{first})

 second <- sample(firstSet,1)
 secondSet <- setdiff(firstSet,{second})

 third <- sample(secondSet,1)
 thirdSet <- setdiff(secondSet,{third})

 forth <- sample(thirdSet,1)
 realNum <- first*1000+second*100+third*10+forth
 realNum
 mode(realNum)
 guessNum <- readline()
 mode(guessNum)
 print("猜一个4位数，-9999为退出键")
 while(guessNum != realNum & guessNum != -9999){
   while(guessNum < 1000 | guessNum > 9999 )
   {
     print(mode(guessNum))
     print("猜的数字不符合要求，不是四位数，或者不是正整数！")
     print("请再猜一个数：")
     guessNum <- readline()
   }
   if(guessNum < realNum){
     print("猜小了")
   }else if(guessNum > realNum){
     print("猜大了")
   }
   print("再猜一个：")
   guessNum <- readline()
 }
 if(guessNum == realNum)
 print("恭喜你猜中了！")