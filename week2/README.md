## NTU R_资料科学程序设计
2018年9月台湾科技大学大学交流期间于台湾大学修习
合肥工业大学 计算机科学与技术 2016级 洪世豪
https://haroldhong.github.io/Data-Science-and-programing/week2/hw_2/crawler.html
### 分 课前 课堂 课后三部分解说
- 课前
助教有提前把资料放在NTU COOL上，老师的课件包含有视频和相关网站，因为对爬虫本身很感兴趣，而且英文功底不佳，故提前一天过了一遍老师的课件，对爬虫和课堂任务有具体的了解。但是对于阅读GitHub上的code以及网站的code还是有困难，及时查阅资料也是半懂不懂，没有吃透。什么时候用css什么时候用json，什么是xml等等。总之带着问题进入课堂
- 课堂
在课堂上学习了详细的操作步骤，怎么从找class，怎么找url，怎么从Chrome的检查元素里面看是要用json还是css还是其他。学到了在R语言中library语句执行前还要先install，课前以为markdown要另外下载编辑器，原来就在RStudio里面。跟着课件里的范例做了个小的爬虫，但是后来因为readme的格式问题(不是utf-8编码)，导致一直没法生成GitHub的URL，所以没能展示，有点遗憾。
- 课后
因对杭州这座城市很是喜爱，所以很好奇它房租的数据(https://hz.lianjia.com/zufang/binjiang/pg1/ )。  在其他博客上学到Chrome有个SelectorGadget的插件，用来挑出class真的非常方便！我用的是rvest，一切按部就班地写好后我先是生成.csv文件，发现很多栏有<U00A0><U00A0>的字样。明显是ASCII码但是我没有多想，上来就是字符替换，删除等，但都失败了，查资料才知道是html里的空格(不换行空格)，果然在后来做出RMarkdown和html文件后，在网站上打开<U00A0>就消失了。但我还是想请教下，怎样去掉csv文件里的<U00A0>。
