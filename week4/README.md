﻿## NTU R_资料科学程序设计
2018年9月台湾科技大学大学交流期间于台湾大学修习
合肥工业大学 计算机科学与技术 2016级 洪世豪

textMining作业网址：https://haroldhong.github.io/Data-Science-and-programing/week4/hw_4/datamining.html 
leafletMap作业网址：https://haroldhong.github.io/Data-Science-and-programing/week4/hw_4/leafletMap.html


- 课堂
在课上分组老师安排我们对安宁病房的数据处理分析，首先我们先查Google怎么用R语言读取doc文档。发现是件比较麻烦的事情，
遂转变思路，查找格式转换的应用程式，5分钟搞定。然后我们按照教程上的返利照本宣科地对病历处理，发现很多医用剂量单位
和医学化学名词被读取，生成的Wordcloud看不出任何含义。
- 课后
医学的英文词典包很多不支持R语言，在痛苦中放弃。又想到对资料分析，数据挖掘类的工作感兴趣，想要了解业界对这个岗位的
要求，转而搜寻相关文本资料。很幸运找到了包含450个岗位具体信息的Excel表格（包含薪资，要求，工作描述，共计20多万字）
在处理原始资料时需要用到R语言的字符串函数，就搜索了有关正则表达式的知识https://blog.csdn.net/ISMedal/article/details/79450820
以及R语言中比较特殊的转义字符\\1 https://zhidao.baidu.com/question/240565568.html ，jiebar里有很多很好用的函数，网上
有很多介绍的文章，其中我推荐https://blog.csdn.net/qq_41518277/article/details/80198407 。
- 附加题
找了leafletMap的教程，但是简单做了个实例却无法加载地图。看了半个多小时，才发现是因为数据量太大，几十万条数据脚本跑不动导致的。
这里推荐两个教程网站https://blog.csdn.net/allenlu2008/article/details/52830635 和 https://blog.csdn.net/sinat_26917383/article/details/57083985
