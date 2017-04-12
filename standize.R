standardize<-function(data1)
{
  data1<-data1[,c(7,8,12,13,15,16,18,20,21)]
  #names(data1)
  library("reshape")
  data1<-rename(data1,c("性别"="sex","出生日期" ="birthday","病人属于"="domain",
                        "现住地址国标"="code","职业"="worktypename","病例分类"="casetype",
                        "发病日期"="fbdate","死亡日期"="swdate","疾病名称"="disease"))
  
  attach(data1)
  #year1、month1--出生日期；year2、month2--发病日期
  data1$year1<-as.numeric(substr(birthday,1,4));data1$month1<-as.numeric(substr(birthday,6,7))
  data1$year2<-as.numeric(substr(fbdate,1,4));data1$month2<-as.numeric(substr(fbdate,6,7))
  data1$age<-(data1$year2-data1$year1)+(data1$month2-data1$month1)*1/12
  data1$disease<-gsub(" ","",data1$disease)
  data1$code1<-as.numeric(substr(code,1,2))
  
  #标明疾病分类
  data1$abc[data1$disease %in% c('鼠疫','霍乱')]<-"A"
  data1$abc[data1$disease %in% c('甲肝','乙肝','丙肝','戊肝','肝炎（未分型）','细菌性痢疾','阿米巴性痢疾','伤寒','副伤寒','艾滋病','淋病','Ⅰ期梅毒',
                                 'Ⅱ期梅毒','III期梅毒','胎传梅毒','隐性梅毒','脊灰','麻疹','百日咳','白喉','流脑','猩红热','出血热','狂犬病',
                                 '钩体病','肺炭疽','皮肤炭疽','炭疽（未分型）','乙脑','间日疟','恶性疟','疟疾（未分型）','登革热','新生儿破伤风',
                                 '涂(+)','菌(-)','未痰检','仅培养','传染性非典','血吸虫病','人禽流感','甲型H1N1流感')]<-"B"         
  data1$abc[data1$disease %in% c('流行性感冒','流行性腮腺炎','风疹','急性出血性结膜炎','麻风病','斑疹伤寒','黑热病','包虫病','丝虫病','其它感染性腹泻病','手足口病')]<-"C"  
  detach(data1)
  
  #标明省名
  data1$province[data1$code1==11]<-'北京'
  data1$province[data1$code1==12]<-'天津'
  data1$province[data1$code1==13]<-'河北'
  data1$province[data1$code1==14]<-'山西'
  data1$province[data1$code1==15]<-'内蒙古'
  data1$province[data1$code1==21]<-'辽宁'
  data1$province[data1$code1==22]<-'吉林'
  data1$province[data1$code1==23]<-'黑龙江'
  data1$province[data1$code1==31]<-'上海'
  data1$province[data1$code1==32]<-'江苏'
  data1$province[data1$code1==33]<-'浙江'
  data1$province[data1$code1==34]<-'安徽'
  data1$province[data1$code1==35]<-'福建'
  data1$province[data1$code1==36]<-'江西'
  data1$province[data1$code1==37]<-'山东'
  data1$province[data1$code1==41]<-'河南'
  data1$province[data1$code1==42]<-'湖北'
  data1$province[data1$code1==43]<-'湖南'
  data1$province[data1$code1==44]<-'广东'
  data1$province[data1$code1==45]<-'广西'
  data1$province[data1$code1==46]<-'海南'
  data1$province[data1$code1==50]<-'重庆'
  data1$province[data1$code1==51]<-'四川'
  data1$province[data1$code1==52]<-'贵州'
  data1$province[data1$code1==53]<-'云南'
  data1$province[data1$code1==54]<-'西藏'
  data1$province[data1$code1==61]<-'陕西'
  data1$province[data1$code1==62]<-'甘肃'
  data1$province[data1$code1==63]<-'青海'
  data1$province[data1$code1==64]<-'宁夏'
  data1$province[data1$code1==65]<-'新疆'
  data1$province[data1$code1==66]<-'建设兵团'
}
