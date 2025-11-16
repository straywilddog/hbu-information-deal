library(openxlsx)
library(readtext)

# 函数代码（请勿修改） --------------------------------------------------------------

informaion_deal <- function(dir, year, disabled = "disability", output = T) {
  
  all_doc <- readtext(dir)
  file_size <- length(all_doc$doc_id)
  #创建信息表
  total_data <- data.frame(name = c(rep(NA, file_size)), 
                           sex = c(rep(NA, file_size)), 
                           age = c(rep(NA, file_size)), 
                           household_size = c(rep(NA, file_size)), 
                           only_child = c(rep(NA, file_size)), 
                           farming_size = c(rep(NA, file_size)), 
                           unemployment = c(rep(NA, file_size)), 
                           disabled = c(rep(NA, file_size)), 
                           PCAI = c(rep(NA, file_size)), 
                           debt = c(rep(NA, file_size)))
  for (file_num in 1 : file_size) {
    doc_data <- all_doc[file_num, ]
    #提取信息并分割
    text_data <- strsplit(doc_data$text, split = "\n")
    text_data <- unlist(strsplit(text_data[[1]], split = ":"))
    text_data <- unlist(strsplit(text_data, split = "："))
    #提取名字、性别
    total_data$name[file_num] <- text_data[which(text_data == "姓 名")[1] + 1]
    total_data$sex[file_num] <- text_data[which(text_data == "性 别")[1] + 1]
    #计算年龄
    brith <- as.numeric(paste0(strsplit(text_data[which(text_data == "出生年月") + 1], 
                                        split = "")[[1]][1 : 4]))
    brith <- brith[1] * 10 ^ 3 + brith[2] * 10 ^ 2 + brith[3] * 10 + brith[4]
    total_data$age[file_num] <- year - brith
    #家庭人口
    total_data$household_size[file_num] <- as.numeric(text_data[which(text_data == "家庭人口") + 1])
    #独生子女情况
    total_data$only_child[file_num] <- length(
      which(text_data == "兄妹" | text_data == "兄弟" | 
              text_data == "姐弟" | text_data == "姐妹")
    )
    #务农人口
    total_data$farming_size[file_num] <- length(
      which(text_data == "务农" | text_data == "农民")
    )
    #失业人口
    section_data <- text_data[which(text_data == "父女" | text_data == "父子" | 
                                      text_data == "母女" | text_data == "母子" | 
                                      text_data == "祖孙" | text_data == "兄妹" | 
                                      text_data == "兄弟" | text_data == "姐弟" | 
                                      text_data == "姐妹") + 2]
    total_data$unemployment[file_num] <- length(which(section_data == "无"))
    #病伤残
    section_data <- 
      text_data[which(text_data == "家庭主要成员情况") : 
                  which(text_data == "影响家庭经济状况有关信息")]
    if (disabled == "disability") {
      total_data$disabled[file_num] <- length(which(section_data == "患病" | 
                                                      section_data == "残疾" | 
                                                      section_data == "伤残"))
    } else if (disabled == "level") {
      total_data$disabled[file_num] == length(which(section_data != "健康" | 
                                                      section_data != "良好"))
    }
    
    #家庭人均年收入
    total_data$PCAI[file_num] <- text_data[which(text_data == "家庭人均年收入") + 1]
    total_data$PCAI[file_num] <- as.numeric(unlist(strsplit(total_data$PCAI[file_num], "元"))[1])
    #负债
    total_data$debt[file_num] <- text_data[which(text_data == "家庭欠债情况") + 1]
    if (length(grep("无", total_data$debt[file_num])) > 0) {
      total_data$debt[file_num] <- 0
    } else {
      wan <- 1
      if (length(grep("万", total_data$debt[file_num])) > 0) {wan <- 10000}
      total_data$debt[file_num] <- 
        sum(na.omit(as.numeric(
          regmatches(total_data$debt[file_num], 
                     regexpr("\\d+(\\.\\d+)?", total_data$debt[file_num]))))) * wan
    }
  }
  #修改列名
  colnames(total_data) <- c("姓名", "性别", "年龄", "家庭人口", "独生子女情况", 
                            "务农人数", "失业人数", "伤残人数", "家庭人均年收入", 
                            "负债金额")
  
  #写出
  if (output) {
    write.xlsx(total_data, "部分信息汇总.xlsx", overwrite = T)
    print("表格输出至当前工作目录中部分信息汇总.xlsx文件")
  }
  
  total_data
}
