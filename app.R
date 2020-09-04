# devtools::install_github("IshidaMotohiro/RMeCab")
library(shiny)
library(magrittr)
# library(RMeCab)

data <- read.csv("data.csv")
error_text <- c("Look At The End","Please Enter Noun")
lose_text <- c("Using ん","Duplicate word","Error count 3")

data_names <- data$Name %>% as.vector()

test_data <- read.csv("test.csv")
test_data <- test_data$Name

# 上で配列1000個作る
# start時に乱数でID作る
# IDを隠しておく
# ここで初期化するか〜
# hoge[ran]を<<で変更してアクセス

# my_data_names <- lapply(1:1000,function(x){return(c(""))})
# my_bot_ans <- numeric(1000)
# my_bot_ans[1:1000] <- "しりとり"
# my_error_count <- numeric(1000)
# my_garbage <- lapply(1:1000,function(x){return(c(""))})

makeButton <- function(btn_id,btn_value){
  renderUI({
    tags$div(class = "btn-container",
             tags$button(
               id = btn_id,
               class = "btn action-button",
               #onclick = "cartain()",
               #onfocus = "this.blur();",
               btn_value
             )
    )
  }) %>% return()
}

ui <- fluidPage(
   titlePanel("Happy Picture Shiritori"),
   # Title
   uiOutput("StartButton"),
   uiOutput("ExplanationButton"),
   # Game
   uiOutput("MemberID"),
   uiOutput("BotText"),
   uiOutput("Showcase"),
   uiOutput("InputText"),
   uiOutput("ErrorText"),
   uiOutput("ErrorCount"),
   uiOutput("SubmitButton"),
   # Explanation
   uiOutput("ExplanationText"),
   uiOutput("ReturnButton")
)

server <- function(input, output) {
  
  ######## functions ##########################################
  MakeStart <- function(){
    output$StartButton <- makeButton("start_button","Start")
    output$ExplanationButton <- makeButton("explanation_button","Explanation")
  }
  
  DeleteStart <- function(){
    output$StartButton <- renderUI({})
    output$ExplanationButton <- renderUI({})
  }
  
  ChangeAns <- function(num){
    if(num == 0) output$Showcase <- renderUI({h3("しりとり")})
    else{
      output$Showcase <- renderUI({
        tags$object( id = "image",class = "img",tags$img(src = paste0(num,".png"),height = "100px",width = "400px") )
      })
    }
    output$InputText <- renderUI({textInput("ans_text",label = h3("Your Answer"),value = "")})
  }
  
  ErrorIndication <- function(x){
    output$ErrorText <- renderUI({div(error_text[x],style = "color:red")})
    error_count <<- error_count + 1
    output$ErrorCount <- renderUI({h4(paste0("Your Error is ",error_count))})
    output$InputText <- renderUI({textInput("ans_text",label = h3("Your Answer"),value = "")})
  }
  
  Termination <- function(x){
    if(x == 0) output$ErrorText <- renderUI({h3("You win")})
    else output$ErrorText <- renderUI({h3(paste0("You lose ",lose_text[x]))})
    output$ReturnButton <- makeButton("return_button","Return")
    output$BotText <- renderUI({})
    output$Showcase <- renderUI({}) 
    output$InputText <- renderUI({})
    output$SubmitButton <- renderUI({})
    output$ErrorCount <- renderUI({})
    output$MemberID <- renderUI({})
  }

  ##############################################################
  
  # make start page 
  MakeStart()
  # Explanation page
  observeEvent(input$explanation_button,{
    DeleteStart()
    output$ExplanationText <- renderUI({
      tags$div(class = "exp-txt",
               tags$h2("ルール説明"),
               tags$p("絵が表示されるので，末尾の文字を想像してしりとりを行います"),
               tags$p("3回間違えたり、一度出た単語を再度使用したり、「ん」で終わる文字を送信するとゲームオーバーです"),
               tags$p("一般名詞を平仮名かカタカナで入力してください"),
               tags$p("相手が絵を出せなくなると勝利です！"),
               tags$p("注意！「ー」で終わる単語はその一つ前の文字、捨て仮名は大文字で考えること ex)スキー -> 「き」、いしゃ -> 「や」")
      )
    })
    output$ReturnButton <- makeButton("return_button","Return")
  })
  
  
  
  # Game page
  observeEvent(input$start_button,{
    data_names <<- data$Name %>% as.vector()
    # 乱数生成
    id <- floor(runif(1) * 1000)
    # my_data_names[[id]] <- data$Name %>% as.vector()
    # my_data_names <<- my_data_names
    # my_bot_ans[id] <- "しりとり"
    # my_bot_ans <<- my_bot_ans
    # my_garbage[[id]] <- c("")
    # my_garbage <<- my_garbage
    # my_error_count[id] <- 0
    # my_error_count <<-  my_error_count
    bot_ans <<- "しりとり"
    garbage <<- c("")
    error_count <<- 0
    ChangeAns(0)
    DeleteStart()
    output$BotText <- renderUI({h3("Bot Answer")})
    output$ErrorCount <- renderUI({h4(paste0("Your Error is ",error_count))})
    output$SubmitButton <- makeButton("submit_button","Submit")
    output$MemberID <- renderUI({tags$div(class = "hoge-container",  textInput("my_id", label = h3(""),value = id))})
    
  })
  
  # return process
  observeEvent(input$return_button,{
    output$ExplanationText <- renderUI({})
    output$ReturnButton <- renderUI({})
    output$ErrorText <- renderUI({})
    MakeStart()
  })
  
  # submit answer
  observeEvent(input$submit_button,{
    
    your_id <- as.numeric(input$my_id)
    
    # data_names <- my_data_names[your_id]
    # bot_ans <- my_bot_ans[your_id]
    # garbage <- my_garbage[your_id]
    # 
    # print(data_names)
    # print(bot_ans)
    # print(garbage)
    
    your_ans <- input$ans_text
    your_ans <- chartr("[ア-ン]","[あ-ん]",your_ans)
    
    bot_ans <- chartr("[ぁぃぅぇぉっゃゅょゎ]","[あいうえおつやゆよわ]",bot_ans)
    bot_ans <- gsub("ー","",bot_ans)
    
    if(chartr("[ア-ン]","[あ-ん]",substr(your_ans,1,1)) !=  substr(bot_ans,nchar(bot_ans),nchar(bot_ans))) ErrorIndication(1) # Error1
    # else if(RMeCabC(your_ans) %>% unlist() %>% names() %>% grep("名詞",.) %>% length() != 1) ErrorIndication(2) # Error2
    else if( is.na(match(your_ans,test_data)) ) ErrorIndication(2) # Error2
    else{
      your_ans <- chartr("[ア-ン]","[あ-ん]",your_ans)
      if(substring(your_ans,nchar(your_ans),nchar(your_ans)) == "ん") Termination(1) # Game Over1
      else if(!is.na(match(your_ans,garbage))) Termination(2) # Game Over2
      else{
        output$ErrorText <- renderUI({})
        garbage <<- append(garbage,your_ans,length(garbage))
        last_char <- substring(your_ans,nchar(your_ans),nchar(your_ans))
        tmp <- grep(last_char,substr(data_names,1,1))
        if(length(tmp) == 0) Termination(0)
        else{
          bot_ans <- ifelse(length(tmp) == 1,data_names[tmp],data_names[sample(tmp,1)])
          
          # my_bot_ans[your_id] <<- bot_ans
          # my_garbage[your_id] <<- append(garbage,bot_ans,length(garbage))
          # my_data_names[your_id] <<- data_names[data_names != bot_ans]
          
          bot_ans <<- bot_ans
          garbage <<- append(garbage,bot_ans,length(garbage))
          data_names <<- data_names[data_names != bot_ans]
          
          ChangeAns(data[data$Name == bot_ans, ]$No)
        }
      }
    }
    if(error_count >= 3) Termination(3) # Game Over3
  })
}

shinyApp(ui = ui, server = server)