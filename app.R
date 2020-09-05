library(shiny)
library(magrittr)

data <- read.csv("data.csv")
data_names <- data$Name %>% as.vector()
error_text <- c("Check The First Letter","Please Enter a Noun")
lose_text <- c("Using ん","Duplicate word","Error count 3")
test_data <- read.csv("test.csv")
test_data <- test_data$Name

my_data_names <- lapply(1:1000,function(x){return(c(""))})
my_bot_ans <- numeric(1000)
my_bot_ans[1:1000] <- "しりとり"
my_error_count <- numeric(1000)
my_garbage <- lapply(1:1000,function(x){return(c(""))})

makeTitleImage <- function(){
  renderUI({
    tags$div(class = "title-img-container",
             tags$object(id = "c_image",class = "img",tags$img(src = "title.png",height = "320px",width = "216px"))
    )
  })
}

makeCharaImage <- function(img_name,sbn){
  renderUI({
    tags$div(class = "chara-img-container",
             tags$object(id = "c_image",class = "img",tags$img(src = paste0(img_name,sbn,".png"),height = "320px",width = "216px"))
    ) 
  })
}

makeAnsImage <- function(img_name){
  renderUI({
    tags$div(class = "ans-img-container",
             tags$object(id = "a_image",class = "img",tags$img(src = paste0(img_name,".png"),height = "200px",width = "400px"))
    ) 
  })
}

makeLifeImage <- function(img_name){
  renderUI({
    tags$div(class = "life-img-container",
             tags$object(id = "l_image",class = "img",tags$img(src = paste0("life",img_name,".png"),height = "100px",width = "200px"))
    ) 
  })
}

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
  })
}

ui <- fluidPage(
   tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),tags$script(src = "script.js")),
   uiOutput("Title"),
   # titlePanel("Happy Picture Chain Game"),
   # Title
   uiOutput("StartButton"),
   uiOutput("ExplanationButton"),
   # Game
   uiOutput("MemberID"),
   uiOutput("Showcase"),
   uiOutput("ShowChara"),
   uiOutput("InputText"),
   uiOutput("SubmitButton"),
   uiOutput("ErrorCount"),
   uiOutput("ErrorText"),
   uiOutput("Tweet"),
   # Explanation
   uiOutput("ExplanationText"),
   uiOutput("ReturnButton")
)

server <- function(input, output) {
  
  ######## functions ##########################################
  
  MakeStart <- function(){
    output$Title <- makeTitleImage()
    output$StartButton <- makeButton("start_button","Start")
    output$ExplanationButton <- makeButton("explanation_button","Explanation")
  }
  
  DeleteStart <- function(){
    output$StartButton <- renderUI({})
    output$ExplanationButton <- renderUI({})
  }
  
  ChangeAns <- function(num){
    if(num == 0) output$Showcase <- renderUI({h3("しりとり")})
    else  output$Showcase <- makeAnsImage(num)
    output$InputText <- renderUI({textInput("ans_text",label = h3("Your Answer"),value = "")})
  }
  
  ErrorIndication <- function(x,id){
    output$ErrorText <- renderUI({div(error_text[x],style = "color:red")})
    my_error_count[id] <<- my_error_count[id] + 1
    output$ShowChara <- makeCharaImage("chara",1)
    output$ErrorCount <- makeLifeImage(3 - my_error_count[id])
    output$InputText <- renderUI({textInput("ans_text",label = h3("Your Answer"),value = "")})
  }
  
  Termination <- function(x){
    if(x == 0) {
      output$ErrorText <- renderUI({h3("You win")})
      output$Tweet <- TweetMake("You win ")
    }
    else {
      output$ErrorText <- renderUI({h3(paste0("You lose ",lose_text[x]))})
      output$Tweet <- TweetMake("You lose ")
    }
    output$ReturnButton <- makeButton("return_button","Return")
    output$Showcase <- renderUI({}) 
    output$ShowChara <- renderUI({}) 
    output$InputText <- renderUI({})
    output$SubmitButton <- renderUI({})
    output$ErrorCount <- renderUI({})
    output$MemberID <- renderUI({})
  }
  
  TweetMake <- function(text){
    renderUI({ 
      hoge_tweet <- paste0("https://twitter.com/share?url=https://minoeru.shinyapps.io/PictureChainGame/&text=",text,"%20%23みすゲームジャム2020%20")
      tags$div(class = "btn-container",
               tags$button(
                 id = "Tweet_button",class = "btn action-button","結果をツイートする",
                 onclick = paste0( "window.open('", hoge_tweet ,"','_blank')" )
               )
      )
    })
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
    data_names <- data$Name %>% as.vector()
    # 乱数生成
    id <- floor(runif(1) * 1000)
    my_data_names[[id]] <-  data_names
    my_data_names <<-  my_data_names
    my_bot_ans[id] <<- "しりとり"
    my_garbage[[id]] <- c("")
    my_garbage <<- my_garbage
    my_error_count[id] <<- 0
    ChangeAns(0)
    DeleteStart()
    output$Title <- renderUI({})
    output$ShowChara <- makeCharaImage("chara",0)
    output$ErrorCount <- makeLifeImage(3)
    output$SubmitButton <- makeButton("submit_button","Submit")
    output$MemberID <- renderUI({tags$div(class = "hoge-container",  textInput("my_id", label = h3(""),value = id))})
  })
  
  # return process
  observeEvent(input$return_button,{
    output$ExplanationText <- renderUI({})
    output$ReturnButton <- renderUI({})
    output$ErrorText <- renderUI({})
    output$Tweet <- renderUI({})
    MakeStart()
  })
  
  # submit answer
  observeEvent(input$submit_button,{
    
    id <- as.numeric(input$my_id)
    data_names <- my_data_names[[id]]
    bot_ans <- my_bot_ans[id]
    garbage <- my_garbage[[id]]
    
    your_ans <- input$ans_text
    your_ans <- chartr("[ア-ン]","[あ-ん]",your_ans)
    
    bot_ans <- chartr("[ぁぃぅぇぉっゃゅょゎ]","[あいうえおつやゆよわ]",bot_ans)
    bot_ans <- gsub("ー","",bot_ans)
    
    if(chartr("[ア-ン]","[あ-ん]",substr(your_ans,1,1)) !=  substr(bot_ans,nchar(bot_ans),nchar(bot_ans))) ErrorIndication(1,id) # Error1
    else if( is.na(match(your_ans,test_data)) ) ErrorIndication(2,id) # Error2
    else{
      your_ans <- chartr("[ア-ン]","[あ-ん]",your_ans)
      if(substring(your_ans,nchar(your_ans),nchar(your_ans)) == "ん") Termination(1) # Game Over1
      else if(!is.na(match(your_ans,garbage))) Termination(2) # Game Over2
      else{
        output$ErrorText <- renderUI({})
        output$ShowChara <- makeCharaImage("chara",0)
        my_garbage[[id]] <- append(garbage,your_ans,length(garbage))
        my_garbage <<- my_garbage
        last_char <- substring(your_ans,nchar(your_ans),nchar(your_ans))
        tmp <- grep(last_char,substr(data_names,1,1))
        if(length(tmp) == 0) Termination(0)
        else{
          bot_ans <- ifelse(length(tmp) == 1,data_names[tmp],data_names[sample(tmp,1)])
          my_bot_ans[id] <<- bot_ans
          my_garbage[[id]] <- append(garbage,bot_ans,length(garbage))
          my_garbage <<- my_garbage
          my_data_names[[id]] <- data_names[data_names != bot_ans]
          my_data_names <<- my_data_names
          ChangeAns(data[data$Name == bot_ans, ]$No)
        }
      }
    }
    if(my_error_count[id] >= 3) Termination(3) # Game Over3
  })
}
shinyApp(ui = ui, server = server)