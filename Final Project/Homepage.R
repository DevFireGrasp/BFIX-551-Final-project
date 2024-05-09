home_page <- tabItem(tabName = "Home",
                     tags$div(
                       style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); text-align: center; background: linear-gradient(to bottom right, steelblue, steelblue);",
                       h1("Welcome to Our Interactive Census Data Explorer", style = "color: white;"),
                       img(src = 'homepage.png', width = '130%'),
                       p("Are you looking to make informed decisions about where to live based on the current state of the economy? Look no further! Our interactive web page provides you with a powerful tool to explore the latest US Census Data from the American Community Survey (ACS).", style = "color:white;")
                     )
)