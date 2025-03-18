library(shiny)
library(shinyAce)




shinyUI(bootstrapPage(
    headerPanel("Dominance Analysis"),


    ########## loading message #######################################

    div(
        id = "loadingMessage",
        "Analysis in progress. Please wait...",
        style = "position: fixed; top: 0; left: 0; width: 100%; background-color: #CCFF66;
            color: black; padding: 10px; text-align: center; font-weight: bold;
            z-index: 9999; display: none;"
    ),
    tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
        var loadingMsg = document.getElementById('loadingMessage');
        var checkIntervalId = null;

        // Initially hide the message
        loadingMsg.style.display = 'none';

        // Dominance Analysis button
        document.getElementById('do').addEventListener('click', function() {
        // Show loading message
        loadingMsg.style.display = 'block';

        // Clear any existing interval
        if (checkIntervalId) clearInterval(checkIntervalId);

        // Start checking for results
        checkIntervalId = setInterval(function() {
            // Check if dominance output exists and has content
            var outputElement = document.getElementById('dominance.out');
            if (outputElement && outputElement.textContent && outputElement.textContent.length > 20) {
            // Results found, hide message
            loadingMsg.style.display = 'none';
            clearInterval(checkIntervalId);
            }
        }, 1000); // Check every second

        // Backup timeout - hide after 60 seconds regardless
        setTimeout(function() {
            loadingMsg.style.display = 'none';
            if (checkIntervalId) clearInterval(checkIntervalId);
        }, 60000);
        });

        // Random Forest button
        document.getElementById('run_forest').addEventListener('click', function() {
        // Show loading message
        loadingMsg.style.display = 'block';

        // Clear any existing interval
        if (checkIntervalId) clearInterval(checkIntervalId);

        // Start checking for results
        checkIntervalId = setInterval(function() {
            // Check if boruta output exists and has content
            var outputElement = document.getElementById('boruta.out');
            if (outputElement && outputElement.textContent && outputElement.textContent.length > 20) {
            // Results found, hide message
            loadingMsg.style.display = 'none';
            clearInterval(checkIntervalId);
            }
        }, 1000); // Check every second

        // Backup timeout - hide after 90 seconds regardless
        setTimeout(function() {
            loadingMsg.style.display = 'none';
            if (checkIntervalId) clearInterval(checkIntervalId);
        }, 90000);
        });
    });
    ")),

    ###################################################################


    mainPanel(
        tabsetPanel(


            # Raw Data
            tabPanel(
                "Raw Data",
                h2("Using Raw Data"),
                p("Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers."),
                p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>
                    Wait patiently. It will take a minute or so until you get to see the result.<br>
                    Please make sure that your data includes the header (variable names) in the first row.<br>
                    The criterion (dependent) variable should be placed in the first column.</b></div>")),
                aceEditor("text", value = "Proficiency\tVocabulary\tGrammar\tPronunciation\n275\t21\t35\t53\n175\t21\t29\t0\n535\t37\t56\t60\n10\t3\t4\t55\n385\t14\t36\t62\n655\t44\t47\t62\n335\t9\t2\t77\n630\t3\t46\t81\n605\t34\t44\t67\n530\t18\t35\t82\n645\t42\t45\t53\n440\t25\t35\t59\n750\t24\t57\t79\n610\t28\t52\t73\n830\t55\t44\t66\n600\t22\t50\t44\n760\t63\t30\t73\n300\t14\t33\t77\n340\t22\t0\t59\n585\t25\t73\t52\n475\t34\t32\t74\n215\t10\t32\t73\n150\t1\t31\t71\n655\t39\t17\t56\n710\t10\t73\t68\n360\t34\t12\t62\n350\t9\t37\t77\n540\t27\t55\t82\n710\t36\t52\t51\n310\t10\t19\t87\n655\t41\t53\t74\n500\t32\t59\t55\n115\t19\t29\t63\n615\t32\t58\t88\n400\t11\t16\t57\n440\t27\t28\t46\n555\t24\t30\t71\n625\t32\t51\t80\n345\t22\t31\t45\n670\t44\t7\t63\n565\t47\t32\t62\n595\t35\t43\t63\n585\t67\t61\t75\n415\t5\t40\t58\n620\t27\t80\t66\n175\t6\t64\t23\n790\t53\t64\t66\n450\t38\t18\t61\n750\t39\t41\t70\n665\t48\t62\t80\n700\t11\t82\t91\n585\t34\t53\t81\n380\t19\t48\t76\n680\t61\t50\t86\n315\t41\t45\t68\n330\t15\t32\t67\n645\t42\t56\t66\n180\t11\t4\t34\n835\t79\t64\t45\n550\t40\t45\t70\n340\t23\t40\t62\n805\t45\t85\t87\n615\t42\t92\t56\n815\t62\t44\t71\n610\t10\t59\t53\n720\t58\t55\t50\n525\t45\t43\t55\n575\t48\t49\t74\n570\t6\t43\t62\n420\t22\t22\t73\n395\t24\t44\t73\n280\t35\t33\t86\n945\t66\t56\t68\n880\t60\t73\t72\n290\t8\t13\t48\n420\t12\t2\t66\n950\t50\t59\t67\n435\t27\t46\t79\n605\t18\t60\t78\n170\t1\t47\t12\n940\t60\t70\t63\n260\t11\t35\t73\n335\t7\t34\t78\n270\t9\t60\t45\n670\t45\t39\t70\n860\t59\t100\t72\n835\t68\t36\t73\n850\t61\t58\t68\n405\t9\t55\t53\n500\t25\t34\t65\n490\t20\t32\t49\n515\t28\t72\t64\n950\t89\t47\t63\n425\t4\t37\t73\n430\t12\t28\t73\n695\t36\t55\t64\n150\t11\t28\t80\n355\t12\t41\t81\n785\t39\t62\t74\n250\t31\t26\t61\n560\t12\t45\t75\n425\t6\t14\t60\n455\t39\t20\t65\n815\t59\t62\t92\n715\t51\t85\t74\n270\t17\t44\t65\n395\t8\t30\t78\n210\t8\t33\t20\n705\t30\t50\t89\n250\t23\t1\t69\n665\t45\t54\t69\n440\t26\t29\t70\n805\t55\t74\t97\n180\t9\t21\t82\n795\t77\t53\t66\n605\t29\t47\t76\n280\t10\t19\t69\n605\t8\t57\t75\n250\t5\t11\t33\n525\t33\t63\t58\n570\t43\t29\t59\n535\t2\t64\t66\n425\t39\t12\t71\n330\t18\t82\t70\n555\t10\t43\t72\n550\t34\t29\t71\n870\t51\t59\t62\n550\t18\t17\t45\n840\t49\t66\t84\n665\t67\t41\t54\n210\t18\t31\t84\n405\t13\t41\t91\n585\t22\t31\t92\n520\t17\t67\t75\n585\t35\t55\t57\n390\t21\t31\t59\n830\t69\t37\t88\n400\t10\t23\t72\n240\t11\t31\t38\n275\t12\t29\t64\n340\t18\t51\t61\n105\t21\t15\t34\n615\t8\t39\t65\n175\t14\t33\t38\n425\t25\t56\t72\n275\t10\t35\t60\n475\t27\t28\t48\n765\t32\t53\t80\n810\t51\t84\t50\n825\t51\t64\t73\n795\t35\t54\t68\n585\t37\t56\t64\n610\t26\t47\t81\n595\t49\t28\t72\n490\t5\t75\t65\n880\t78\t49\t97\n605\t4\t59\t57\n765\t35\t31\t72\n650\t52\t35\t57\n430\t23\t50\t78\n625\t26\t52\t74\n700\t13\t61\t77\n585\t25\t59\t70\n895\t100\t70\t100\n190\t1\t57\t68\n460\t25\t34\t70\n680\t68\t17\t75\n915\t69\t69\t66\n120\t1\t20\t84\n150\t29\t30\t57\n265\t12\t37\t55\n275\t0\t3\t62\n935\t67\t58\t80\n700\t25\t51\t69\n750\t48\t52\t70\n430\t45\t19\t61\n240\t8\t6\t74\n660\t35\t66\t69\n405\t10\t40\t79\n950\t57\t65\t82\n660\t18\t37\t74\n425\t36\t15\t64\n615\t32\t81\t69\n355\t23\t24\t69\n550\t48\t21\t77\n635\t32\t73\t78\n190\t28\t29\t16\n170\t32\t16\t54\n125\t6\t41\t78\n410\t6\t31\t62\n715\t68\t44\t85\n990\t88\t55\t49\n785\t58\t40\t75\n320\t12\t51\t65\n890\t78\t53\t47\n565\t38\t38\t66\n525\t11\t73\t55\n730\t11\t67\t73\n750\t53\t49\t74\n565\t69\t51\t79", mode = "r", theme = "cobalt"),
                br(),
                tags$head(
                    tags$style(HTML("
                    .btn-modern {
                    margin-bottom: 10px;
                    text-transform: uppercase;
                    font-weight: bold;
                    border-radius: 4px;
                    box-shadow: 0 2px 5px rgba(0,0,0,0.16), 0 2px 5px rgba(0,0,0,0.23);
                    transition: all 0.3s;
                    }
                    .btn-modern:hover {
                    box-shadow: 0 4px 10px rgba(0,0,0,0.25), 0 4px 10px rgba(0,0,0,0.22);
                    transform: translateY(-2px);
                    }
                    .btn-primary {
                    background-color: #2196F3;
                    color: white;
                    border-color: #1E88E5;
                    }
                    .btn-success {
                    background-color: #4CAF50;
                    color: white;
                    border-color: #43A047;
                    }
                "))
                ),
                actionButton("do", "Run Dominance Analysis",
                    class = "btn-primary btn-modern",
                    icon = icon("bar-chart")
                ),
                h3("Basic Statistics"),
                verbatimTextOutput("textarea.out"),
                br(),
                h3("Correlation"),
                plotOutput("corPlot"),
                br(),
                h3("Regression Analysis"),
                verbatimTextOutput("reg.out"),
                br(),
                h3("Dominance Analysis"),
                verbatimTextOutput("dominance.out"),
                br(),
                h3("Dominance Weight Plot with 95% CI"),
                plotOutput("confPlot"),
                br(),
                h3("Just for Reference: Relative Weight Analysis"),
                p(HTML("Note: Relative weight analysis is criticized and not recommended due to mathematical concerns (see <a href='https://doi.org/10.1080/00273171.2014.905766' target='_blank'>this paper</a>).")),
                verbatimTextOutput("rwa.out"),
                br(),
                h3("Feature Selection: Variable Importance (Random Forest)"),
                actionButton("run_forest", "Run Random Forest",
                    class = "btn-success btn-modern",
                    icon = icon("tree")
                ),
                plotOutput("randomForestPlot"),
                verbatimTextOutput("boruta.out"),
                br(),
                br(),
                strong("R session info"),
                verbatimTextOutput("info1.out")
            ),




            # Correlation Matrix

            tabPanel(
                "Correlation Matrix",
                h2("Using Correlation Matrix"),
                p("Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers."),
                p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Please make sure that your data includes the header (variable names) in the first row.<br>
                    The criterion (dependent) variable should be placed in the first column.</b></div>")),
                aceEditor("text2",
                    value = "\tScore\tWordcount\tCLI\tCommas\tStopwords\tLinking\tWordsSentence\nScore\t1\t0.67\t0.41\t0.02\t-0.35\t0.35\t0.45\nWordcount\t0.67\t1\t0.18\t0.05\t-0.28\t0.24\t0.31\nCLI\t0.41\t0.18\t1\t0.21\t-0.32\t0.34\t0.43\nCommas\t0.02\t0.05\t0.21\t1\t-0.03\t0.22\t0.47\nStopwords\t-0.35\t-0.28\t-0.32\t-0.03\t1\t-0.13\t-0.22\nLinking\t0.35\t0.24\t0.34\t0.22\t-0.13\t1\t0.33\nWordsSentence\t0.45\t0.31\t0.43\t0.47\t-0.22\t0.33\t1", mode = "r", theme = "cobalt"
                ),
                br(),
                numericInput("n", " Sample Size", 200),
                h3("Check the Input Correlation Matrix"),
                verbatimTextOutput("inputData.out"),
                br(),
                h3("Regression Analysis"),
                verbatimTextOutput("mra2.result.out"),
                br(),
                h3("Dominance Analysis"),
                p("Note: The 95% confidence interval (CI) cannot be calculated from the correlation matrix."),
                verbatimTextOutput("dominance.cor.out"),
                br(),
                h3("Dominance Weight Plot"),
                p("Note: The 95% confidence interval (CI) cannot be calculated from the correlation matrix."),
                plotOutput("imPlot"),
                br(),
                h3("Just for Reference: Relative Weight Analysis"),
                p(HTML("Note: Relative weight analysis is criticized and not recommended due to mathematical concerns (see <a href='https://doi.org/10.1080/00273171.2014.905766' target='_blank'>this paper</a>).")),
                verbatimTextOutput("wra.cor.out"),
                br(),
                br(),
                strong("R session info"),
                verbatimTextOutput("info2.out")
            ),



            # About

            tabPanel(
                "About",
                strong("Note"),
                p(
                    "This web application is developed with",
                    a("Shiny.", href = "http://www.rstudio.com/shiny/", target = "_blank"),
                    ""
                ),
                br(),
                strong("List of Packages Used"), br(),
                code("library(shiny)"), br(),
                code("library(shinyAce)"), br(),
                code("library(psych)"), br(),
                code("library(car)"), br(),
                code("library(rpsychi)"), br(),
                code("library(boot)"), br(),
                code("library(plyr)"), br(),
                code("library(ggplot2)"), br(),
                code("library(Boruta)"), br(),
                code("library(relaimpo)"), br(),
                code("library(MASS)"), br(),
                code("library(yhat)"), br(),
                br(),
                strong("Code"),
                p(
                    "Source code for this application is based on",
                    a('"RWA Web" (by Dr. Scott Tonidandel)', href = "https://relativeimportance.davidson.edu/", target = "_blank")
                ),
                p("and", a('"The handbook of Research in Foreign Language Learning and Teaching" (Takeuchi & Mizumoto, 2012).', href = "https://mizumot.com/handbook/", target = "_blank")),
                p(
                    "The code for this web application is available at",
                    a("GitHub.", href = "https://github.com/mizumot/relimp", target = "_blank")
                ),
                br(),
                strong("Citation in Publications"),
                p("Mizumoto, A. (2015). Langtest (Version 1.0) [Web application]. https://langtest.jp"),
                br(),
                strong("Article"),
                p("Mizumoto, A., & Plonsky, L. (2016).", a("R as a lingua franca: Advantages of using R for quantitative research in applied linguistics.", href = "https://doi.org/10.1093/applin/amv025", target = "_blank"), em("Applied Linguistics,"), "37(2), 284–291. https://doi.org/10.1093/applin/amv025"),
                p("Mizumoto, A. (2023).", a("Calculating the relative importance of multiple regression predictor variables using dominance analysis and random forests.", href = "https://doi.org/10.1111/lang.12518", target = "_blank"), em("Language Learning,"), "73(1), 161–196. https://doi.org/10.1111/lang.12518"),
                br(),
                strong("Recommended"),
                p(
                    "To learn more about R, I suggest this excellent and free e-book (pdf),",
                    a("A Guide to Doing Statistics in Second Language Research Using R,", href = "https://www.tufs.ac.jp/ts/personal/corpuskun/pdf/2015/__A_Guide_to_Doing_Statistics_in_SLA_Using_R.pdf", target = "_blank"),
                    "written by Dr. Jenifer Larson-Hall."
                ),
                br(),
                strong("Developer"),
                p(
                    a("Atsushi MIZUMOTO,", href = "https://mizumot.com", target = "_blank"), " Ph.D.", br(),
                    "Professor of Applied Linguistics", br(),
                    "Faculty of Foreign Language Studies /", br(),
                    "Graduate School of Foreign Language Education and Research,", br(),
                    "Kansai University, Osaka, Japan"
                ),
                br(),
                a(img(src = "https://creativecommons.jp/wp-content/uploads/2015/04/by-nc-sa.png?w=75"), target = "_blank", href = "https://creativecommons.org/licenses/by-nc-sa/4.0/"),
                p(br())
            )
        )
    )
))
