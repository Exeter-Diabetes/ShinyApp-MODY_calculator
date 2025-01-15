####################
## Description:
##  - In this file we make a Shiny App prototype for the 
##      MODY calculator v2
####################


# C-peptide limits
## Plasma
### pmol/L
# min: 5
# max: 5000
# cut-off: 200
### ng/ml
# min: 0.1 (5*0.003)
# max: 15 (5000*0.003)
# cut-off: 0.6 (200*0.003)
## UCPCR
### nmol/mmol
# min: 0.01
# max: 20
# cut-off: 0.2

suppressPackageStartupMessages(require(shiny))
suppressPackageStartupMessages(require(shinythemes))
suppressPackageStartupMessages(require(shinyWidgets))
suppressPackageStartupMessages(require(tibble))
suppressPackageStartupMessages(require(bslib))
suppressPackageStartupMessages(require(shinycssloaders))

#:--------------------------------------------------------------------------------
# Define UI for inputting patient values

# Define UI for inputting patient values
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  fluidRow(
    ### colour for the patient features
    tags$head(tags$style(HTML(".grey-row {background-color: #f2f2f2;}"))),
    
    tags$style(HTML("
      #first {
          border-radius: 10px !important;
          border: 10px solid grey;
          background-color: white
      }
    ")),
    
    ## Title of calculator
    column(10,
           # Title of the page
           titlePanel("MODY Calculator (prototype)"),
           # PSA for using the app
           h5("Please note this is a beta version provided for academic research and validation purposes and should not be used for clinical decision making.", style="color:black"),
           h5("If the patient has just been diagnosed with diabetes in the last 8 weeks, please use", tags$a("this calculator", target = "_blank", href = "https://julieanneknupp.shinyapps.io/mody_bdd/"), ".", style="color:black")
    ),
    column(2,
           br(),
           img(src='uni_logo.JPG', align = "right", width = "200"),
    )
  ),
  ## Sidebar layout
  sidebarLayout(
    # Patient characteristics
    sidebarPanel(
      style = "height: 88vh; overflow-y: auto;",
      width = 5,
      ## Title of patient features
      fluidRow(
        column(8, h2("Clinical information:"))
      ),
      ## Patient features listed
      fluidRow(
        column(12,
               # Current features
               tags$div(title="Current Age must be between 1 and 120",numericInput(inputId = "agerec_num", label = p("Current Age (years):", style = "font-size:16px;"), value = NA, min = 1, max = 120)),
               awesomeRadio(inputId = "sex_select", label = p("Sex:", style = "font-size:16px;"), choices = c("Male","Female"), inline = TRUE, selected = "Male"),
               tags$div(title="BMI must be between 10 and 70",numericInput(inputId = "bmi_num", label = p("BMI (kg/m", tags$sup("2"),"):", style = "font-size:16px;"), value = NA, min = 10, max = 70)),
               fluidRow(
                 column(6, tags$div(title="HbA1c must be between 3% and 15% (9mmol/mol or 140mmol/mol)",numericInput(inputId = "hba1c_combined_num", label = p("HbA1c:", style = "font-size:16px;"), value = NA, min = 3, max = 140))),
                 column(6, tags$div(title="HbA1c must be between 3% and 15% (9mmol/mol or 140mmol/mol)",selectInput(inputId = "hba1c_unit_select", label = p("Units", style = "font-size:16px;"), choices = c("%" = "%", "mmol/mol" = "mmol"), width = "100%")))
               ),
               # Diagnosis
               tags$div(title="Age at diagnosis must be between 1 and 35",numericInput(inputId = "age_dx_num", label = p("Age at diagnosis (years):", style = "font-size:16px;"), value = NA, min = 1, max = 35)),
               tags$div(title="Are the patient's parents affected with diabetes?",awesomeRadio(inputId = "pardm_select", label = p("Parent affected with diabetes:", style = "font-size:16px;"), choices = c("Yes", "No"), inline = TRUE, selected = "Yes")),
               # Treatment
               awesomeRadio(inputId = "insoroha_select", label = p("Currently treated with insulin or glucose-lowering medication (excluding insulin):", style = "font-size:16px;"), choices = c("Yes", "No"), inline = TRUE, selected = "Yes"),
               awesomeRadio(inputId = "insulin_treatment_select", label = p("Time to insulin treatment (if currently treated with insulin):", style = "font-size:16px;"), choices = c("Not currently treated with insulin", "Within 6 months of diagnosis", "Over 6 months after diagnosis"), inline = FALSE, selected = "Not currently treated with insulin"),
               # Conditional panel of: c-peptide and antibody tests
               conditionalPanel(
                 condition = "input.insulin_treatment_select == 'Within 6 months of diagnosis'",
                 awesomeRadio(inputId = "c_pep_testing_done", label = p("C-peptide testing:", style = "font-size:16px;font-weight:bold;"), choices = c("Yes","No"), inline = TRUE, selected = "No"),
                 conditionalPanel(
                   condition = "input.c_pep_testing_done == 'Yes'",
                   fluidRow(
                     column(4, pickerInput(inputId = "c_pep_sample_type", label = p("Sample", stype = "font-size:16px;"), choices = c("Plasma", "UCPCR"), options = list(title = "Type"))),
                     column(4,
                            conditionalPanel(
                              condition = "input.c_pep_sample_type == 'UCPCR'",
                              pickerInput(inputId = "c_pep_ucpcr_units", label = p("Units", style = "font-size:16px;"), choices = c("nmol/mmol"), selected = "nmol/mmol")
                            ),
                            conditionalPanel(
                              condition = "input.c_pep_sample_type == 'Plasma'",
                              pickerInput(inputId = "c_pep_plasma_units", label = p("Units", style = "font-size:16px;"), choices = c("pmol/L", "ng/ml"), options = list(title = "Units"))
                            )
                     ),
                     column(4,
                            conditionalPanel(
                              condition = "input.c_pep_sample_type == 'UCPCR'",
                              tags$div(title="UCPCR must be between 0.01nmol/mmol and 20nmol/mmol",numericInput(inputId = "c_pep_ucpcr_num", label = p("Value", style = "font-size:16px;"), value = NA, min = 0.01, max = 20))
                            ),
                            conditionalPanel(
                              condition = "input.c_pep_sample_type == 'Plasma' && input.c_pep_plasma_units == 'pmol/L'",
                              tags$div(title="Plasma must be between 5pmol/L and 5000pmol/L", numericInput(inputId = "c_pep_plasma_pmolL_num", label = p("Value", style = "font-size:16px;"), value = NA, min = 5, max = 5000))
                            ),
                            conditionalPanel(
                              condition = "input.c_pep_sample_type == 'Plasma' && input.c_pep_plasma_units == 'ng/ml'",
                              tags$div(title="Plasma must be between 0.1ng/ml and 15ng/ml", numericInput(inputId = "c_pep_plasma_ngml_num", label = p("Value", style = "font-size:16px;"), value = NA, min = 0.1, max = 15))
                            )
                     )
                   )
                 ),
                 awesomeRadio(inputId = "antibody_testing_done", label = p("Islet autoantibody testing:", style = "font-size:16px;font-weight:bold;"), choices = c("Yes","No"), inline = TRUE, selected = "No"),
                 conditionalPanel(
                   condition = "input.antibody_testing_done == 'Yes'",
                   fluidRow(
                     column(12,
                            awesomeRadio(inputId = "antibody_gad_test", label = p("GAD", style = "font-size:16px;font-weight:bold;"), choices = c("Positive", "Negative", "Not Done"), inline = TRUE, selected = "Not Done"),
                     ),
                     column(12,
                            awesomeRadio(inputId = "antibody_ia2_test", label = p("IA2", style = "font-size:16px;font-weight:bold;"), choices = c("Positive", "Negative", "Not Done"), inline = TRUE, selected = "Not Done"),
                     ),
                     column(12,
                            awesomeRadio(inputId = "antibody_znt8_test", label = p("ZnT8", style = "font-size:16px;font-weight:bold;"), choices = c("Positive", "Negative", "Not Done"), inline = TRUE, selected = "Not Done"),
                     ),
                   )
                 )
               )
        )
      ),
      fluidRow(
        column(4,
               actionButton("calculate", "Calculate"),
               br(),
               br() # space/padding below the calculate button so that the text isn't glued to it
        ),
        column(4,
               actionButton("reset", "New Patient"),
               br(),
               br() # space/padding below the calculate button so that the text isn't glued to it
        )
      ),
      fluidRow(
        column(12, HTML("For any enquiries, please contact:<br/>p.cardoso@exeter.ac.uk<br/>j.knupp@exeter.ac.uk"))
      )
    ),
    
    mainPanel(
      width = 7,
      column(12,
             # page_fillable(
             #:------------------------------------------------------------------
             # MODY calculator results
             conditionalPanel(
               class = "grey-row",
               condition = 'output.panelStatus',
               fluidRow(id = "first",
                        column(12,
                               p("Probability of MODY:", style="font-size:30px;color:#327eac"),
                               tags$head(tags$style("#mody_cal_v2_text{font-size: 26px;}")),
                               htmlOutput("mody_cal_v2_text"),
                               style = "padding:25px;"
                        )
               )
             ),
             
             br(),
             
             #:------------------------------------------------------------------
             ## Patient details summary:
             conditionalPanel(
               class = "grey-row",
               condition = "input.calculate > 0",
               # wellPanel(
               fluidRow(id = "first",
                        column(12,
                               h3("Patient summary:"),
                               column(6,
                                      p("Current features:",
                                        style="font-weight:bold;font-size:20px;"),
                                      tags$head(tags$style("#patient_details_agerec{font-size: 16px;}")),
                                      htmlOutput("patient_details_agerec"),
                                      tags$head(tags$style("#patient_details_sex{font-size: 16px;}")),
                                      textOutput("patient_details_sex"),
                                      tags$head(tags$style("#patient_details_bmi{font-size: 16px;}")),
                                      htmlOutput("patient_details_bmi"),
                                      tags$head(tags$style("#patient_details_hba1c{font-size: 16px;}")),
                                      htmlOutput("patient_details_hba1c"),
                                      br(),
                                      tags$head(tags$style("#patient_details_age{font-size: 16px;}")),
                                      htmlOutput("patient_details_age"),
                               ),
                               column(6,
                                      p("Treatment:",
                                        style="font-weight:bold;font-size:20px;"),
                                      tags$head(tags$style("#patient_details_insoroha{font-size: 16px;}")),
                                      textOutput("patient_details_insoroha"),
                                      tags$head(tags$style("#patient_details_time_to_insulin{font-size: 16px;}")),
                                      htmlOutput("patient_details_time_to_insulin"),
                                      br(),
                                      p("Family history:",
                                        style="font-weight:bold;font-size:20px;"),
                                      tags$head(tags$style("#patient_details_pardm{font-size: 16px;}")),
                                      textOutput("patient_details_pardm")
                               ), style = "padding:25px;"
                        ),
                        
                        # Panel with patient's cpep and antibody information
                        uiOutput("patient_cpep_antibody_info_panel"),
                        
                        # Panel giving information about how the cpep and antibody information is used
                        column(12,
                               uiOutput("additional_information_panel"), style = "padding:25px;"
                        ),
               )
             ),
             
             br(),
             
             conditionalPanel(
               condition = "input.calculate > -1",
               tabsetPanel(id = "tabs",
                           selected = "Extra Information",
                           tabPanel("Extra Information",
                                    br(),
                                    h4("This is an example calculator that will accompany a paper, any feedback will help inform the final CE marked version.", style="color:black"),
                                    h4(tags$a("Please leave your feedback here.", target = "_blank", href = "https://forms.office.com/e/gHAeTLr7XF"), style = "color:black"),
                                    br(),
                                    # HTML("This calculator has been developed in conjunction with University of Exeter."),
                                    tags$footer("More information on MODY can be found here: ", tags$a("www.diabetesgenes.org/what-is-mody/", target = "_blank", href = "https://www.diabetesgenes.org/what-is-mody/")),
                                    br(),
                                    tags$footer("Publications related to this work can be found here:"),
                                    HTML(paste0("<ul><li>", tags$a("Shields et al., Diabetologia (2012)", target = "_blank", href = "https://doi.org/10.1007/s00125-011-2418-8"), "</li><li>", tags$a("Cardoso et al., BMC Med Res Methodol (2024)", target = "_blank", href = "https://doi.org/10.1186/s12874-024-02239-w"), "</li></ul>")),
                                    br(),
                                    br()
                           )
               )
             )
      )
      
      
      
    )
  )
)



#:--------------------------------------------------------------------------------
# Define server for APP
server <- function(input, output, session) {
  
  #:---------------------------------------------------------------
  # libraries
  suppressPackageStartupMessages({
    require(tidyverse)
    require(rms)
  })
  
  # load functions
  source("functions/prediction_functions.R")
  
  # load model posterior samples
  rcs_parms <- readRDS("posteriors/rcs_parms.rds")
  posterior_samples_T1D <- readRDS("posteriors/type_1_model_posteriors_single_value.rds")
  posterior_samples_T2D <- readRDS("posteriors/type_2_model_posteriors_single_value.rds")
  
  ### create object to use for prediction
  posterior_samples_T1D_obj <- list(post = posterior_samples_T1D$samples)
  class(posterior_samples_T1D_obj) <- "T1D"
  
  posterior_samples_T2D_obj <- list(post = posterior_samples_T2D$samples)
  class(posterior_samples_T2D_obj) <- "T2D"
  
  
  #:---------------------------------------------------------------
  # collect patient details
  observeEvent(input$reset, {session$reload()})
  
  
  patient <- eventReactive(input$calculate, {   # can use this one so that it is reactive with the click
    # patient <- reactive({
    # join patient's data
    patient <- NULL
    # age of diagnosis
    patient$agedx <- as.numeric(input$age_dx_num)
    # sex
    patient$sex <- ifelse(input$sex_select == "Male", 0, 1)
    # insulin or tablets
    patient$insoroha <- ifelse(input$insoroha_select == "Yes", 1, 0)
    # progression to insulin
    if (input$insulin_treatment_select %in% c("Over 6 months after diagnosis")) {
      patient$progression_to_insulin <- factor("not progressed", levels = c("progressed", "not progressed", "not treated"))
    } else if (input$insulin_treatment_select %in% c("Not currently treated with insulin")) {
      patient$progression_to_insulin <- factor("not treated", levels = c("progressed", "not progressed", "not treated"))
    } else {
      patient$progression_to_insulin <- factor("progressed", levels = c("progressed", "not progressed", "not treated"))
    }
    # bmi
    patient$bmi <- as.numeric(input$bmi_num)
    # hba1c per
    if (input$hba1c_unit_select == "%") {
      patient$hba1c_per <- as.numeric(input$hba1c_combined_num)
    }
    # hba1c mmol
    if (input$hba1c_unit_select == "mmol") {
      patient$hba1c_per <- as.numeric(sprintf(((as.numeric(input$hba1c_combined_num))/10.929) + 2.15, fmt = "%#.1f"))
      patient$hba1c_mol <- as.numeric(input$hba1c_combined_num)
    }
    # current age
    patient$agerec <- as.numeric(input$agerec_num)
    # parent history
    patient$pardm <- ifelse(input$pardm_select == "Yes", 1, 0)
    # C-peptide testing
    handle_c_peptide <- function(value, lower_limit, upper_limit, cutoff) {
      # Check if value is NA, within limits
      if (!is.na(value) && value >= lower_limit && value <= upper_limit) {
        # return value and corresponding C value
        list(value = as.numeric(value), result = ifelse(value < cutoff, 0, 1))
      } else {
        # return NA
        list(value = NA, result = NA)
      }
    }
    # C-peptide testing
    # If patient with 6 months and c-pep done
    if (input$insulin_treatment_select == "Within 6 months of diagnosis" && input$c_pep_testing_done == "Yes") {
      if (input$c_pep_sample_type == "UCPCR" && input$c_pep_ucpcr_units %in% c("nmol/mmol")) {
        c_peptide <- handle_c_peptide(input$c_pep_ucpcr_num, 0.01, 20, 0.2)
        patient$C_ucpcr <- c_peptide$value
      } else if (input$c_pep_sample_type == "Plasma") {
        if (input$c_pep_plasma_units == "pmol/L") {
          c_peptide <- handle_c_peptide(input$c_pep_plasma_pmolL_num, 5, 5000, 200)
          patient$C_pasma_pmolL <- c_peptide$value
        } else if (input$c_pep_plasma_units == "ng/ml") {
          c_peptide <- handle_c_peptide(input$c_pep_plasma_ngml_num, 0.1, 15, 200*0.003)  # converted cutoff 1000 pmol/l = 3 ng/ml [ref PMID: 23413806]
          patient$C_pasma_ngml <- c_peptide$value
        } else {
          c_peptide <- list(value = NA, result = NA)
        }
      } else {
        c_peptide <- list(value = NA, result = NA)
      }
      patient$C <- c_peptide$result
    } else {
      patient$C <- NA
    }
    # Antibody testing
    if (input$insulin_treatment_select == "Within 6 months of diagnosis" && input$antibody_testing_done == "Yes") {
      patient$A_ia2 <- ifelse(input$antibody_ia2_test == "Positive", 1, ifelse(input$antibody_ia2_test == "Negative", 0, NA))
      patient$A_gad <- ifelse(input$antibody_gad_test == "Positive", 1, ifelse(input$antibody_gad_test == "Negative", 0, NA))
      patient$A_znt8 <- ifelse(input$antibody_znt8_test == "Positive", 1, ifelse(input$antibody_znt8_test == "Negative", 0, NA))
      
      if (all(is.na(c(patient$A_ia2, patient$A_gad, patient$A_znt8)))) {
        patient$A <- NA
      } else {
        patient$A <- ifelse(sum(c(patient$A_ia2, patient$A_gad, patient$A_znt8), na.rm = TRUE) > 0, 1, 0)
      }
    } else {
      patient$A <- NA
    }
    # turn into data.frame
    patient <- as.data.frame(patient)
    # check missingness - using this to ensure patients don't have missingness
    patient$missingness <- ifelse(any(is.na(patient[c("agedx", "bmi", "hba1c_per", "agerec")])), 1, 0)
    
    # return data.frame
    return(patient)
  })
  
  #:---------------------------------------------------------------
  ## Choice whether panels should be shown (don't show if missingness)
  output$panelStatus <- reactive({
    # collect patient values
    patient <- patient()
    # decide whether panel should be shown
    if (input$calculate == 0 | patient$missingness == 1) {
      # Output message for logs
      print("Prediction attempt")
      return(FALSE)
    }
    # if patient T2D, show panel
    if (patient$progression_to_insulin == "not treated") {
      return(TRUE)
    }
    # Check C-peptide and antibodies status
    if (is.na(patient$C) || is.na(patient$A) || is.na(patient$A_ia2) || is.na(patient$A_gad)) {
      return(FALSE)
    }
    return(TRUE)
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  
  
  #:---------------------------------------------------------------
  ## Patient summary:
  # Age at diagnosis
  output$patient_details_age <- renderText({
    patient <- patient()
    if (is.na(patient$agedx) | patient$agedx < 1 | patient$agedx > 35) {
      paste("Age at diagnosis:", "<span style=\"color:red\">must be \u22651 or \u226435</span>")
    } else {
      paste("Age at diagnosis:", patient$agedx)
    }
  })
  # Sex
  output$patient_details_sex <- renderText({
    patient <- patient()
    paste("Sex:", ifelse(patient$sex == 0, "Male", "Female"))
  })
  # Current treatment
  output$patient_details_insoroha <- renderText({
    patient <- patient()
    if (patient$insoroha == 1) {
      if (patient$progression_to_insulin == "not treated") {
        paste("Currently treated with tablets")
      } else {
        paste("Currently treated with insulin or tablets")
      }
    } else {
      paste("Not currently treated with insulin or tablets")
    }
  })
  # Progression to insulin
  output$patient_details_time_to_insulin <- renderText({
    patient <- patient()
    if (patient$progression_to_insulin != "not treated") {
      if (patient$progression_to_insulin == "progressed") {
        paste("Progressed to insulin within 6 months of diagnosis")
      } else {
        paste("Progressed to insulin after 6 months of diagnosis")
      }
    } else {
      paste("")
    }
  })
  # BMI
  output$patient_details_bmi <- renderText({
    patient <- patient()
    if (is.na(patient$bmi) | patient$bmi < 10 | patient$bmi > 70) {
      paste("BMI:", "<span style=\"color:red\">must be \u226510 or \u226470</span>")
    } else {
      paste("BMI:", patient$bmi)
    }
  })
  # HbA1c (in mmol/mol and percentage)
  output$patient_details_hba1c <- renderText({
    patient <- patient()
    if (is.null(patient$hba1c_mol)) {
      if (is.na(patient$hba1c_per) | patient$hba1c_per < 3 | patient$hba1c_per > 15) {
        paste("HbA1c:", "<span style=\"color:red\">must be \u22653% or \u226415%</span>")
      } else {
        paste("HbA1c:", patient$hba1c_per, "%")
      }
    } else {
      if (is.na(patient$hba1c_mol) | patient$hba1c_mol < 9 | patient$hba1c_mol > 140) {
        paste("HbA1c:", "<span style=\"color:red\">must be \u22659 or \u2264140 mmol/mol</span>")
      } else {
        paste0("HbA1c: ", sprintf(patient$hba1c_per, fmt = "%#.1f"), " % (", patient$hba1c_mol, " mmol/mol)")
      }
    }
  })
  # Age at recruitment
  output$patient_details_agerec <- renderText({
    patient <- patient()
    
    if (is.na(patient$agerec) | patient$agerec < 1 | patient$agerec >120) {
      paste("Current age:", "<span style=\"color:red\">must be \u22651 or \u2264120</span>")
    } else if (!is.na(patient$agedx) & patient$agerec < patient$agedx) {
      paste("Current age:", "<span style=\"color:red\">must be \u2265 age at diagnosis</span>")
    } else {
      paste("Current age:", patient$agerec)
    }
    
  })
  # Parent affected with diabetes
  output$patient_details_pardm <- renderText({
    patient <- patient()
    if (patient$pardm == 1) {
      paste("Parent affected with diabetes")
    } else {
      paste("Parents not affected with diabetes")
    }
  })
  # C-peptide testing information
  output$patient_details_c_pep <- renderText({
    ## load patient
    patient <- patient()
    if (patient$progression_to_insulin == "progressed") {
      if (!is.na(patient$C)) {
        if (patient$C == 0) {
          if (input$c_pep_sample_type == "UCPCR" && input$c_pep_ucpcr_units %in% c("nmol/mmol")) {
            paste0("Evidence of low insulin production: ", patient$C_ucpcr, " nmol/mmol")
          } else if (input$c_pep_sample_type == "Plasma") {
            if (input$c_pep_plasma_units == "pmol/L") {
              paste0("Evidence of low insulin production: ", patient$C_pasma_pmolL, " pmol/L")
            } else if (input$c_pep_plasma_units == "ng/ml") {
              paste0("Evidence of low insulin production: ", patient$C_pasma_ngml, " ng/ml")
            }
          }
        } else {
          if (input$c_pep_sample_type == "UCPCR" && input$c_pep_ucpcr_units %in% c("nmol/mmol")) {
            paste0("Evidence of insulin production: ", patient$C_ucpcr, " nmol/mmol")
          } else if (input$c_pep_sample_type == "Plasma") {
            if (input$c_pep_plasma_units == "pmol/L") {
              paste0("Evidence of insulin production: ", patient$C_pasma_pmolL, " pmol/L")
            } else if (input$c_pep_plasma_units == "ng/ml") {
              paste0("Evidence of insulin production: ", patient$C_pasma_ngml, " ng/ml")
            }
          }
        }
      } else {
        checkLimits <- function(value, lower, upper, name, unit) {
          if (is.na(value) || value < lower || value > upper) {
            paste(name, ":", "<span style=\"color:red\"> must be ≥", lower, unit, " or ≤", upper, unit, "</span>", sep = "")
          } else {
            paste("<span style=\"color:red\">This is an error and shouldn't be here.</span>")
          }
        }
        
        if (!is.null(patient$C_ucpcr)) {
          checkLimits(patient$C_ucpcr, 0.01, 20, "UCPCR", "nmol/mmol")
        } else if (!is.null(patient$C_pasma_ngml)) {
          checkLimits(patient$C_pasma_ngml, 0.1, 15, "Plasma", "ng/ml")
        } else if (!is.null(patient$C_pasma_pmolL)) {
          checkLimits(patient$C_pasma_pmolL, 5, 5000, "Plasma", "pmol/L")
        } else {
          paste("<span style=\"color:red\">No information provided</span>")
        }
      }
    } else {
      paste("<span style=\"color:red\">No information provided</span>")
    }
  })
  # Antibody testing information
  output$patient_details_antibody <- renderText({
    patient <- patient()
    if (patient$progression_to_insulin == "progressed") {
      if (is.na(patient$A)) {
        paste("<span style=\"color:red\">No information provided</span>")
      } else if (patient$A == 0) {
        paste("No evidence of high islet autoantibody levels")
      } else {
        paste("Evidence of high", paste(c("GAD", "IA2", "ZnT8")[which(unlist(patient[c("A_gad", "A_ia2", "A_znt8")]) == 1)], collapse = " / "), "islet autoantibody levels")
      }
    } else {
      paste("<span style=\"color:red\">No information provided</span>")
    }
  })
  
  
  #:---------------------------------------------------------------
  # Patient info (patient_cpep_antibody_info_panel)
  observeEvent(input$calculate, {
    show_panel <- input$insulin_treatment_select == "Within 6 months of diagnosis"
    output$patient_cpep_antibody_info_panel <- renderUI({
      if (show_panel == TRUE) {
        column(12,
               br(),
               column(6,
                      p("C-peptide:",
                        style="font-weight:bold;font-size:20px;"),
                      tags$head(tags$style("#patient_details_c_pep{font-size: 16px;}")),
                      htmlOutput("patient_details_c_pep")
               ),
               column(6,
                      p("Islet autoantibody status:",
                        style="font-weight:bold;font-size:20px;"),
                      tags$head(tags$style("#patient_details_antibody{font-size: 16px;}")),
                      htmlOutput("patient_details_antibody")
               )
        )
      } else {
        column(12)
      }
    })
  })
  # Additional information panel
  observeEvent(input$calculate, {
    patient <- patient()
    if (input$insulin_treatment_select == "Within 6 months of diagnosis") {
      show_panel <- TRUE
    } else {
      show_panel <- FALSE
    }
    if (show_panel == TRUE) {
      if (is.na(patient$C) & is.na(patient$A)) {
        # If C-pep and Anti are missing
        phrase <- paste("<b>As your patient is treated with insulin, they are required to have tested C-peptide and IA2/GAD islet autoantibodies prior to diagnostic molecular genetic testing for MODY.</b>")
      } else if (is.na(patient$C) & !is.na(patient$A)) {
        # If C-pep is missing and Anti isn't
        
        # If any antibody is positive
        if (sum(patient[c("A_ia2", "A_gad", "A_znt8")], na.rm = TRUE) > 0) {
          phrase <- paste("<b>As your patient is treated with insulin and tested positive for", paste(c("IA2", "GAD", "ZnT8")[which(patient[c("A_ia2", "A_gad", "A_znt8")] == 1)], collapse = " / "), "islet antoantibodies, they may not benefit from diagnostic molecular genetic testing for MODY since the probability of MODY is very low.</b>")
        } else {
          # If all antibodies are tested (but negative, implicit)
          if (sum(is.na(unlist(patient[c("A_ia2", "A_gad", "A_znt8")]))) == 0) {
            phrase <- paste("<b>As your patient is treated with insulin, they are required to have tested C-peptide prior to diagnostic molecular genetic testing for MODY.</b>")
          } else if (sum(is.na(unlist(patient[c("A_ia2", "A_gad")]))) == 2) {
            # Check if GAD and IA2 is missing
            phrase <- paste("<b>As your patient is treated with insulin, they are required to have tested C-peptide and IA2/GAD islet autoantibodies prior to diagnostic molecular genetic testing for MODY.</b>")
          } else if (sum(is.na(unlist(patient[c("A_ia2", "A_gad")]))) == 1) {
            # If GAD or IA2 is missing
            phrase <- paste("<b>As your patient is treated with insulin, they are required to have tested C-peptide and", paste(c("IA2", "GAD")[which(is.na(unlist(patient[c("A_ia2", "A_gad")])))], collapse = " / "), "islet autoantibodies prior to diagnostic molecular genetic testing for MODY.</b>")
          } else {
            phrase <- paste("<b>As your patient is treated with insulin, they are required to have tested C-peptide and may benefit from testing ZnT8 islet autoantibody prior to diagnostic molecular genetic testing for MODY.</b>")
          }
        }
      } else if (!is.na(patient$C) & is.na(patient$A)) {
        # If C-pep isn't missing but A is missing
        ## If C-pep = 1
        if (patient$C == 1) {
          phrase <- paste("<b>As your patient is treated with insulin, they are required to have tested IA2/GAD islet autoantibodies prior to diagnostic molecular genetic testing for MODY.</b>")
        } else {
          phrase <- paste("<b>As your patient is treated with insulin and has evidence of low insulin production, they may not benefit from diagnostic molecular genetic testing for MODY since the probability of MODY is very low.</b>")
        }
      } else {
        # If both tests are done
        if (patient$C == 0) {
          phrase <- paste("<b>As your patient is treated with insulin and has evidence of low insulin production, they may not benefit from diagnostic molecular genetic testing for MODY since the probability of MODY is very low.</b>")
        } else if (patient$C == 1) {
          # If any antibody is positive
          if (sum(patient[c("A_ia2", "A_gad", "A_znt8")], na.rm = TRUE) > 0) {
            phrase <- paste("<b>As your patient is treated with insulin and tested positive for", paste(c("IA2", "GAD", "ZnT8")[which(patient[c("A_ia2", "A_gad", "A_znt8")] == 1)], collapse = " / "), "islet antoantibodies, they may not benefit from diagnostic molecular genetic testing for MODY since the probability of MODY is very low.</b>")
          } else {
            # If all antibodies are tested (but negative, implicit)
            if (sum(is.na(unlist(patient[c("A_ia2", "A_gad", "A_znt8")]))) == 0) {
              # phrase <- paste("Your patient is treated with insulin, has evidence of insulin production and has tested negative for IA2/GAD/ZnT8 islet autoantibodies.")
              phrase <- paste("")
            } else if (sum(is.na(unlist(patient[c("A_ia2", "A_gad")]))) == 2) {
              # Check if GAD and IA2 is missing
              phrase <- paste("<b>As your patient is treated with insulin, they are required to have tested IA2/GAD islet autoantibodies prior to diagnostic molecular genetic testing for MODY.</b>")
            } else if (sum(is.na(unlist(patient[c("A_ia2", "A_gad")]))) == 1) {
              # If GAD or IA2 is missing
              phrase <- paste("<b>As your patient is treated with insulin, they are required to have tested", paste(c("IA2", "GAD")[which(is.na(unlist(patient[c("A_ia2", "A_gad")])))], collapse = " / "), "islet autoantibodies prior to diagnostic molecular genetic testing for MODY.</b>")
            } else {
              phrase <- paste("<b>As your patient is treated with insulin, they may benefit from testing ZnT8 islet autoantibody prior to diagnostic molecular genetic testing for MODY.</b>")
            }
          }
        }
      }
    } else {
      phrase <- paste0("")
    }
    phrase <- paste0("<font size='+1'>", phrase, "</font><br/>")
    if (!is.na(patient$C)) {
      if (patient$C == 0) {
        phrase <- paste0(phrase, "<font size='+1'>If C-peptide is <200pmol/L then the patient will have <0.1% chance of MODY.</font><br/>")
      }
    }
    if (!is.na(patient$A)) {
      if (patient$A == 1) {
        phrase <- paste0(phrase, "<font size='+1'>If the patient tests positive for islet autoantibodies, they will have a <0.1% chance of MODY.</font><br/>")
      }
    }
    # Test for C-peptide and antibodies if they are missing
    output$additional_information_panel <- renderUI({
      if (show_panel == TRUE) {
        column(12,
               br(),
               p("Additional information:",
                 style="font-weight:bold;font-size:24px;"),
               HTML(phrase)
        )
      } else {
        column(12)
      }
    })
    
  })
  
  
  #:--------------------------------------------------------------------
  
  # MODY calculator v2
  output$mody_cal_v2_text <- renderUI({
    
    # load patient information
    patient <- patient()
    
    if (patient$missingness == 1 ||
        patient$agedx < 1 || patient$agedx > 35 ||
        patient$bmi < 10 || patient$bmi > 70 ||
        patient$agerec < 1 || patient$agerec > 100 ||
        patient$hba1c_per < 3 || patient$hba1c_per > 15 ||
        patient$agerec < patient$agedx) {
      
      # Output message for logs
      print("Prediction attempt")
      
      return(HTML("Based on the clinical features entered into the calculator, the probability of your patient having MODY cannot be calculated."))
    }
    
    if (patient$progression_to_insulin == "progressed") {
      if (is.na(patient$C)) {
        if (is.na(patient$A)) {
          
          # Output message for logs
          print("Prediction attempt")
          
          return(HTML("Based on the clinical features entered into the calculator, the probability of your patient having MODY cannot be calculated."))
        } else if (patient$A == 0) {
          
          # Output message for logs
          print("Prediction attempt")
          
          return(HTML("Based on the clinical features entered into the calculator, the probability of your patient having MODY cannot be calculated."))
        } else if (patient$A == 1) {
          
        }
      } else if (patient$C == 0) {
        
      } else if (patient$C == 1) {
        if (is.na(patient$A)) {
          
          # Output message for logs
          print("Prediction attempt")
          
          return(HTML("Based on the clinical features entered into the calculator, the probability of your patient having MODY cannot be calculated."))
        } else if (patient$A == 0) {
          
        } else if (patient$A == 1) {
          
        }
      }
    }
    
    # Output message for logs
    print("Prediction made")
    
    
    # Calculator for those who progressed to insulin with 6 months
    if (patient$progression_to_insulin == "progressed") {
      # Add value of T based on C or A
      patient_prediction <- as_tibble(as.matrix(select(patient, pardm, agerec, hba1c_per, agedx, sex, bmi)))
      if (is.na(patient$C)) {
        if (is.na(patient$A)) {
          patient_prediction$T <- as.numeric(NA)
        } else if (patient$A == 0) {
          patient_prediction$T <- as.numeric(NA)
        } else if (patient$A == 1) {
          patient_prediction$T <- as.numeric(1)
        }
      } else if (patient$C == 0) {
        patient_prediction$T <- as.numeric(1)
      } else if (patient$C == 1) {
        if (is.na(patient$A)) {
          patient_prediction$T <- as.numeric(NA)
        } else if (patient$A == 0) {
          patient_prediction$T <- as.numeric(0)
        } else if (patient$A == 1) {
          patient_prediction$T <- as.numeric(1)
        }
      }
      
      # Agerec limit for T1D model
      patient_prediction$agerec <- ifelse(patient_prediction$agerec > 50, 50, patient_prediction$agerec)
      
      # Predict from calculator V2
      predictions_T1D <- predict(posterior_samples_T1D_obj, patient_prediction, rcs_parms) %>%
        apply(., 2, function(x) {
          data.frame(prob = mean(x), LCI = quantile(x, probs = 0.025), UCI = quantile(x, probs = 0.975))
        }) %>%
        bind_rows()
      # custom output text based on the probability
      if (predictions_T1D$prob < 0.001) {
        ## if the probability is less than 0.1% (0.001)
        
        showPageSpinner()
        Sys.sleep(0.5)
        
        # output
        prob_result <- paste0("<b><0.1%</b> (a 1 in ", sprintf((1/(predictions_T1D$prob)), fmt = "%#.1f"), " chance of having MODY)")
        
        hidePageSpinner()
        
        # output text
        return(HTML(paste("Based on the clinical features entered into the calculator, the probability of your patient having MODY is:", prob_result, sep = '<br/>')))
        
      } else {
        ## if the probability is less than 0.1% (0.001)
        
        showPageSpinner()
        Sys.sleep(0.5)
        
        # output
        prob_result <- paste0("<b>", sprintf(predictions_T1D$prob*100, fmt = "%#.1f"), "%</b> (a 1 in ", sprintf((1/(predictions_T1D$prob)), fmt = "%#.1f"), " chance of having MODY)")
        
        hidePageSpinner()
        
        # output text
        return(HTML(paste("Based on the clinical features entered into the calculator, the probability of your patient having MODY is:", prob_result, sep = '<br/>')))
        
      }
    } else {
      # Calculator for those who progressed to insulin after 6 months
      
      # Collect values of patient
      patient_prediction <- as_tibble(as.matrix(select(patient, pardm, agerec, hba1c_per, agedx, sex, bmi, insoroha)))
      # Predict from calculator V2
      predictions_T2D <- predict(posterior_samples_T2D_obj, patient_prediction) %>%
        apply(., 2, function(x) {
          data.frame(prob = mean(x), LCI = quantile(x, probs = 0.025), UCI = quantile(x, probs = 0.975))
        }) %>%
        bind_rows
      # custom output text based on the probability
      if (predictions_T2D$prob < 0.001) {
        ## if the probability is less than 0.1% (0.001)
        
        showPageSpinner()
        Sys.sleep(0.5)
        
        # output
        prob_result <- paste0("<b><0.1%</b> (a 1 in ", sprintf((1/(predictions_T2D$prob)), fmt = "%#.1f"), " chance of having MODY)")
        
        hidePageSpinner()
        
        # output text
        return(HTML(paste("Based on the clinical features entered into the calculator, the probability of your patient having MODY is:", prob_result, sep = '<br/>')))
        
      } else {
        ## if the probability is less than 0.1% (0.001)
        
        showPageSpinner()
        Sys.sleep(0.5)
        
        # output
        prob_result <- paste0("<b>", sprintf(predictions_T2D$prob*100, fmt = "%#.1f"), "%</b> (a 1 in ", sprintf((1/(predictions_T2D$prob)), fmt = "%#.1f"), " chance of having MODY)")
        
        hidePageSpinner()
        
        # output text
        return(HTML(paste("Based on the clinical features entered into the calculator, the probability of your patient having MODY is:", prob_result, sep = '<br/>')))
        
      }
    }
    
    
    # this is here just to check whether there was an error with the calculator
    return(HTML("Nothing coded"))
    
    
    # hidePageSpinner()
  })
  
  
}

#:--------------------------------------------------------------------------------
# Run the Shiny APP
shinyApp(ui, server)

