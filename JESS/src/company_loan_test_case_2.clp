;Global Variables
(defglobal ?*g_years* = 3)
(defglobal ?*g_rt* = 25)
(defglobal ?*g_comp* = 10)
(defglobal ?*g_prodbase* = 3)
(defglobal ?*g_cr* = 2.5)
(defglobal ?*g_qk* = 2)
(defglobal ?*g_de* = 20)
(defglobal ?*g_ic* = 3)


;Template Definition
(deftemplate Company_Credit_Applicant
    (slot company_name)

    ;1. Number of years in the industry (INTEGER)
    (slot n_years (type INTEGER))

    ;2. Percentage Receivables Turnover in 1 Month (INTEGER)
    (slot n_rtTime (type FlOAT))

    ;3. Number of Competitors in the Industry (INTEGER)
    (slot n_comp (type INTEGER))

    ;4. Product Base (INTEGER)
    (slot n_prodBase (type INTEGER))

    ;5. Creditworthiness of Stakeholders (ALLOWED VALUES)
    (slot av_credWorth (allowed-values High Medium Low))

    ;6. Product Demand of the Overall Industry (ALLOWED VALUES)
    (slot av_demand (allowed-values High Medium Low))

    ;7. Current Ratio of the Company (FLOAT)
    (slot f_curRatio (type FlOAT))

    ;8. Quick Ratio of the Company (FLOAT)
    (slot f_qckRatio (type FlOAT))

	;9. Debt-Equity Financing Ratio Growth Percentage of the Company (INTEGER)
    (slot f_deRatio (type INTEGER))    

    ;10. Interest-Coverage Ratio of the Company (FLOAT)
    (slot f_icRatio (type FlOAT))   
    )

;Initial Menu
(defrule CreditWorthInit
    (declare (salience 100))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (printout t crlf)
    (printout t "Your Company's Credit Worthiness is being Assessed" crlf)
)


;Printing the Current Input

 (defrule PrintInput
    (declare (salience 99))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (printout t "Company "?Company_Credit_Applicant.company_name " under review for credit worthiness." crlf)

    (printout t "Number of Years in Business: " ?Company_Credit_Applicant.n_years " years" crlf)
    (printout t "Receivable Turnover Time: " ?Company_Credit_Applicant.n_rtTime " Weeks" crlf)
    (printout t "Number of Competitors in the Industry: " ?Company_Credit_Applicant.n_comp crlf)
    (printout t "Product Base: " ?Company_Credit_Applicant.n_prodBase crlf)
    (printout t "Credit Worthiness of the Stakeholders: " ?Company_Credit_Applicant.av_credWorth crlf)
    (printout t "Demand for the Sector: " ?Company_Credit_Applicant.av_demand crlf)
    (printout t "Current Ratio of the Company: " ?Company_Credit_Applicant.f_curRatio crlf)
    (printout t "Quick Ratio of the Company: " ?Company_Credit_Applicant.f_qckRatio crlf)
    (printout t "Debt-Equity Ratio Growth Percentage of the Company: " ?Company_Credit_Applicant.f_deRatio crlf)
    (printout t "Interest-Coverage Ratio of the Company: " ?Company_Credit_Applicant.f_icRatio crlf)
  	)


;Suggestions from the JESS System
(defrule CreditWorthSuggestions
    (declare (salience 98))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (printout t crlf)
    (printout t "Here are some insights: " crlf)
    )


; Rule 1: Check whether the company has been around for a substantial period of time
(defrule YearsCheck
    (declare (salience 97))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (< ?Company_Credit_Applicant.n_years ?*g_years*) then
        (printout t "The company has not existed long enough to be eligible for a credit." crlf)
        )
    )


; Rule 2: Percentage Receivables Turnover in 1 Month is good enough
(defrule RTCheck
    (declare (salience 96))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (< ?Company_Credit_Applicant.n_rtTime ?*g_rt*) then
        (printout t "The company's receivable turnaround time is not good enough." crlf)
        )
    )


; Rule 3: Check whether the company's competitors are too many
(defrule CompCheck
    (declare (salience 95))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (> ?Company_Credit_Applicant.n_comp ?*g_comp*) then
        (printout t "The company has too much competition." crlf)
        )
    )


; Rule 4: Check whether the company has a large enough product base
(defrule ProdBaseCheck
    (declare (salience 94))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (< ?Company_Credit_Applicant.n_prodBase ?*g_prodbase*) then
        (printout t "The company does not have enough product base." crlf)
        )
    )

; Rule 5: Check whether the company's stakeholders have a good creditworthiness
(defrule CredWorthCheck
    (declare (salience 93))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (= ?Company_Credit_Applicant.av_credWorth Low) then
        (printout t "The company's stakeholders don't have a good creditworthiness." crlf)
        )
    )

; Rule 6: Check whether the industry has a good demand
(defrule DemandCheck
    (declare (salience 92))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (= ?Company_Credit_Applicant.av_demand Low) then
        (printout t "The Industry is not doing well." crlf)
        )
    )

; Rule 7: Check whether the current ratio of the company is good
(defrule CRCheck
    (declare (salience 91))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (< ?Company_Credit_Applicant.f_curRatio ?*g_cr*) then
        (printout t "Current Ratio of the Company is not Good Enough" crlf)
        )
    )

; Rule 8: Check whether the quick ratio of the company is good
(defrule QKCheck
    (declare (salience 90))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (< ?Company_Credit_Applicant.f_qckRatio ?*g_qk*) then
        (printout t "Quick Ratio of the Company is not Good Enough" crlf)
        )
    )

; Rule 9: Check whether the debt-equity ratio of the company is good
(defrule DECheck
    (declare (salience 89))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (> ?Company_Credit_Applicant.f_deRatio ?*g_de*) then
        (printout t "Debt-Equity Finance Ratio Growth Percentage of the Company is high" crlf)
        )
    )

; Rule 10: Check whether the interest-coverage ratio of the company is good
(defrule ICCheck
    (declare (salience 88))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (< ?Company_Credit_Applicant.f_icRatio ?*g_ic*) then
        (printout t "Interest-Coverage Ratio of the Company is not Good Enough" crlf)
        )
    )


; Rule 11: Final Decision

(defrule FinalDecision
    (declare (salience 87))
    ?x <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (or (< ?x.f_curRatio ?*g_cr*) (< ?x.f_qckRatio ?*g_qk*) (> ?x.f_deRatio ?*g_de*) (< ?x.f_icRatio ?*g_ic*) (= ?x.av_credWorth Low) (< ?x.n_prodBase ?*g_prodbase*)) then
        (printout t " " crlf)
        (printout t "The company's credit worthiness is not good enough. Please reject the loan application." crlf)

    else
        (printout t " " crlf)
        (printout t "The company's credit worthiness is good. Please sanction the loan." crlf)
        )
    )

;Get User Input
(assert (Company_Credit_Applicant (company_name "abc_company")
            (n_years 3) (n_rtTime 30) (n_comp 6) (n_prodBase 5)
            (av_credWorth Medium)  (av_demand High)  
            (f_curRatio 2.7) (f_qckRatio 2.1)  (f_deRatio 15)  (f_icRatio 3.3)))
(run)





