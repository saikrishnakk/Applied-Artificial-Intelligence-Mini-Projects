(import nrc.fuzzy.*)

(import nrc.fuzz.jess.*)

(load-package nrc.fuzzy.jess.FuzzyFunctions)

;Global Variables
(defglobal ?*g_years* = 3)
(defglobal ?*g_rt* = 25)
(defglobal ?*g_comp* = 10)
(defglobal ?*g_prodbase* = 3)
(defglobal ?*g_cr* = 2.5)
(defglobal ?*g_qk* = 2)
(defglobal ?*g_de* = 20)
(defglobal ?*g_ic* = 3)
(defglobal ?*g_legal* = (new FuzzyVariable "legal" 0 10))
(defglobal ?*g_ind_tr* = (new FuzzyVariable "ind" 0 1000000))
(defglobal ?*g_st_health* = (new FuzzyVariable "st" 0 10))
(defglobal ?*g_emp_happy* = (new FuzzyVariable "emp" 0 10))
(defglobal ?*g_n_banks* = (new FuzzyVariable "bank" 0 10))
(call nrc.fuzzy.FuzzyValue setMatchThreshold 0.1)


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
    
    ;11. Number of Legal Cases in Proceeding (INTEGER)
    (slot n_legal (type INTEGER))  
    
    ;12. Turn-Over of the Industry (INTEGER)
    (slot n_indTurnOver (type INTEGER)) 
    
    ;13. Stock Health (INTEGER)
    (slot n_stHealth (type INTEGER)) 
    
    ;14. Employee Happiness (INTEGER)
    (slot n_empHappiness (type INTEGER)) 
    
    ;15. Number of Banks willing to give Loans (INTEGER)
    (slot n_banks (type INTEGER))    
    )


;Fuzzy Template
(deftemplate legal_cases
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate industry_turnover
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate stock_health
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate emp_happy
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate banks
    "Auto-generated"
    (declare (ordered TRUE)))



;Rule 1
(defrule MAIN::init-FuzzyVariables
    (declare (salience 101))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (call ?*g_legal* addTerm "low" (new ZFuzzySet 0 2))
    (?*g_legal* addTerm "medium" (new TrapezoidFuzzySet 3 4 5 6))
    (?*g_legal* addTerm "high" (new SFuzzySet 7 10))
    (?*g_ind_tr* addTerm "low" (new ZFuzzySet 0 200000))
    (?*g_ind_tr* addTerm "medium" (new TrapezoidFuzzySet 300000 400000 500000 600000))
    (?*g_ind_tr* addTerm "high" (new SFuzzySet 700000 1000000))
    (?*g_st_health* addTerm "low" (new ZFuzzySet 0 3))
    (?*g_st_health* addTerm "medium" (new TrapezoidFuzzySet 3 4 5 6))
    (?*g_st_health* addTerm "high" (new SFuzzySet 7 10))
    (?*g_emp_happy* addTerm "low" (new ZFuzzySet 0 3))
    (?*g_emp_happy* addTerm "medium" (new TrapezoidFuzzySet 3 4 5 6))
    (?*g_emp_happy* addTerm "high" (new SFuzzySet 7 10))
    (?*g_n_banks* addTerm "low" (new ZFuzzySet 0 5))
    (?*g_n_banks* addTerm "high" (new SFuzzySet 5 10))
    (assert (legal_cases (new FuzzyValue ?*g_legal* (new SingletonFuzzySet ?Company_Credit_Applicant.n_legal))))
    (assert (industry_turnover (new FuzzyValue ?*g_ind_tr* (new SingletonFuzzySet ?Company_Credit_Applicant.n_indTurnOver))))
    (assert (stock_health (new FuzzyValue ?*g_st_health* (new SingletonFuzzySet ?Company_Credit_Applicant.n_stHealth))))
    (assert (emp_happy (new FuzzyValue ?*g_emp_happy* (new SingletonFuzzySet ?Company_Credit_Applicant.n_empHappiness))))
    (assert (banks (new FuzzyValue ?*g_emp_happy* (new SingletonFuzzySet ?Company_Credit_Applicant.n_banks))))      
    )

;Rule 2 Initial Menu
(defrule CreditWorthInit
    (declare (salience 100))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (printout t crlf)
    (printout t "Your Company's Credit Worthiness is being Assessed" crlf)
)


;Rule 3 Printing the Current Input

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
    (printout t "Number of Legal Proceedings on the Company: " ?Company_Credit_Applicant.n_legal crlf)
    (printout t "Turnover of the Industry: " ?Company_Credit_Applicant.n_indTurnOver crlf)
    (printout t "Stock Health of the Company: " ?Company_Credit_Applicant.n_stHealth crlf)
    (printout t "Employee Happiness of the Company: " ?Company_Credit_Applicant.n_empHappiness crlf)
    (printout t "Number of other banks willing to give loans: " ?Company_Credit_Applicant.n_banks crlf)
  	)


;Rule 4 Suggestions from the JESS System
(defrule CreditWorthSuggestions
    (declare (salience 98))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (printout t crlf)
    (printout t "Here are some insights: " crlf)
    )


; Rule 5: Check whether the company has been around for a substantial period of time
(defrule YearsCheck
    (declare (salience 97))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (< ?Company_Credit_Applicant.n_years ?*g_years*) then
        (printout t "The company has not existed long enough to be eligible for a credit." crlf)
        )
    )


; Rule 6: Percentage Receivables Turnover in 1 Month is good enough
(defrule RTCheck
    (declare (salience 96))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (< ?Company_Credit_Applicant.n_rtTime ?*g_rt*) then
        (printout t "The company's receivable turnaround time is not good enough." crlf)
        )
    )


; Rule 7: Check whether the company's competitors are too many
(defrule CompCheck
    (declare (salience 95))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (> ?Company_Credit_Applicant.n_comp ?*g_comp*) then
        (printout t "The company has too much competition." crlf)
        )
    )


; Rule 8: Check whether the company has a large enough product base
(defrule ProdBaseCheck
    (declare (salience 94))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (< ?Company_Credit_Applicant.n_prodBase ?*g_prodbase*) then
        (printout t "The company does not have enough product base." crlf)
        )
    )

; Rule 9: Check whether the company's stakeholders have a good creditworthiness
(defrule CredWorthCheck
    (declare (salience 93))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (= ?Company_Credit_Applicant.av_credWorth Low) then
        (printout t "The company's stakeholders don't have a good creditworthiness." crlf)
        )
    )

; Rule 10: Check whether the industry has a good demand
(defrule DemandCheck
    (declare (salience 92))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (= ?Company_Credit_Applicant.av_demand Low) then
        (printout t "The Industry is not doing well." crlf)
        )
    )

; Rule 11: Check whether the current ratio of the company is good
(defrule CRCheck
    (declare (salience 91))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (< ?Company_Credit_Applicant.f_curRatio ?*g_cr*) then
        (printout t "Current Ratio of the Company is not Good Enough" crlf)
        )
    )

; Rule 12: Check whether the quick ratio of the company is good
(defrule QKCheck
    (declare (salience 90))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (< ?Company_Credit_Applicant.f_qckRatio ?*g_qk*) then
        (printout t "Quick Ratio of the Company is not Good Enough" crlf)
        )
    )

; Rule 13: Check whether the debt-equity ratio of the company is good
(defrule DECheck
    (declare (salience 89))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (> ?Company_Credit_Applicant.f_deRatio ?*g_de*) then
        (printout t "Debt-Equity Finance Ratio Growth Percentage of the Company is high" crlf)
        )
    )

; Rule 14: Check whether the interest-coverage ratio of the company is good
(defrule ICCheck
    (declare (salience 88))
    ?Company_Credit_Applicant <- (Company_Credit_Applicant (company_name ?company_name))
    =>
    (if (< ?Company_Credit_Applicant.f_icRatio ?*g_ic*) then
        (printout t "Interest-Coverage Ratio of the Company is not Good Enough" crlf)
        )
    )


;Rule 17
(defrule LegalCheck1
    (declare (salience 87))
    (legal_cases ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "low"))
    =>
        (printout t "Company has a low number of legal cases" crlf)
    )


;Rule 18
(defrule LegalCheck2
    (declare (salience 87))
    (legal_cases ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "medium"))
    =>
        (printout t "Company has a medium number of legal cases" crlf)
        )

;Rule 19
(defrule LegalCheck3
    (declare (salience 87))
    (legal_cases ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "high"))
    =>
        (printout t "Company has a high number of legal cases" crlf)
        )



;Rule 20
(defrule IndustryTurnOver1
    (declare (salience 86))
    (industry_turnover ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "low"))
    =>
        (printout t "Industry has a low turnover" crlf)
    )


;Rule 21
(defrule IndustryTurnOver2
    (declare (salience 86))
    (industry_turnover ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "medium"))
    =>
        (printout t "Industry has a medium turnover" crlf)
        )

;Rule 22
(defrule IndustryTurnOver3
    (declare (salience 86))
    (industry_turnover ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "high"))
    =>
        (printout t "Industry has a high turnover" crlf)
        )

;Rule 23
(defrule StockHealth1
    (declare (salience 85))
    (stock_health ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "low"))
    =>
        (printout t "Company has a low stock health" crlf)
    )


;Rule 24
(defrule StockHealth2
    (declare (salience 85))
    (stock_health ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "medium"))
    =>
        (printout t "Company has a medium stock health" crlf)
    )

;Rule 25
(defrule StockHealth3
    (declare (salience 85))
    (stock_health ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "high"))
    =>
        (printout t "Company has a high stock health" crlf)
    )

;Rule 26
(defrule EmpHappy1
    (declare (salience 84))
    (emp_happy ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "low"))
    =>
        (printout t "Company has a low employee happiness" crlf)
    )

;Rule 27
(defrule EmpHappy2
    (declare (salience 84))
    (emp_happy ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "medium"))
    =>
        (printout t "Company has a medium employee happiness" crlf)
    )

;Rule 28
(defrule EmpHappy3
    (declare (salience 84))
    (emp_happy ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "high"))
    =>
        (printout t "Company has a high employee happiness" crlf)
    )

;Rule 29
(defrule bank1
    (declare (salience 83))
    (banks ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "low"))
    =>
        (printout t "Company has a low number of banks offering loans" crlf)
    )

;Rule 30
(defrule bank2
    (declare (salience 83))
    (banks ?Company_Credit_Applicant&:(fuzzy-match ?Company_Credit_Applicant "high"))
    =>
        (printout t "Company has a high number of banks offering loans" crlf)
    )

; Rule 31: Final Decision
(defrule FinalDecision
    (declare (salience 85))
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
            (n_years 1) (n_rtTime 20) (n_comp 20) (n_prodBase 1) (n_legal 1) (n_indTurnOver 900000)
            (n_stHealth 8) (n_empHappiness 10) (n_banks 9) (av_credWorth Low)  (av_demand Medium)  
            (f_curRatio 1) (f_qckRatio 1)  (f_deRatio 1)  (f_icRatio 1)))
(run)




