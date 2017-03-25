
library(tidyverse)
library(scales)
library(feather)

max.income = 600000

# Parameters (2016 tax brackets, rates etc.) ----

# personal exemptions
pe.thresh = list('Single' =	259400, 'Married' =	311300, 'hoh' = 285350)
pe = 4050

pe.ca = 111
pe.ca.dependent = 344
pe.ca.thresh = list('Single' = 182459, 'Married' = 364923, 'hoh' = 273692)

# AMT
amt.high.thresh = list('Single' = 186300, 'hoh' = 186300, 'Married' = 186300)
amt.phaseout.thresh = list('Single' = 119700,'hoh'= 119700, 'Married' = 159700)
amt.exemption = list('Single' = 53900, 'hoh' = 53900, 'Married' = 83800)

#medicare thresholds
medicare.thresh = list('Single' = 200000, 'Married' = 250000, 'hoh' = 200000)

# standard deductions
fed.std = list('Single' = 6300, 'Married' = 12600, 'hoh' = 9300)
ca.std = list('Single' = 4129, 'Married' = 8258, 'hoh' = 8258)
ca.std.dep = 1050

#federal rates
fed.rates = list(
  'Single' = list('thresh'=c(0,9275,37650,91150,190150,413350,415050),
                  'rate'=c(0.1,.15,.25,.28,.33,.35,.396)),
  'Married' = list('thresh'=c(0,18550,75300,151900,231450,413350,466950),
                   'rate'=c(0.1,.15,.25,.28,.33,.35,.396)),
  'hoh' = list('thresh'=c(0,13250,50400,130150,210800,413350,441000),
               'rate'=c(0.1,.15,.25,.28,.33,.35,.396))
)
#ca rates
ca.rates = list(
  'Single' = list('thresh'=c(0,8015,19001,29989,41629,52612,268750,322499,537498),
                 'rate'=c(0.01,.02,.04,.06,.08,.093,.103,.113,.123)),
  'hoh' = list('thresh'=c(0,16040,38003,48990,60630,71615,365499,438599,730997),
                 'rate'=c(0.01,.02,.04,.06,.08,.093,.103,.113,.123))
)
ca.rates$Married = list('thresh' = ca.rates$Single$thresh*2,
                    'rate' = ca.rates$Single$rate)

# Earned Income Tax Credit table
eic = read_feather('eic.feather')

# Child tax credit thresholds
ctc.thresh = list('Single' = 75000, 'Married' = 110000, 'hoh' = 75000)

# Helper Functions ----

get.eic = function(income, children, filing.status){
  fs = if_else(filing.status %in% c('Single','hoh'), 's','m')
  children = min(3, children)
  ix = paste0(fs,children)
  rows = floor(income/50)+1
  credit = eic[rows,ix][[1]]
  credit[is.na(credit)] = 0
  return(credit)
}

get.child.tax.credit = function(income, children, filing.status){
  thresh = ctc.thresh[[filing.status]]
  credit = min(3000, children * 1000)
  #decrease by $50 for every $1000 over thresh
  credit = credit - pmax(0,ceiling((income-thresh)/1000)*50)
  credit = pmax(credit, 0)
  return(credit)
}

get.tax = function(taxable.income, t.rates, marital.status) {
  rate = t.rates[[marital.status]]$rate
  thresh = t.rates[[marital.status]]$thresh
  tax = taxable.income * rate[1]
  for (i in 2:length(rate)) {
    ct = thresh[i]
    tax[taxable.income>ct] = tax[taxable.income>ct] +
      (rate[i]-rate[i-1])*(taxable.income[taxable.income>ct]-ct)
  }
  return(pmax(tax,0))
}

get.social.security = function(marital.status, income, income.ratio=1){
  if (marital.status == 'Married') {
    inc1 = income * income.ratio
    inc2 = income - inc1
    return(get.social.security('Single', inc1)+get.social.security('Single', inc2))
  }
  return(pmin(income*0.062, 7347))
}


extract.tax = function(marital.status, dependent.exemptions, 
                       income.ratio=1, m.income = max.income, 
                       steps = 250, min.income = 0, all = FALSE){
  data = get.data(marital.status, dependent.exemptions, 
                  income.ratio, m.income,
                  steps, min.income)
  if (all){
    return(gather(data))
  }
  return(as.numeric(data[1,'total.tax']))
}

# Compare married versus single ----
marriage.or.divorce = function(dependent.exemptions, income, income.ratio){
  
  married = extract.tax('Married', dependent.exemptions, income.ratio, income, 
                        1, min.income = income)

  inc1 = income * income.ratio
  inc2 = income - inc1
  
  hoh.single = if_else(dependent.exemptions > 0, 'hoh', 'Single')
  
  # assign the children to higher income partner, and then to low income
  # see which one is lower
  s1 = extract.tax(hoh.single, dependent.exemptions, m.income = inc1, steps = 1, min.income = inc1)+
    extract.tax('Single', 0, m.income = inc2, steps = 1, min.income = inc2)
  s2 = extract.tax(hoh.single, dependent.exemptions, m.income = inc2, steps = 1, min.income = inc2)+
    extract.tax('Single', 0, m.income = inc1, steps = 1, min.income = inc1)
  
  single = min(s1, s2)
  tax.diff = married - single
  return(tax.diff)
}

# Main Engine ----

get.data = function(marital.status, dependent.exemptions, 
                    income.ratio=1, m.income = max.income, 
                    steps = 250, min.income = 0){
  
  pept = pe.thresh[[marital.status]]
  med.t = medicare.thresh[[marital.status]]
  personal.exemptions = ifelse(marital.status == 'Married', 2, 1)
  money = data_frame(income = seq(from=min.income,to=m.income, length.out = steps))
  
  money = money %>% 
    mutate(ded.f.personal = (personal.exemptions+dependent.exemptions) * pe,
           ded.f.personal = ded.f.personal - 
             (ceiling(pmax(income - pe.thresh[[marital.status]],
                           0 )/2500)*ded.f.personal*0.02),
           ded.f.personal = pmax(ded.f.personal,0),
           ded.f.personal = pmin(income, ded.f.personal),
           
           ded.f.standard = pmin(fed.std[[marital.status]],income),
           
           credit.s.personal = pe.ca * personal.exemptions -
             (ceiling(pmax(income-pe.ca.thresh[[marital.status]],
                           0)/2500/personal.exemptions))*6,
           credit.s.dependent = pe.ca.dependent * dependent.exemptions -
             (ceiling(pmax(income-pe.ca.thresh[[marital.status]],
                           0)/2500/personal.exemptions))*6*dependent.exemptions,
           # credit.s.personal = pmax(0,credit.s.personal) + credit.s.dependent,
           credit.s.personal = pmax(credit.s.personal,0) + pmax(credit.s.dependent, 0),
           
           ded.s.standard = ca.std[[marital.status]],
           
           state.taxable = pmax(income  - ded.s.standard, 0),
           state.tax = get.tax(state.taxable, ca.rates, marital.status) - credit.s.personal,
           state.tax = pmax(0, state.tax),
           
           ded.f.itemized = state.tax,
           ded.f.itemized.20 = ded.f.itemized * 0.20,
           ded.f.itemized = ifelse(income > pept,
                                   ded.f.itemized - (income-pept) * 0.03,
                                   ded.f.itemized),
           ded.f.itemized = pmax(ded.f.itemized, ded.f.itemized.20, ded.f.standard),
           ded.f.itemized = pmin(ded.f.itemized, income),
           
           social.security = get.social.security(marital.status,income,income.ratio),
           medicare = income * 0.0145,
           medicare = ifelse( income > med.t, medicare + (income - med.t) * 0.009, medicare),
           
           federal.taxable = pmax(0, income - ded.f.itemized - ded.f.personal),
           
           federal.tax = get.tax(federal.taxable, fed.rates, marital.status),
           # amt.tax = get.amt(income, marital.status),
           
           amt.exempt = amt.exemption[[marital.status]],
           amt.exempt = ifelse(income > amt.phaseout.thresh[[marital.status]],
                               amt.exempt -
                                 (income - amt.phaseout.thresh[[marital.status]])/4,
                               amt.exempt),
           amt.exempt = pmax(0, amt.exempt),
           amt.income = pmax(0, income - amt.exempt),
           amt.tax = amt.income * 0.26 +
             ifelse( amt.income>amt.high.thresh[[marital.status]],
                     (amt.income-amt.high.thresh[[marital.status]])*.02,
                     0),
           amt.tax = pmax(0, amt.tax - federal.tax),
           
           
           eic = - get.eic(income, dependent.exemptions, marital.status),
           child.tax.credit = - get.child.tax.credit(income, 
                                                   dependent.exemptions, 
                                                   marital.status),
           
           total.tax = social.security + medicare + federal.tax + 
             state.tax + amt.tax + eic + child.tax.credit,
           carry.home = income - total.tax,
           
           total.tax.percent =  pmax(0,total.tax / income * 100),
           federal.percent = federal.tax / income * 100,
           state.percent = state.tax / income * 100,
           medicare.percent = medicare / income * 100,
           social.security.percent = social.security / income * 100,
           amt.percent = amt.tax / income * 100
    )
}