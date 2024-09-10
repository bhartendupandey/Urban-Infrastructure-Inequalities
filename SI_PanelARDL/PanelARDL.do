import delimited "D:\Paper2\Data\OutputData\panel.csv"
gen lGDP=log(gdp_pc)
gen lsol = log(sol)
egen id = group(country)
xtset id year
ssc install xtpmg
xtsum inequality_wtn inequality_btwn lGDP lsol urbanization
corr lGDP lsol urbanization

//https://www.youtube.com/watch?v=4Gg2bFG4Bm0
//https://www.youtube.com/watch?v=VvXfgyhISB0
// Im-Pesaran-Shin unit-root test (for global sample) with constant and 1 lag:
// Panel Unit Root Tests using IPS (for assumption of heterogeneous slopes) and LLC (for assumption of homogeneous slopes)
// IPS test (for global sample) with constant and 1 lag:: non stationary when pvale> 0.05

xtunitroot ips inequality_wtn, lags(1)
xtunitroot ips inequality_btwn, lags(1)
xtunitroot ips lGDP, lags(1)
xtunitroot ips lsol, lags(1)
xtunitroot ips urbanization, lags(1)

// For non-stationary variables take first difference

xtunitroot ips d.inequality_wtn, lags(1) 
xtunitroot ips d.inequality_btwn, lags(1) 
xtunitroot ips d.lGDP, lags(1)
xtunitroot ips d.lsol, lags(1)


// For Between Inequality
xtpmg2 d.inequality_btwn d.urbanization d.lsol, lr(l.inequality_btwn urbanization lsol) ec(ECT) replace mg
xtpmg2 d(inequality_btwn urbanization lsol), lr(l.inequality_btwn urbanization  lsol) ec(ECT) replace pmg
xtpmg2 d(inequality_btwn urbanization lsol), lr(l.inequality_btwn urbanization  lsol) ec(ECT) replace dfe
hausman mg pmg,sigmamore
hausman DFE pmg,sigmamore

xtpmg2 d.inequality_btwn d.lGDP d.lsol, lr(l.inequality_btwn lGDP lsol) ec(ECT) replace mg
xtpmg2 d(inequality_btwn lGDP lsol), lr(l.inequality_btwn lGDP lsol) ec(ECT) replace pmg
xtpmg d(inequality_btwn lGDP lsol), lr(l.inequality_btwn lGDP lsol) ec(ECT) replace dfe
hausman mg pmg,sigmamore
hausman DFE pmg,sigmamore


// For within Inequality
xtpmg2 d.inequality_wtn d.urbanization d.lsol, lr(l.inequality_wtn urbanization lsol) ec(ECT) replace mg
xtpmg2 d(inequality_wtn urbanization lsol), lr(l.inequality_wtn urbanization  lsol) ec(ECT) replace pmg
xtpmg2 d(inequality_wtn urbanization lsol), lr(l.inequality_wtn urbanization  lsol) ec(ECT) replace dfe
hausman mg pmg,sigmamore
hausman dfe pmg,sigmamore

xtpmg2 d.inequality_wtn d.lGDP d.lsol, lr(l.inequality_wtn lGDP lsol) ec(ECT) replace mg
xtpmg2 d(inequality_wtn lGDP lsol), lr(l.inequality_wtn lGDP lsol) ec(ECT) replace pmg
xtpmg d(inequality_wtn lGDP lsol), lr(l.inequality_wtn lGDP lsol) ec(ECT) replace dfe
hausman mg pmg,sigmamore
hausman dfe pmg,sigmamore