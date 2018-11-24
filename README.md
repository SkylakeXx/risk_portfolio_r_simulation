# risk_portfolio_r_simulation
An easy implementation of risk management in R:

This takes part of my master project, I will upload every project I think might be helpful for other master/engineering
students, in this case the project is quite simple:

I have a bunch of companies with the following data:

PD Default Probability / P
LGD Loss Given Default / % from EAD that losess if default
EAD Expexted Amount at Default / Total amount the company losses at default

Basically they ask me to run 1000 scenarios where most of the variables folloe a P(x) = N(0,1) distribution where

Wi is a unique value foreach scenario in every company
P is a fixex value 0.1 or 0.3
Y is a unique value for each scenario
Z = sqrt(p)*Y + sqrt(1-p)* W

if  Zi<qnorm(PDi) then the company collapses, get fun.
