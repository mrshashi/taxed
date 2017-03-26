# taxed

This project lets you generate 2016 taxes for various incomes, filing status and number of children. Only CA and federal taxes are calculated. The shiny app is hosted at http://speakempirics.com/taxed/ 

There's also a blog post, that goes along with the shiny app: http://medium.com/what??

If you want to play with output of this engine, see marriage_or_divorce.Rmd for examples of how to call the get_data function. 

Things I know to be wrong about this calculator:
Both CA and Federal taxes use tax table look ups, rather than a calcultion for income up to 100k. I didn't do that, which can lead the actual tax to be off by several dollars. I didn't see any examples where it was off by more than $10. It can be fixed, but it would add to not a very big difference in outcome or understanding of tax policy.

