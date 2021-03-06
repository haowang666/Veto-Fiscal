# Data summary

## For V-dem project
V-dem file is download from [stata-and-other]()


### Related to democracy
```{r}
# v2x_delibdem v2dlreason v2dlcommon v2dlcountr v2dlconslt v2dlengage

#Deliberative democracy index v2x-delibdem
#To what extend is deliberatuve democracy achieved? aggregated index
summary(mydata$v2x_delibdem)

#give reasoned justification of their positions on public policy
summary(mydata$v2dlreason)
#give reasoned justification of their positions on common good
summary(mydata$v2dlcommon)
#give respect for counterarguments
summary(mydata$v2dlcountr)
#range of consultation
summary(mydata$v2dlconslt)
#engaged society
summary(mydata$v2dlengage)
#-----------------------------------------------------------------
# v2xeg_eqprotec v2xeg_eqdr v2x_jucon v2xlg_legcon v2cscnsult
#equal democracy index
#equal protection of individual rights
summary(mydata$v2xeg_eqprotec)
#equal distribution of resource 
summary(mydata$v2xeg_eqdr)

#constraints on executive branches
#judicial constraints
summary(mydata$v2x_jucon)
#legislative constraints
summary(mydata$v2xlg_legcon)

#civil society organization participation index
#rountely consulted by policy maker
summary(mydata$v2cscnsult)

#---------------------------------------------------
#party system institutionalization
summary(mydata$v2xps_party)

#---------------------------------------------------
#legislative constraints 
#legislature questions officials in practice
summary(mydata$v2lgqstexp)
#legislature investigates in practice
summary(mydata$v2lginvstp)
#executive oversight: other body engages in the investigation of the executive branch
summary(mydata$v2lgotovst)

#--------------------------------------------------
#veto legislation by law (Elkins et al. 2012) CCP project
#summary(mydata$e_ccp_leg_in_1)

#--------------------------------------------------
#minimal competitive elections Skaaning et al. 2015
summary(mydata$e_competition)

#-------------------------------------------------
#Marshall and Jaggers 2013
#institutionalizaed democracy
summary(mydata$e_democ)
#institutionalized autocracy
summary(mydata$e_autoc)

#-------------------------------------------------
#institutional constraints as a whole
summary(mydata$e_h_polcon5)
```


### Related to corruption and clientelism

```{r corription}
#political corruption index-aggregated
summary(mydata$v2x_corr)
#public sector corruption index
summary(mydata$v2x_pubcorr)
#executure corruption index
summary(mydata$v2x_execorr)
#-------------------------------------------

#election vote buying 
summary(mydata$v2elvotbuy)
#voting irregularities
summary(mydata$v2elirreg)

#-------------------------------------------
#party linkage: from programmatic to clientelist 
summary(mydata$v2psprlnks)
#executure bribery and corruption exchange 
summary(mydata$v2exbribe)
#executive embezzlement and theft 
summary(mydata$v2exembez)
#public sector corrupt exchanges 
summary(mydata$v2excrptps)

#------------------------------------------
#on delivery of puclic goods and private goods 
#particularistic to puclic funded
summary(mydata$v2dlencmps)


#--------------------------------------------
#Cite: Kaufman et al. 2010
#world bank government efffectiveness
summary(mydata$e_wbgi_gee)

#world bank rule of law index
summary(mydata$e_wbgi_rle)

```


### Political Equality

```{r}
# equal power over socioeconomic conditions
summary(mydata$v2pepwrses)
# equal power over social groups 
summary(mydata$v2pepwrsoc)
```



### Related to Federalism and Decentralization 

```{r decentralization}
#regional government exist 
summary(mydata$v2elreggov)
#regional offices relative power 
summary(mydata$v2elrgpwr)

#local offices exists
summary(mydata$v2ellocgov)
#local offices relative power
summary(mydata$v2ellocpwr)

#division of power index (central-regional-local)
summary(mydata$v2x_feduni)

#Database of political institutions (Beck et al. 2001)
#state government authority over taxation
summary(mydata$e_dpi_author)

```


### Related to information and media

```{r}
#government censorship: media
summary(mydata$v2mecenefm)
#internet censorship
summary(mydata$v2mecenefi)

```


### Demographics (controls)

```{r}
#education: average year of education among citizens older than 15
summary(mydata$e_peaveduc)

#gdp per capita (logged)
summary(mydata$e_migdppcln)

#gdp growth
summary(mydata$e_migdpgro)

#income inequality: UNU-Wider 2008
summary(mydata$e_peginiwi)

#Oil reserves: Haber and Menaldo 2011
# in billions of barrels
summary(mydata$e_reserves_billions)
# Oil production per capita: Haber and Menaldo 2011
summary(mydata$e_Total_Oil_Income_PC)
# Government revenues from oil, gas and minerals
summary(mydata$e_Fiscal_Reliance)

#------------------------------------------------
#Armed conflicts International: Brecke 2001
summary(mydata$e_miinteco)
#Internal armed conflicts 
summary(mydata$e_miinterc)
#civil war: Haber and Menaldo 2011
summary(mydata$e_Civil_War)

```



## GSRE Part

### Percentage spent as of total expenditure
expend_security_EXP expenddefence_EXP exp_public_order_EXP wagessalaries_EXP subpentrans_EXP pensions_EXP total_welfare_EXP education_EXP health_EXP social_protection_EXP housing_EXP owelfarespend_EXP



### Percentage spent as of GDP
expend_security_GDPGSRE expenddefence_GDPGSRE exp_public_order_GDPGSRE wagessalaries_GDPGSRE subpentrans_GDPGSRE pensions_GDPGSRE total_welfare_GDPGSRE education_GDPGSRE health_GDPGSRE social_protection_GDPGSRE housing_GDPGSRE owelfarespend_GDPGSRE

### Raw expenditure
expend_security expenddefence exp_public_order wagessalaries subpentrans pensions total_welfare education health social_protection housing owelfarespend