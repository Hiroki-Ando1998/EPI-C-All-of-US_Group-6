# Mold exposure as a driver of inflammatory disease

## 1. Specific Aim
To investigate the association between mold exposure and inflammatory diseases, we propose three specific aims: 

**Aim 1**: Explore the association between mold exposure and incident inflammatory diseases, including rheumatoid arthritis,  psoriasis, and multiple sclerosis, among adult participants in the All of Us Research Program. Findings from this aim will provide foundational evidence for the role of mold as a potential environmental factor that increases the risk of inflammatory disease.

**Aim 2**: Identify demographic and social determinants of health that modify the association between mold exposure and incident inflammatory diseases. Findings will characterize populations that may be particularly vulnerable to the potential health effects of mold exposure.

**Aim 3**: Assess regional variation in the association between mold exposure and inflammatory diseases across four U.S. regions: the Pacific Northwest(), the Southwest (California, Arizona, and Texas), South (Florida, Georgia, and Alabama), Midwest (Michigan, Illinois, and Wisconsin), and Northeast (New York, Pennsylvania, and Massachusetts). States were selected to avoid borderline regions, under the assumption that they are representative of each respective region.  Results will provide insight into geographic disparities and environmental conditions that contribute to disease risk.

## 2.Methods
We conduct a retrospective cohort study using data from the All of Us Research Program. The study population consists of participants aged ≥18 years residing in the Southwest, South, Midwest, or the Northeast of the US. Participants with missing information on either the exposure or outcome are excluded from the analytic population. The primary outcome is inflammatory disease, specifically rheumatoid arthritis, psoriasis, and multiple sclerosis, which is defined using phecodes. The primary exposure is household mold exposure, which is assessed using self-reported information from the Social Factors Survey. The mold exposure level is corroborated using proxy measures, including housing conditions, water leaks, and potentially indoor air quality, as well as environmental data from the U.S. Environmental Protection Agency. Potential confounding variables and effect modifiers will be identified based on previous literature. Potential confounders include age, sex, drinking, smoking, and education. Potential effect modifiers include income, health insurance coverage, housing type, and neighborhood condition.

#### 2-A: Cohort 
We defined the study cohort within the All of Us Researcher Workbench using the following eligibility criteria 
[All of US Data Browser](https://databrowser.researchallofus.org/?_gl=1*1msoqag*_ga*MTEzOTQwMzQzMS4xNzg3OTMwMjA0*_ga_MQVR5DG2C4*czE3ODc5NTI1NTYkbzIkZzEkdDE3ODc5NTI2MDQkajEyJGwwJGgyNzc3OTI2MDc):
- Visit (inpatient or outpatinet visits) at least twice
- Social Determinants Of Health: Think about the place you live. Do you have problems with any of the following? 
- Condition: Psoriasis Occurrence count: Greater than or equals 2
- Condition: Rheumatoid arthritis Occurrence count: Greater than or equals 2
- Condition: Multiple sclerosis Occurrence count: Greater than or equals 2
- Basic: What was your biological sex at birth?
- Basic: What is the highest grade or year of school you completed_
- Basic: Are you coververed by health insurance or some other kind of health care plan?
- Basic: What is your current employment status?
- Basic: What is your annual household income from all sources?
- Basic: Do you own or rent the place where you live?
- Current age
- Observation: Postal code and Tabacco smoking status
- Personal And Family Health History: Including yourself, who in your family has had multiple sclerosis (MS)?
- Personal And Family Health History: Including yourself, who in your family has had rheumatoid arthritis (RA)?

After collecting the data, we established the study cohort using R propgramming
[R-code_cohort](https://databrowser.researchallofus.org/?_gl=1*1msoqag*_ga*MTEzOTQwMzQzMS4xNzg3OTMwMjA0*_ga_MQVR5DG2C4*czE3ODc5NTI1NTYkbzIkZzEkdDE3ODc5NTI2MDQkajEyJGwwJGgyNzc3OTI2MDc):.
1. We included individuals in the study cohort who visited the EHS at least twice **and** answered the survey question before 2024: 'Think about the place you live. Do you have problems with any of the following?
2. From this cohort, we included **only** individuals who answered all six basic questions and provided information on postal code and current age.
3. **Cases** were defined as individuals within the established cohort who had been diagnosed with psoriasis, rheumatoid arthritis, or multiple sclerosis.
4. **Controls** were defined as individuals with no recorded diagnosis of psoriasis, rheumatoid arthritis, or multiple sclerosis in the EHS system.
5. 




