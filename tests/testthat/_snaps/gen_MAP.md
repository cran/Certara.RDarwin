# Specific mapping of A1 when A1 is main absorption compartment (Single Model)

    {
      "type": "character",
      "attributes": {},
      "value": ["##MAP    A1 = DOSE_AMT_A1 CObs = CONC_OBS id = SUBJ_ID time = TIME_VAL"]
    }

# Specific A1 mapping takes precedence over AMT when A1 is main (Single Model)

    {
      "type": "NULL"
    }

# AMT maps to main dosepoint 'Aa' when Aa not specifically mapped (Single Model)

    {
      "type": "character",
      "attributes": {},
      "value": ["##MAP    Aa = DOSE_FOR_MAIN_COMP CObs = CONC_OBS id = SUBJ_ID time = TIME_VAL"]
    }

# Secondary dosepoint A1 mapped specifically, AMT maps to main Aa (Single Model)

    {
      "type": "character",
      "attributes": {},
      "value": ["##MAP    Aa = DOSE_FOR_MAIN_AA A1 = DOSE_FOR_SECONDARY_A1 CObs = CONC_OBS id = SUBJ_ID time = TIME_VAL"]
    }

# AMT and Specific A1/Ext mapping (Multiple Models, different mains)

    NULL

---

    {
      "type": "NULL"
    }

# Generic 'Rate' maps to MainDosepoint_Rate when AMT is used (Single Model)

    [1] "##MAP    Aa = DOSE_MAIN Aa_Rate = INF_RATE CObs = CONC_OBS id = SUBJ_ID time = TIME_VAL"

# Generic 'Duration' maps to MainDosepoint_Duration when AMT is used (Single Model)

    [1] "##MAP    Aa = DOSE_MAIN Aa_Duration = INF_DUR CObs = CONC_OBS id = SUBJ_ID time = TIME_VAL"

# Specific 'A1_Rate' maps correctly when A1 is main (Single Model)

    [1] "##MAP    A1 = DOSE_A1 A1_Rate = RATE_FOR_A1 CObs = CONC_OBS id = SUBJ_ID time = TIME_VAL"

# Specific 'A1_Duration' maps correctly when A1 is main (Single Model)

    [1] "##MAP    A1 = DOSE_A1 A1_Duration = DUR_FOR_A1 CObs = CONC_OBS id = SUBJ_ID time = TIME_VAL"

# Mixed generic/specific Rate/Duration with multiple models and Ext mapped

    NULL

---

    {
      "type": "NULL"
    }

# Unmapped covariate 'WT' is auto-mapped if data column exists (Single Model)

    NULL

# Cat Cov (Named) generates correct MAP block

    [1] "##MAP  Sex=Gender(Male = 0, Female = 1)  A1 = Amount CObs = Conc id = Subject time = Act_Time"

# Cat Cov (Unnamed/Map Def) generates correct MAP block

    [1] "##MAP  Sex=Gender(female = 0, male = 1)  A1 = Amount CObs = Conc id = Subject time = Act_Time"

# Occ Cov (Named) generates correct MAP block

    [1] "##MAP  Period=StudyPeriod(Period1 = 1, Period2 = 2)  A1 = Dose CObs = CObs id = ID time = time"

# Occ Cov (Unnamed/Map Def) generates correct MAP block

    [1] "##MAP  Period=StudyPeriod(P1=1, P2=2)  A1 = Dose CObs = CObs id = ID time = time"

# Continuous Cov and IGNORE generate correct MAP block

    [1] "##MAP  Age=SubjectAge  A1 = Dose CObs = CObs id = ID time = time IGNORE = UnusedColumn"

# gen_MAP warns when model's named categories are overridden by mapping definition

    [1] "##MAP  Sex=Gender(M=0, F=1)  A1 = AMT CObs = DV id = ID time = TIME"

---

    [1] "##MAP  Sex=Gender_COL(M=0, F=1)  A1 = AMT_COL CObs = DV_COL id = ID time = TIME"

