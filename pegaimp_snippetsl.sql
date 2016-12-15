update USR_OTTO.sa_int_preds_nov16 set Revenue = 0 where Revenue is null;


insert into USR_OTTO.sa_int_preds select * from USR_OTTO.sa_int_preds_nov16;

TRUNCATE TABLE "USR_OTTO"."SA_INT_PREDS";


select count(IND_NOMINA_ULT1),IND_NOMINA_ULT1 from USR_ANALYTICS.KAGGLE_SANTANDER2 group by IND_NOMINA_ULT1;

UPDATE USR_ANALYTICS.KAGGLE_SANTANDER2 
SET IND_NOMINA_ULT1 = CASE WHEN IND_NOMINA_ULT1 = 'NA' THEN '' ELSE IND_NOMINA_ULT1 END;

