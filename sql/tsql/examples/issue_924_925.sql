CREATE FUNCTION [Report].[DataSet]
    (
      @Monat TINYINT ,
      @Jahr SMALLINT
    )
RETURNS @xxx TABLE
    (
      [AUSLASTUNGSBETRAG_STICHTAG] DECIMAL(28, 4) ,
      [AUSLASTUNGSBETRAG_VERGLEICHSSTICHTAG] DECIMAL(28, 4) ,
      RATINGNOTE INT
    )
AS
    BEGIN
        INSERT  INTO @xxx
                ( AUSLASTUNGSBETRAG_STICHTAG ,
                  AUSLASTUNGSBETRAG_VERGLEICHSSTICHTAG ,
                  RATINGNOTE
                )
                SELECT  [AUSLASTUNGSBETRAG_STICHTAG] = SUM(Stichtage.[Auslastungsbetrag zum Stichtag]) ,
                        [AUSLASTUNGSBETRAG_VERGLEICHSSTICHTAG] = SUM(Stichtage.[Auslastungsbetrag zum Vergleichsstichtag]) ,
                        RATINGNOTE = ISNULL(CAST(RATING.RATINGNOTE_KURZ AS INT),
                                            -1)
                FROM aaa.bbb;
        RETURN;
    END;


