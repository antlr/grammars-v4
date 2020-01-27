package xxx.yyy.zzz.aaa.bbbb.commons.transformation
import java.math.BigDecimal
import xxx.yyy.zzz.aaa.bbbb.commons.constants.Fields
import xxx.yyy.zzz.aaa.bbbb.commons.utils.Functions._
import xxx.yyy.zzz.aaa.bbbb.configuration.SparktacusTransformation
import xxx.yyy.zzz.aaa.bbbb.engine.ProcessStatus
import xxx.yyy.zzz.aaa.bbbb.transformation.TransformationTrait
import org.apache.spark.sql.{DataFrame, Row, functions}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types.{Decimal, DecimalType, StringType, StructField, StructType}
class CapComer(val transformation: SparktacusTransformation) extends TransformationTrait{
  def runTransformation: DataFrame = {
    val previousDF = ProcessStatus.components(transformation.previous.get(0))
    val outPutDataframe = capComer(previousDF)
    outPutDataframe
  }
  def capComer(previousDF: DataFrame): DataFrame = {
    val capComer = previousDF.
      withColumn(Fields.cap_comer, when(col(Fields.Trel).isNull, col(Fields.capacc)).
        otherwise(when(col(Fields.Trel).isNotNull && col(Fields.capacc) === "", lit("ECE")).
          otherwise(when(col(Fields.Trel).isNotNull && col(Fields.capacc) === "ECE", lit("ECE")).
            otherwise(lit("ECE,ACR")))))
    val rcct = capComer.select(
      Fields.Tper,
      Fields.Cper,
      Fields.cap_comer
    ).map(x => ((x.getString(0), x.getDecimal(1)), x.getString(2)))
      .reduceByKey((a, b) => a + b)
      .mapPartitions(row => row.map {
        case ((tipo_per: String, cod_pers: BigDecimal), cap: String)
        =>
          if(cap.contains("ECE") && cap.contains("ACR")){
            Row.fromSeq(Seq(tipo_per, cod_pers, "ECE,ACR"))
          } else if (cap.contains("ECE") && !cap.contains("ACR")) {
            Row.fromSeq(Seq(tipo_per, cod_pers, "ECE"))
          } else if (!cap.contains("ECE") && cap.contains("ACR")) {
            Row.fromSeq(Seq(tipo_per, cod_pers, "ACR"))
          } else {
            Row.fromSeq(Seq(tipo_per, cod_pers, cap))
          }
      })
    val schema = StructType(Seq(
      StructField(Fields.Tper + "_crt", StringType),
      StructField(Fields.Cper + "_crt", DecimalType(10,0)),
      StructField(Fields.cap_comer, StringType)
    ))
    val rIaDF = ProcessStatus.hiveContext.createDataFrame(rcct, schema)
    val output = previousDF.join(rIaDF,
      previousDF(Fields.Tper) === rIaDF(Fields.Tper + "_crt") &&
        previousDF(Fields.Cper) === rIaDF(Fields.Cper + "_crt")
    )
      .drop(Fields.Tper + "_crt")
      .drop(Fields.Cper + "_crt")
    output
  }
}