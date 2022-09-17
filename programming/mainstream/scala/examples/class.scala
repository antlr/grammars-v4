package  xxx.yy.zz.aa.bb.commons.transformation
import  xxx.yy.zz.aa.bb.commons.constants.Forms
import  xxx.yy.zz.aa.core.configuration.KingTransformation
import  xxx.yy.zz.aa.core.engine.ProcessStatus
import xxx.yy.zz.aa.core.transformation.TransformationTrait
import org.apache.spark.sql.{DataFrame, Row, functions}
import org.apache.spark.sql.functions._

class AddingHoldingFlag1(val transformation: KingTransformation) extends TransformationTrait{
  def runTransformation: DataFrame = {
    val previousDF = ProcessStatus.components(transformation.previous.get(0))
    val outPutDataframe = addingHoldingFlag1(previousDF)
    outPutDataframe
  }
  def addingHoldingFlag1(previousDF: DataFrame): DataFrame = {
    // adding holding flag field when a customer has at least one account valid and is in scope for CS
    val addingFlag = previousDF
      .withColumn(Forms.HOLD_1, when(col(Forms.capamer).contains("WER") && col(Forms.FLCOUNT) === 1, lit("YES"))
      .otherwise(lit("NO")))
    addingFlag
    }

  def concatTest() = {
    val a = "test" + "test" + "test"
    val schema = StructType(Seq( StructField(Fields.Alpha + "_trc", StringType), StructField(Fields.Beta + "_trc", DecimalType(10,0)), StructField(Fields.Gamma, StringType) ))
  }
}