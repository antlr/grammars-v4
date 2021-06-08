package uuu.vvv.www.xxx.yyy.zzzz
import java.math.BigDecimal
import org.apache.spark.sql.hive.HiveContext

object CreateTable extends CreateTableInterface {
  val a =
    """
       Test
       Test
    """
  val b =
    """
      |Test
      |Test
      |""".stripMargin
  val c = a""" dsada ${2+2}"""
  val d = b""" dsada $a"""
  def execute (hiveContext: HiveContext) = {
    hiveContext.sql("""asd""")
  }
}