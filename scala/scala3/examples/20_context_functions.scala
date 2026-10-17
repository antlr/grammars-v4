trait Config {
  def host: String
  def port: Int
}

type Configured[A] = Config ?=> A

def getHost: Configured[String] = summon[Config].host
def getPort: Configured[Int] = summon[Config].port

def connect: Configured[String] = {
  val h = getHost
  val p = getPort
  s"Connected to $h:$p"
}

object ContextFunctionDemo {
  def runWithConfig[A](h: String, p: Int)(f: Configured[A]): A = {
    given Config with {
      def host: String = h
      def port: Int = p
    }
    f
  }

  def main(args: Array[String]): Unit = {
    val result = runWithConfig("localhost", 8080) {
      connect
    }
    println(result)
  }
}
