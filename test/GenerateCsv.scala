object GenerateCsv {
  val SampleSize = 10 * 1000

  val rand = new scala.util.Random

  def chooseint(min: Int, max: Int): Int = {
    val size = max - min
   rand.nextInt(size + 1) + min
  }

  def oneof[A](xs: Seq[A]): A = {
    xs.apply(rand.nextInt(xs.size))
  }

  def randPath: String = {
    val set = 
      ('a' to 'z') ++
      ('A' to 'Z') ++
      ('0' to '9') ++
      List('-', '.', '_', '~', '!', '$')

    val size = chooseint(0, 20)
    val cs   = (0 until size) map { _ => oneof(set) }
    cs.mkString
  }

  def randId: String = {
    val set = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')

    val size = chooseint(1, 12)
    val cs   = (0 until size) map { _ => oneof(set) }
    cs.mkString
  }

  def generateCorrectData(fileName: String): Unit = {
    val string = (for (i <- 0 until SampleSize) yield {
      randPath + "," + randId
    }).mkString("\n")
    
    stringToFile(string, fileName)
  }

  
  def stringToFile(string: String, fileName: String): Unit = { 
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets

    Files.write(Paths.get(fileName), string.getBytes(StandardCharsets.UTF_8))
  }

  def main(args: Array[String]): Unit = {
    generateCorrectData("correct.csv")
  }
}
