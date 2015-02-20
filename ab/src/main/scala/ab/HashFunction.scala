package ab

import model.Experiment

trait HashFunction {
  def hashFor(experiment: Experiment): Long
}
object HashFunction {
  def apply(f: Experiment => Long): HashFunction = new HashFunction {
    override def hashFor(experiment: Experiment): Long = f(experiment)
  }

  /*
   * See [www.exp-platform.com/documents/guidecontrolledexperiments.pdf] for the
   * details why md5 was chosen.
   */
  val md5 = HashFunction { experiment =>
    import scodec._
    import scodec.bits._

    // XXX: Encoding `Experiment` cannot fail therefore returning 0 bit is safe.
    val bits = Encoder.encode(experiment).getOrElse(bin"0")
    val digest = bits.digest("md5")
    val bigInteger = BigInt(digest.toBin, 2)
    (bigInteger % Long.MaxValue).toLong
  }
}
