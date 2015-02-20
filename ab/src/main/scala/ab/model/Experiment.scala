package ab.model

case class Experiment(
  path: String,
  username: Username)

object Experiment {
  import scodec._
  import scodec.codecs._

  implicit val encoder: Encoder[Experiment] = new Encoder[Experiment] {
    override val sizeBound = SizeBound.unknown

    override def encode(e: Experiment) = for {
      path     <- utf8.encode(e.path)
      username <- Encoder.encode(e.username)
    } yield path ++ username
  }
}
