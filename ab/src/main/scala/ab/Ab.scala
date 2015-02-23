package ab

import model.{Experiment, Username, Groups}

trait Service {
  import org.http4s.server._
  import org.http4s.dsl._

  def service = HttpService {
    case req @ GET -> path => 
      Ok(s"hello")
  }

  def service(groups: Groups) = {
    val partition = PartitionFunction.from(groups, HashFunction.md5)

    HttpService {
      case req @ GET -> path => 
        val params = req.params
        params.get("id") match {
          case None =>
            BadRequest("Username must be specified")

          case Some(id) =>
            Username(id).fold({ failures =>
              BadRequest("Failed to parse id: " + failures.list.mkString(",") + ".")
            }, { username =>
              val response = partition.partition(Experiment(path.toString, username)).name
              Ok(response)
            })
        }
      }
    }
}

import scalaz._
import scalaz.concurrent.Task

object Ab extends Service {

  def errorProgram(errors: NonEmptyList[Groups.AquisitionError]): Task[Unit] = Task {
    println(s"Cannot start service. Reason(s): ${errors.list.mkString}.")
  }

  import org.http4s.server.blaze.BlazeBuilder
  def serviceProgram(groups: Groups): Task[Unit] = Task {
    BlazeBuilder.bindHttp(8080)
      .mountService(service(groups), "/")
      .run
      .awaitShutdown
  }

  def program: Task[Unit] = {
    Groups.fromResource("/groups.list").flatMap { validation =>
      validation.fold({ errors =>
        errorProgram(errors)
      }, { groups =>
        serviceProgram(groups)
      })
    }
  }

  def main(args: Array[String]): Unit = {
    program.run
  }
}
