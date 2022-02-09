

import zhttp.http._
import zhttp.service.Server
import zio._
import zio.console._

import zio.internal.Platform

trait datastore

object HelloWorld extends App {
  val myRuntime: Runtime[Int] = Runtime(42, Platform.default)
  // Create HTTP route

  val app= Http.collectZIO[Request] {
    case Method.GET -> !! / "text" =>
        val t : ZIO[Has[datastore],Nothing,Has[datastore] with Has[Response]] = {
          for{
            x <-
          }
        }
//      Response.text("Hello World!")
    case Method.GET -> !! / "json" => ??? //Response.json("""{"greetings": "Hello World!"}""")
  }

  // Run it like any simple app
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
//    Server.start(8090, app).exitCode
  ???
  }

}

