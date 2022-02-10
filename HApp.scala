

import src.com.db.{Action, ActionResponse, Entity, Failure, Player, Success, UpdateUserDirection, Vector3}
import zhttp.http._
import zhttp.service.Server
import zio._
import zio.console._
import zio.internal.Platform
import zhttp.http.{Http, HttpApp, Request, Response}
import zhttp.service.Server
import zio.{App, ExitCode, URIO}
import zio.json._
import scala.collection.concurrent.{TrieMap => TMap}
/**
 * Example to build app on concrete entity
 */
object ConcreteEntity extends App {

  val state: TMap[Long,Entity] = TMap(0l -> Player(0,Vector3(0,0,0)))

  case class RawInput(value: String)

  val user: Http[Any, Nothing, Option[Action], ActionResponse] =
    Http.collect[Option[Action]] {
      case Some(a@UpdateUserDirection(k, v)) =>
        val maybeU = state.get(k)
        maybeU.map(p => {
          state.update(k,p.takeAction(a))
          Success(a)
        }).getOrElse(Failure(a))
    }
  val simpleResponse:HttpApp[Any,Nothing] =
    Http.collect[Request]{
      case Method.GET -> !! /"player"/playerid => {
        val res = state.get(playerid.toLong)
        Response.text(res.toJsonPretty)
      }
    }
  def app(id:Long,token:String): HttpApp[Any, Nothing] =
    user
      .contramap[Request](req => Action.fromString(token,id)) // Http[Any, Nothing, Request, UserCreated]
      .map(userUpdated => Response.text(userUpdated.action.toString))

  val app2 : HttpApp[Any, Nothing] = Http.collectHttp[Request] {
    case Method.POST -> !! / "action"/playerid/token => app(playerid.toLong,token)
    case Method.GET -> _ => simpleResponse
  }
  val appmain = app2
  // Run it like any simple app
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    Server.start(8090, appmain).exitCode
}