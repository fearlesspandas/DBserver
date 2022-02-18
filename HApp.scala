

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

  val state: TMap[String,Entity] = TMap()
  case class RawInput(value: String)
  case class player_data(pub_key:String)
  def update_state_map(key_map:Ref[Map[String,player_data]]): Http[Any,Nothing,player_data,Unit] =
    Http.collectZIO[player_data] { case p =>
      for {
        m <- key_map.get
        _ <- key_map.set(m.updated(p.pub_key,p))
      } yield {}
    }

  val userActionHandler: Http[Any, Nothing, Option[Action], ActionResponse] =
    Http.collect[Option[Action]] {
      case Some(a@UpdateUserDirection(k, v)) =>
        val maybeU = state.get(k)
        maybeU.map(p => {
          state.update(k,p.takeAction(a))
          Success(a)
        }).getOrElse(Failure(a))
    }
  val userMovementHandler:Http[Any,Nothing,(String,Vector3,Vector3),Unit] =
    Http.collect[(String,Vector3,Vector3)]{
      case (id,loc,rot) =>
         val current_state = state.getOrElse(id,Player(id))
         state.update(id,current_state.move_to(loc).rotate_to(rot))
    }
  val simpleResponse:HttpApp[Any,Nothing] =
    Http.collect[Request]{
      case Method.GET -> !! /"player"/playerid => {
        val res = state.get(playerid)
        Response.text(res.toJsonPretty)
      }
    }
  def app(id:String,token:String): HttpApp[Any, Nothing] =
    userActionHandler
      .contramap[Request](req => Action.fromString(token,id)) // Http[Any, Nothing, Request, UserCreated]
      .map(userUpdated => Response.text(userUpdated.action.toString))

  val app2 : HttpApp[Any, Nothing] = Http.collectHttp[Request] {
    case Method.POST -> !! / "action"/playerid/token => app(playerid,token)
    case Method.GET -> _ => simpleResponse
  }
  val appmain = app2
  // Run it like any simple app
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    Server.start(8090, appmain).exitCode
}