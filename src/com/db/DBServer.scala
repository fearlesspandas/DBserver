package src.com.db
import java.security.spec.X509EncodedKeySpec
import java.security.KeyFactory
import java.security.PublicKey
import java.security.Security
import java.util.Base64

import javax.crypto.Cipher
import org.bouncycastle.jce.provider.BouncyCastleProvider
import zhttp.http.Http
import zhttp.http.Request
import zhttp.http.Response
import zhttp.http._
import zhttp.service.Server
import zio.App
import zio.ExitCode
import zio.URIO
import zio._
import zio.json._
import zio.console._
object DBServer extends App {

  type PubKey = String //Array[Byte]

  Security.addProvider(new BouncyCastleProvider())
  type SessionToken = String
  type EncryptedSessionToken = String
  val session_map: Map[PubKey, SessionToken] = Map()
  val player_map: Map[PubKey, Player] = Map()

  val app2: Http[Console, Throwable, Request, Response] = for{
    ref <- Http.collectZIO[Request]{case _ => Ref.make(session_map)}
    s <- Http.collectHttp[Request]{case _ =>addNewSessionToken(ref)}
  }yield s
  implicit def chunktobytearr(c: Chunk[Byte]): Array[Byte] = c.toArray

  def addNewSessionToken(
    m: Ref[Map[PubKey, SessionToken]]
  ): Http[Console, Throwable, Request, Response] = Http.collectZIO[Request] {
    case req =>
      req match {
        case Method.POST -> !! / "create_session" =>
          for {
            smap <- m.get
            pubkey <- req.getBodyAsString
            token = create_session_token(pubkey)
            curr_tokens = smap.getOrElse(pubkey, token._1)
            updatedsmap = smap.updated(pubkey, curr_tokens)
            _ <- m.set(updatedsmap)
            _ <- putStrLn( updatedsmap.toString())
          } yield {
            Response.text(token._2)
          }
      }

  }

  def create_session_token(pubKey: PubKey): (SessionToken, EncryptedSessionToken) = {
    val rsaKeyFactory = KeyFactory.getInstance("RSA")
    val bytes = Base64.getDecoder.decode(
      stripPublicKeyText(pubKey)
    )
    val rsaPublicKey: PublicKey = rsaKeyFactory.generatePublic(new X509EncodedKeySpec(bytes))
    val cipher = Cipher.getInstance("RSA")
    val public_key_object = rsaPublicKey
    cipher.init(Cipher.ENCRYPT_MODE, public_key_object)
    val session_token = generate_random_session_token()
    val encrypted_session_token_bytes =
      cipher.doFinal(Base64.getDecoder.decode(Base64.getEncoder.encode(session_token.getBytes())))
    val encodedMessage = Base64.getEncoder().encodeToString(encrypted_session_token_bytes)
    (session_token, encodedMessage)
  }

  def stripPublicKeyText(keyText: String): String = {
    val res = keyText.stripMargin
      .replace("\n", "")
      .replace("\\n", "")
      .replace("-----BEGIN PUBLIC KEY-----", "")
      .replace("-----END PUBLIC KEY-----", "")
      .replace("\"", "")
    res
  }

  def generate_random_session_token(): SessionToken =
    (0 to 16).foldLeft("") { (a, c) =>
      a + (scala.math.random() * 10).floor.toString
    }

  def stripCertText(certText: String): String =
    certText.stripMargin
      .replace("\n", "")
      .replace("-----BEGIN CERTIFICATE-----", "")
      .replace("-----END CERTIFICATE-----", "")

  def stripPrivateKeyText(keyText: String): String =
    keyText.stripMargin
      .replace("\n", "")
      .replace("-----BEGIN PRIVATE KEY-----", "")
      .replace("-----END PRIVATE KEY-----", "")

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    Server.start(8090, app2).exitCode

  case class Player(pubKey: PubKey)

  object Player {
    implicit val decoder: JsonDecoder[Player] = DeriveJsonDecoder.gen[Player]
    implicit val encoder: JsonEncoder[Player] = DeriveJsonEncoder.gen[Player]
    def apply(pukey: PubKey): Player = Player(pukey)
  }

}
