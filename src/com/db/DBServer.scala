package src.com.db
import java.io.{ByteArrayInputStream, InputStreamReader, Reader, StringReader}
import java.security.cert.X509Certificate
import java.security.{KeyFactory, PublicKey, Security}
import java.security.interfaces.RSAPublicKey
import java.security.spec.{RSAPublicKeySpec, X509EncodedKeySpec}
import java.util.Base64

import javax.crypto.Cipher
import org.bouncycastle.util.io.pem.{PemObject, PemObjectParser, PemReader}
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
import org.bouncycastle.jce.provider.BouncyCastleProvider
object DBServer extends App {

  val teszt = "-----BEGIN PUBLIC KEY-----\nMIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAyEgCivm8procay/H5do9\n1dyzry8SkiiIMSxWMhaaLKzkH1n6WAJcfKpXa3DcqIWLB/lxJQCexf3KE2NN05wA\nDRuRczRs5paG7N04qhhDsSMcgfg0iX+KIbmmkf7n5NvtMolqyTqrH+CgCFEo1wH0\nb6IkOpiOl++8zAFscb6oArZ9oTuSK367ct+xCaXAA4dVYS5pTVO6SIPExRGK809T\n/Q0b4XXqZx0X14j8PApAbPUFT4eqD+i9y1M/g5BvTWSX95hagPrw6URPnLJnW7V5\n1JLsIW20fQjagBub+KKeYLF/5ubhx51lrkSfPxgVzCsFxxLV/l+8iduHigvPse5Q\nLr81YCIipTfhy9gSmcoYLDU8e1czStItuNNqsuCUOY/ytgWE3J/UwSmCGVyThXwr\nNnemED7p00oSjoVv0WiuVQWmqmw69f6ytdIp34i2430ZZCE3Ivk7YyR59PbVfJPC\nwHFvfktsiLTNaEm8EoRq3ce6CTy7SsAP4opFpgjjXLKsoOdF8WELTzharM1jNpuE\ncCP0iDzqCPzPiN9g/hyXBnTVSoRpaud3TjlGUf/3rQp+UD0N7jIapiApyF9IdXDd\nN2X3ffMirTtHpNcKLGGWDAWCeYmXk3cmCakcwxUioCH60h91ayu7CU9JnmVvq34A\nERxXvnHsZ4FA5HoGY2447d0CAwEAAQ==\n-----END PUBLIC KEY-----"



  Security.addProvider(new BouncyCastleProvider())
  type PubKey = String//Array[Byte]
  type SessionToken = String
  type EncryptedSessionToken = String
  val session_map: Map[PubKey, Set[SessionToken]] = Map()
  val player_map : Map[PubKey,Player] = Map()

  case class Player(pubKey: PubKey)
  object Player{
    implicit val decoder:JsonDecoder[Player] = DeriveJsonDecoder.gen[Player]
    implicit val encoder:JsonEncoder[Player] = DeriveJsonEncoder.gen[Player]
    def apply(pukey:PubKey):Player = Player(pukey)
  }

  implicit def chunktobytearr(c:Chunk[Byte]):Array[Byte] = c.toArray
  def addNewSessionToken(
    m: Ref[Map[PubKey, Set[SessionToken]]]
  ): Http[Any, Throwable, Request, Response] = Http.collectZIO[Request] {
    case req => req match {
      case Method.POST -> !! /"create_session" =>
        for {
          smap <- m.get
          pubkey <- req.getBodyAsString
          token = create_session_token(pubkey)
          curr_tokens = smap.getOrElse(pubkey, Set())
          _ <- m.set(smap.updated(pubkey,  curr_tokens + token._1 ))
        } yield {
          Response.text(token._2)
        }
    }


  }


  def stripCertText(certText: String): String =
    certText
      .stripMargin
      .replace("\n", "")
      .replace("-----BEGIN CERTIFICATE-----", "")
      .replace("-----END CERTIFICATE-----", "")

  def stripPrivateKeyText(keyText: String): String =
    keyText
      .stripMargin
      .replace("\n", "")
      .replace("-----BEGIN PRIVATE KEY-----", "")
      .replace("-----END PRIVATE KEY-----", "")

  def stripPublicKeyText(keyText: String): String = {
    val res = keyText
      .stripMargin

      .replace("\n", "")
      .replace("\\n", "")
      .replace("-----BEGIN PUBLIC KEY-----", "")
      .replace("-----END PUBLIC KEY-----", "")
      .replace("\"","")
    res
  }

  import org.bouncycastle.asn1._;
  import org.bouncycastle.asn1.x509.RSAPublicKeyStructure;
  import java.security.cert.CertificateFactory
  import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo
  import org.bouncycastle.openssl.PEMParser
  import org.bouncycastle.openssl.jcajce.JcaPEMKeyConverter
  import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo
  import org.bouncycastle.openssl.jcajce.JcaPEMKeyConverter
  def create_session_token(pubKey: PubKey): (SessionToken, EncryptedSessionToken) = {

    
    val rsaKeyFactory = KeyFactory.getInstance("RSA")
    val strippedKeyText =stripPublicKeyText(pubKey)
    val strippedTest = stripPublicKeyText(teszt)
    val eq = strippedKeyText == strippedTest
    print("stripped",strippedKeyText)
    val bytes = Base64.getDecoder.decode(
      stripPublicKeyText(pubKey)
    )

    val rsaPublicKey: PublicKey = rsaKeyFactory.generatePublic(new X509EncodedKeySpec(bytes))
//    val t =  key_factory.generatePublic(spec)// key_factory.generatePublic(pubKeySpec);


    val cipher = Cipher.getInstance("RSA")

    val public_key_object = rsaPublicKey
    cipher.init(Cipher.ENCRYPT_MODE, public_key_object)
    val session_token = generate_random_session_token()
    val encrypted_session_token_bytes = cipher.doFinal(session_token.getBytes)
    val encodedMessage = Base64.getEncoder().encodeToString(encrypted_session_token_bytes)
    (session_token, encodedMessage)
  }

  def generate_random_session_token(): SessionToken =
    (0 to 16).foldLeft("") { (a, c) =>
      a + (scala.math.random() * 10).floor.toString
    }

//  def get_players(m: Ref[Map[PubKey, Player]]): Http[Any, Nothing, Request, Response] =
//    Http.collectZIO[Request] {
//      case Method.GET -> !! / "player" / pubkey =>
//        for {
//          smap <- m.get
//          player = smap.getOrElse(pubkey.toArray, Player(pubkey.toArray))
//        } yield {
//          Response.json(player.toJson)
//        }
//    }


  val test_post_connection :  Http[Any, Throwable, Request, Response] =
    Http.collectZIO[Request] {
      case req => req match {
        case Method.GET -> !! / "create_session" =>
          for{
            reqtext <- req.getBodyAsString
          }yield Response.text(reqtext)
      }
    }



  val app: Http[Any, Throwable, Request, Response] = Http.collectHttp[Request] {
    case req =>

      for {
        smap <- Http.collectZIO[Request] { case _ => Ref.make(session_map) }
        pmap <- Http.collectZIO[Request] { case _ => Ref.make(player_map) }
        addSessionApp <- addNewSessionToken(smap)
//        getPlayersapp <- get_players(pmap)
      } yield req match {
        case Method.POST -> !! / "create_session" =>
          println("woot")
          addSessionApp
//        case Method.GET -> !! =>
//          println("notwoot")
//          getPlayersapp
      }

  }
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    Server.start(8090, app).exitCode



}
