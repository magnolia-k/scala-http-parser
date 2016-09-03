package http.parser

import scala.util.parsing.combinator._
import scala.collection._

import java.net.URLDecoder._

object HttpParser {
  def parseHttpRequest(blob: String): Either[String, Map[String, String]] = {
    HTTPRequestHeaderParser(blob)
  }
}

object HTTPRequestHeaderParser extends RegexParsers {
  override val skipWhitespace = false
  val env = mutable.Map[String, String]()
  
  def header = rep(CRLF) ~ requestLine ~ CRLF ~ rep(headerField ~ CRLF) ~ rep(headerField ~ CRLF) ~ CRLF ~ """.*""".r

  def requestLine = method ~ SP ~ requestTarget ~ SP ~ HTTPVersion
  def method = token ^^ { method => env += "REQUEST_METHOD" -> method.toString }
  def requestTarget = path ~ opt("?" ~ query) ^^ { request =>
    env += "REQUEST_URI" -> request._1.toString
    request._2 match {
      case Some(q) => env += "QUERY_STRING" -> q.toString
      case None    => env += "QUERY_STRING" -> ""
    }
  }
  def path = """[^?#\ ]+""".r ^^ { r => env += "PATH_INFO" -> decode(r.toString, "UTF-8") }
  def query = """[^#\ ]+""".r
  def token = """[!#$%&'*+-.^_`|~0-9A-Za-z]+""".r
  def CRLF = opt(CR) ~ LF

  def HTTPVersion = """HTTP/1\.[01]""".r ^^ { httpversion => env += "SERVER_PROTOCOL" -> httpversion.toString }

  def headerField = fieldName ~ ":" ~ ows ~ fieldValue ~ ows ^^ { result => 
    val name = { 
      val f = result._1.toString.replaceAll("-", "_").toUpperCase
      if (f != "CONTENT_LENGTH" || f != "CONTENT_TYPE") "HTTP_" + f else f
    }

    env += name -> result._2.mkString(" ")
  }
  def fieldName = token
  def fieldValue = fieldContent ~ rep(obsFold ~> fieldContent) ^^ { result =>
    List(result._1) ++ result._2
  }

  def fieldContent = """.*""".r
  def obsFold = CRLF ~ rep1(SP | HTAB)

  def ows = """[ \t]*""".r
  def HTAB = "\u0009"

  def CR = "\u000d"
  def LF = "\u000a"
  def SP = "\u0020"

  def apply(requestHeader: String): Either[String, Map[String, String]] = parseAll(header, requestHeader) match {
    case Success(parsedHeader, next)    => Right(env.toMap)
    case NoSuccess(errorMessage, next)  =>
      Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }
}
