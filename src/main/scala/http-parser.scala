package http.parser

import scala.util.parsing.combinator._
import scala.collection._

import java.net.URLDecoder._

object HttpParser {
  def parseHttpRequest(blob: String): Either[String, Map[String, String]] =
    HTTPRequestHeaderParser(blob)
}

object HTTPRequestHeaderParser extends RegexParsers {
  override val skipWhitespace = false
  
  def httpHeader = rep(CRLF) ~> requestLine ~ CRLF ~ headers <~ CRLF <~ """.*""".r ^^ {
    case (r ~ _ ~ h ) => Map("SCRIPT_NAME" -> "") ++ r ++ h
  }
  def requestLine = method ~ SP ~ requestTarget ~ SP ~ HTTPVersion ^^ {
    case (m ~ _ ~ r ~ _ ~ h) => m ++ r ++ h
  }
  def method = token ^^ { x => Map("REQUEST_METHOD" -> x) }

  def requestTarget = path ~ opt("?" ~> query) ^^ {
    case (p ~ q) => {
      val m = Map.newBuilder[String, String]
      m += "PATH_INFO" -> decode( p, "UTF-8" )

      q match {
        case Some(x) => {
          m += "REQUEST_URI" -> { p + "?" + x("QUERY_STRING") } 
          m += "QUERY_STRING" -> x("QUERY_STRING")
        }
        case None    => {
          m += "REQUEST_URI" -> p
          m += "QUERY_STRING" -> ""
        }
      }

      m.result
    }
  }

  def path = """[^?#\ ]+""".r
  def query = """[^#\ ]+""".r ^^ { x => Map("QUERY_STRING" -> x ) }
  def token = """[!#$%&'*+-.^_`|~0-9A-Za-z]+""".r
  def CRLF = opt(CR) ~ LF

  def HTTPVersion = """HTTP/1\.[01]""".r ^^ { x => Map( "SERVER_PROTOCOL" -> x ) }
  def headers     = rep(headerField <~ CRLF) ^^ { x =>
    x.foldLeft(Map[String, String]()) { (x, acc) => x ++ acc }
  }
  def headerField = fieldName ~ ":" ~ ows ~ fieldValue <~ ows ^^ {
    case (n ~ ":" ~ o ~ v) => {
      val fn = n.toString.replaceAll("-", "_").toUpperCase
      val name = if (fn != "CONTENT_LENGTH" && fn != "CONTENT_TYPE") "HTTP_" + fn else fn

      Map( name -> v.mkString(" ") )
    }
  }

  def fieldName = token
  def fieldValue = fieldContent ~ rep(obsFold ~> fieldContent) ^^ {
    case (fc ~ fcs) => List(fc) ++ fcs
  }

  def fieldContent = """.*""".r
  def obsFold = CRLF ~ rep1(SP | HTAB)

  def ows = """[ \t]*""".r
  def HTAB = "\u0009"

  def CR = "\u000d"
  def LF = "\u000a"
  def SP = "\u0020"

  def apply(requestHeader: String): Either[String, Map[String, String]] = {
    parseAll(httpHeader, requestHeader) match {
      case Success(parsedHeader, next)    => Right(parsedHeader)
      case NoSuccess(errorMessage, next)  =>
        Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
    }
  }
}
