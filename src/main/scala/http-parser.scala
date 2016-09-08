package http.parser

import scala.util.parsing.combinator._
import scala.collection._

import java.net.URLDecoder._

// Some -> 解析完了
// None -> まだ完了していない
// Throw Exception -> 誤ったヘッダ

case class HttpParsingException(e: String) extends Throwable

object HttpParser {
  def parseHttpRequest(blob: String): Option[(Int, Map[String, String])] = {
    val env = HTTPRequestHeaderParser(blob)

    env match {
      case Right(e) => {
        val m = """(?s)^(?:\u000d\u000a|\u000a)?.*(?:\u000d\u000a|\u000a){2}""".r.findFirstIn(blob)
        val l = m.get.length

        Some( (l, e) )
      }
      case Left(_)  => {
        // pre-header blank lines are allowed (RFC 2616 4.1)
        val h = blob.replaceAll("^(?:\u000d\u000a|\u000a)+", "")

        if ( """(?:\u000d\u000a|\u000a)(?:\u000d\u000a|\u000a)""".r.findFirstIn(h).nonEmpty )
          throw new HttpParsingException("Invalid HTTP header") // invalid header
        else
          None                                                  // request incomplete
      }
    }
  }
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

    case (p ~ Some(q)) => {
      val m = Map.newBuilder[String, String]
      m += "PATH_INFO" -> decode( p, "UTF-8" )
      m += "REQUEST_URI" -> { p + "?" + q("QUERY_STRING") } 
      m += "QUERY_STRING" -> q("QUERY_STRING")

      m.result
    }

    case (p ~ None) => {
      val m = Map.newBuilder[String, String]
      m += "PATH_INFO" -> decode( p, "UTF-8" )
      m += "REQUEST_URI" -> p
      m += "QUERY_STRING" -> ""

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
