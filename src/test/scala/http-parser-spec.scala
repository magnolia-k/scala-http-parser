import org.specs2.mutable.Specification

import http.parser._

class HttpParserSpec extends Specification {
  "basic test" >> {
    val request = "GET / HTTP/1.0\u000d\u000aUser-Agent: a b c\u000d\u000a\u000d\u000a"
    val result = HttpParser.parseHttpRequest(request).get
    result match {
      case (l, env) => {
        l === 37

        env("PATH_INFO") === "/"
        env("QUERY_STRING") === ""
        env("REQUEST_METHOD") === "GET"
        env("SERVER_PROTOCOL") === "HTTP/1.0"
        env("REQUEST_URI") === "/"
        env("HTTP_USER_AGENT") === "a b c"
        env("SCRIPT_NAME") === ""
      }
    }
  }

  "short test" >> {
    val request = "GET / HTTP/1.0\u000d\u000a\u000d\u000a"
    val result = HttpParser.parseHttpRequest(request).get
    result match {
      case (l, env) => {
        l === 18

        env("PATH_INFO") === "/"
        env("QUERY_STRING") === ""
        env("REQUEST_METHOD") === "GET"
        env("SERVER_PROTOCOL") === "HTTP/1.0"
        env("REQUEST_URI") === "/"
        env("SCRIPT_NAME") === ""
      }
    }
  }

  "hello test" >> {
    val request = "GET / HTTP/1.0\u000d\u000a\u000d\u000aHello"
    val result = HttpParser.parseHttpRequest(request).get
    result match {
      case (l, env) => {
        l === 18

        env("PATH_INFO") === "/"
        env("QUERY_STRING") === ""
        env("REQUEST_METHOD") === "GET"
        env("SERVER_PROTOCOL") === "HTTP/1.0"
        env("REQUEST_URI") === "/"
        env("SCRIPT_NAME") === ""
      }
    }
  }

  "preheader test" >> {
    val request = "\u000d\u000aGET / HTTP/1.0\u000d\u000a\u000d\u000aHello"
    val result = HttpParser.parseHttpRequest(request).get
    result match {
      case (l, env) => {
        l === 20

        env("PATH_INFO") === "/"
        env("QUERY_STRING") === ""
        env("REQUEST_METHOD") === "GET"
        env("SERVER_PROTOCOL") === "HTTP/1.0"
        env("REQUEST_URI") === "/"
        env("SCRIPT_NAME") === ""
      }
    }
  }

  "decode test" >> {
    val request = "GET /foo%2A%2c?bar=3 HTTP/1.1\u000d\u000a\u000d\u000a"
    val result = HttpParser.parseHttpRequest(request).get
    result match {
      case (l, env) => {
        l === 33

        env("PATH_INFO") === "/foo*,"
        env("QUERY_STRING") === "bar=3"
        env("REQUEST_METHOD") === "GET"
        env("SERVER_PROTOCOL") === "HTTP/1.1"
        env("REQUEST_URI") === "/foo%2A%2c?bar=3"
        env("SCRIPT_NAME") === ""
      }
    }
  }

  "query test" >> {
    val request = "GET /foo?bar=3 HTTP/1.0\u000d\u000a\u000d\u000aHello"
    val result = HttpParser.parseHttpRequest(request).get
    result match {
      case (l, env) => {
        l === 27

        env("PATH_INFO") === "/foo"
        env("QUERY_STRING") === "bar=3"
        env("REQUEST_METHOD") === "GET"
        env("SERVER_PROTOCOL") === "HTTP/1.0"
        env("REQUEST_URI") === "/foo?bar=3"
        env("SCRIPT_NAME") === ""
      }
    }
  }

  "incomplete test" >> {
    val request = "GET / HTTP/1.0\u000d\u000a"
    val result = HttpParser.parseHttpRequest(request)

    result must beNone
  }

  "invalid test " >> {
    val request = "GET / HTTP/1.0\u000d\u000ahogehoge\u000d\u000a\u000d\u000a"
    HttpParser.parseHttpRequest(request) must throwA[HttpParsingException]
  }

  "CONTENT_TYPE test" >> {
    val request = "GET / HTTP/1.1\u000d\u000acontent-type: text/html\u000d\u000a\u000d\u000a"
    val result = HttpParser.parseHttpRequest(request).get
    result match {
      case (l, env) => {
        l === 43

        env("CONTENT_TYPE") === "text/html"
        env("PATH_INFO") === "/"
        env("QUERY_STRING") === ""
        env("REQUEST_METHOD") === "GET"
        env("SERVER_PROTOCOL") === "HTTP/1.1"
        env("REQUEST_URI") === "/"
        env("SCRIPT_NAME") === ""
      }
    }
  }
}
