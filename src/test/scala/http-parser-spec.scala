import org.specs2.mutable.Specification

import http.parser._

class HttpParserSpec extends Specification {
  "basic test" >> {
    val request = "GET / HTTP/1.0\u000d\u000aUser-Agent: a b c\u000d\u000a\u000d\u000a"
    val env = HttpParser.parseHttpRequest(request).right.get

    env("PATH_INFO") === "/"
    env("QUERY_STRING") === ""
    env("REQUEST_METHOD") === "GET"
    env("SERVER_PROTOCOL") === "HTTP/1.0"
    env("REQUEST_URI") === "/"
    env("HTTP_USER_AGENT") === "a b c"
    env("SCRIPT_NAME") === ""
  }

  "short test" >> {
    val request = "GET / HTTP/1.0\u000d\u000a\u000d\u000a"
    val env = HttpParser.parseHttpRequest(request).right.get

    env("PATH_INFO") === "/"
    env("QUERY_STRING") === ""
    env("REQUEST_METHOD") === "GET"
    env("SERVER_PROTOCOL") === "HTTP/1.0"
    env("REQUEST_URI") === "/"
    env("SCRIPT_NAME") === ""
  }

  "hello test" >> {
    val request = "GET / HTTP/1.0\u000d\u000a\u000d\u000aHello"
    val env = HttpParser.parseHttpRequest(request).right.get

    env("PATH_INFO") === "/"
    env("QUERY_STRING") === ""
    env("REQUEST_METHOD") === "GET"
    env("SERVER_PROTOCOL") === "HTTP/1.0"
    env("REQUEST_URI") === "/"
    env("SCRIPT_NAME") === ""
  }

  "preheader test" >> {
    val request = "\u000d\u000aGET / HTTP/1.0\u000d\u000a\u000d\u000aHello"
    val env = HttpParser.parseHttpRequest(request).right.get

    env("PATH_INFO") === "/"
    env("QUERY_STRING") === ""
    env("REQUEST_METHOD") === "GET"
    env("SERVER_PROTOCOL") === "HTTP/1.0"
    env("REQUEST_URI") === "/"
    env("SCRIPT_NAME") === ""
  }

  "decode test" >> {
    val request = "GET /foo%2A%2c?bar=3 HTTP/1.1\u000d\u000a\u000d\u000a"
    val env = HttpParser.parseHttpRequest(request).right.get

    env("PATH_INFO") === "/foo*,"
    env("QUERY_STRING") === "bar=3"
    env("REQUEST_METHOD") === "GET"
    env("SERVER_PROTOCOL") === "HTTP/1.1"
    env("REQUEST_URI") === "/foo%2A%2c?bar=3"
    env("SCRIPT_NAME") === ""
  }

  "query test" >> {
    val request = "GET /foo?bar=3 HTTP/1.0\u000d\u000a\u000d\u000aHello"
    val env = HttpParser.parseHttpRequest(request).right.get

    env("PATH_INFO") === "/foo"
    env("QUERY_STRING") === "bar=3"
    env("REQUEST_METHOD") === "GET"
    env("SERVER_PROTOCOL") === "HTTP/1.0"
    env("REQUEST_URI") === "/foo?bar=3"
    env("SCRIPT_NAME") === ""
  }

  "invalid test" >> {
    val request = "GET /foo?bar=3 HTTP/1.0\u000d\u000a"
    val result = HttpParser.parseHttpRequest(request).left.get

    result must be contain("expected but end of source found on line")
  }
}
