import org.specs2.mutable.Specification

import http.parser._

class HttpParserSpec extends Specification {
  "basic test" >> {
    val request = "GET / HTTP/1.0\u000d\u000a\u000d\u000a"

    val parsed = HttpParser.parseHttpRequest(request)

    parsed must beRight
  }
}
